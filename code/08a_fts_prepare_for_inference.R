# 08a_fts_prepare_for_inference.R
# PART 1 of 2 — run BEFORE the Python ML inference step.
#
# 1. Loads curated FTS flows and joins project CVA metadata.
# 2. Classifies each flow by sector / method / cluster and project percentage.
# 3. Writes the subset that needs ML inference to classifier_code/fts_to_inference.csv.
# 4. Writes the fully-flagged dataset (pre-ML) to output/fts_output_pre_ml.csv.
#
# After this script, run:
# cd classifier_code && source venv/bin/activate && python3 flow_inference.py && cd ..
# Then run 08b_fts_combine_inference.R.
#
# Run from the project root:
# Rscript code/08a_fts_prepare_for_inference.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

years <- 2018:2025
if (!dir.exists("output"))
  dir.create("output")

# Load curated FTS flows
fts <- rbindlist(lapply(years, function(yr)
  fread(paste0(
    "fts/fts_curated_", yr, ".csv"
  ))),
  use.names = T,
  fill = T)
fts <- fts[as.integer(year) >= 2018L]

# Join project CVA metadata
proj_meta <- fread("projects/cash_projects.csv")
proj_text <- fread("projects/project_text.csv")
proj_data <- merge(proj_text, proj_meta[, project_id := as.character(project_id)], by = "project_id", all = T)
setnames(
  proj_data,
  c("project_id", "text", "cva_percentage", "cva"),
  c(
    "destinationObjects_Project.id",
    "project_text",
    "project_cva_percentage",
    "project_cva"
  )
)

fts[, destinationObjects_Project.id := as.character(destinationObjects_Project.id)]
proj_data[, destinationObjects_Project.id :=
            as.character(destinationObjects_Project.id)]

fts <- merge(fts,
             proj_data,
             by = "destinationObjects_Project.id",
             all.x = T,
             sort = F)

# Combine FTS description with project text for richer ML input
fts[, all_text := fcase(
  !is.na(description) & !is.na(project_text), paste(trimws(description), trimws(project_text)),
  !is.na(description), trimws(description),
  !is.na(project_text), trimws(project_text),
  default = NA_character_
)]

# CVA cash clusters list
CASH_CLUSTERS <- c(
  "Basic Needs / Multi-Purpose Cash",
  "Cash à usage multiple",
  "Multi Purpose Cash",
  "Multi-cluster/Multi-Purpose Cash",
  "Multi-Purpose Cash & Social Protection",
  "Multipurpose Cash Assistance (MPC)",
  "Multi-Purpose Cash Assistance (MPCA)",
  "Multipurpose cash/ IDPs/ multisector",
  "Multi-sector Cash/Social Protection COVID-19",
  "Cash",
  "Multi-purpose Cash",
  "Multipurpose cash assistance",
  "Multi-Purpose Cash Assistance",
  "Multipurpose Cash Assistance COVID-19",
  "Multi-Purpose Cash Assistance COVID-19",
  "Multi-purpose Cash COVID-19",
  "Multipurpose cash",
  "Protection: Multi-Purpose Cash Assistance",
  "Cash Transfer COVID-19"
)
cash_cluster_regex <- paste(paste0(
  "(^|;\\s*)",
  gsub("([][(){}+*?|^$\\\\.])", "\\\\\\1", CASH_CLUSTERS),
  "(\\s*;|$)"
), collapse = "|")

# Step 1: classify by sector / method / cluster
# Bug fix: apply CTP LAST so it cannot be downgraded by the multi-cluster rule.
fts[, sector_method_cluster_relevance := "None"]

# Single cash cluster → Full
fts[destinationObjects_Cluster.name %in% CASH_CLUSTERS, sector_method_cluster_relevance := "Full"]

# Multiple clusters including a cash cluster → Partial
fts[grepl(";", destinationObjects_Cluster.name, fixed = T) &
      grepl(cash_cluster_regex, destinationObjects_Cluster.name), sector_method_cluster_relevance := "Partial"]

# CTP method → always Full, overriding the multi-cluster Partial rule above
fts[method == "Cash transfer programming (CTP)", sector_method_cluster_relevance := "Full"]

# Step 2: derive per-cluster count for Partial flows
fts[, destinationClusterCount :=
      fifelse(
        is.na(destinationObjects_Cluster.name) |
          destinationObjects_Cluster.name == "",
        0L,
        1L + nchar(destinationObjects_Cluster.name) -
          nchar(gsub(
            ";", "", destinationObjects_Cluster.name, fixed = T
          ))
      )]

# Step 3: initial relevance = sector/cluster, then project % may override
# Bug fix: only apply project-percentage overrides to rows NOT already
# classified by sector/cluster, preventing the relevance label from diverging
# from the actual amount calculation.
fts[, relevance := sector_method_cluster_relevance]
fts[, relevance_method := "Sector/Method/Cluster"]

fts[sector_method_cluster_relevance == "None" &
      !is.na(project_cva_percentage) &
      project_cva_percentage >= 0.75, `:=`(relevance = "Full", relevance_method = "Project CVA Percentage")]

fts[sector_method_cluster_relevance == "None" &
      !is.na(project_cva_percentage) &
      project_cva_percentage > 0 &
      project_cva_percentage < 0.75, `:=`(relevance = "Partial", relevance_method = "Project CVA Percentage")]

# Step 4: keyword flag for ML candidate selection
# Non-case-sensitive keywords (matched case-insensitively)
KW_NONCASE <- c(
  "cash",
  "voucher",
  "vouchers",
  "cash transfer",
  "cash grant",
  "unconditional cash",
  "money",
  "conditional cash transfer",
  "argent",
  "monetaires",
  "bons",
  "espèces",
  "monnaie",
  "monétaires",
  "monétaire",
  "tokens",
  "coupons",
  "cupones",
  "public works programme",
  "social assistance",
  "social safety net",
  "social transfer",
  "social protection"
)
# Acronyms: match case-insensitively too (they appear in many casings in text)
KW_ACRONYMS <- c("CCT",
                 "UCT",
                 "CTP",
                 "CFW",
                 "CFA",
                 "SSN",
                 "ESSN",
                 "MPC",
                 "MPCT",
                 "CVA")
kw_regex <- paste0("\\b(", paste(c(KW_NONCASE, KW_ACRONYMS), collapse = "|"), ")\\b")
fts[, keyword_match := grepl(kw_regex, all_text, ignore.case = T)]

# Write ML input: flows needing inference
# Candidates: keyword hit or project-flagged as CVA, but still "None" relevance
to_infer <- unique(fts[(keyword_match |
                          !is.na(project_cva) & project_cva) &
                         relevance == "None", .(id, text = all_text)])

fwrite(to_infer, "classifier_code/fts_to_inference.csv")
message(nrow(to_infer),
        " flows written to classifier_code/fts_to_inference.csv")

# Save pre-ML flagged dataset
fwrite(fts, "output/fts_output_pre_ml.csv")
message("Pre-ML dataset written to output/fts_output_pre_ml.csv")
message("\nNext step: run the Python ML classifier, then 08b_fts_combine_inference.R")
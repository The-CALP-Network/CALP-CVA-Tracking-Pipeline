# 07_process_project_data.R
# Processes the HPC project Q&A data to produce per-project CVA flags and
# planned budget percentages, which are later joined to FTS flows.
#
# Outputs:
# output/questions.csv — all unique question strings (for review)
# output/potential_new_cash_questions.csv — newly detected CVA questions
# projects/cash_projects.csv — per-project cva_percentage & cva flag
# projects/project_text.csv — project name + objective text
#
# Run from the project root:
# Rscript code/07_process_project_data.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

years <- 2018:2025
if (!dir.exists("output"))
  dir.create("output")

# Load and combine all project years
project_list <- lapply(years, function(yr) {
  load(paste0("projects/project_data_", yr, ".RData")) # loads all_projects
  all_projects
})
all_projects <- rbindlist(project_list, fill = T, use.names = T)
rm(project_list)
gc()

# Identify questions potentially related to CVA
all_questions <- unique(all_projects$question)
fwrite(data.table(question = all_questions), "output/questions.csv")

cash_kw <- paste0("\\b(", paste(
  c(
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
    "social protection",
    # acronyms — case-sensitive match below
    "CVA",
    "CCT",
    "UCT",
    "CTP",
    "CFW",
    "CFA",
    "SSN",
    "ESSN",
    "MPC",
    "MPCT"
  ),
  collapse = "|"
), ")\\b")

potential_cash_qs <- all_questions[grepl(cash_kw, all_questions, ignore.case = T)]

labeled_qs <- fread("reference_datasets/cva_project_questions.csv", encoding = "UTF-8")
new_qs <- setdiff(potential_cash_qs, labeled_qs$Question)
if (length(new_qs)) {
  fwrite(data.table(question = new_qs),
         "output/potential_new_cash_questions.csv")
  message(
    length(new_qs),
    " potentially new CVA question(s) written to ",
    "output/potential_new_cash_questions.csv — please review and label."
  )
}

# Separate quantitative (percentage) and boolean (yes/no) questions
quant_qs <- labeled_qs[`Question type` %in% c("quantC", "quantV"), Question]
bool_qs <- labeled_qs[`Question type` == "flagCVA", Question]

quant_rows <- all_projects[question %in% quant_qs]
bool_rows <- all_projects[question %in% bool_qs]

# Standardise percentage answers
standardize_pct <- function(x) {
  x <- trimws(tolower(x))
  num <- NA_real_
  
  if (grepl("%", x, fixed = T)) {
    # "40%", "40 %", "about 40%", etc.
    m <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)?(?=\\s*%)", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
    
  } else if (grepl("percent", x, fixed = T)) {
    m <- regmatches(x,
                    regexpr("[0-9]+(?:\\.[0-9]+)?(?=\\s*percent)", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
    
  } else if (grepl("^[0-9]+(?:\\.[0-9]+)?$", x, perl = T)) {
    num <- as.numeric(x)
    
  } else if (grepl("less than 1", x, fixed = T)) {
    num <- 0
    
  } else {
    # Last resort: extract first numeric token
    m <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)?", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
  }
  
  num
}

# Only keep answers that contain at least one numeric token
quant_rows <- quant_rows[grepl("[0-9]", answer)]
quant_rows[, pct := vapply(answer, standardize_pct, numeric(1))]

# Sum cash + voucher percentages per project; cap at 100
# NOTE: summing quantC and quantV can exceed 100 if both refer to overlapping
# portions of the budget. The cap prevents obvious over-counting but the
# underlying ambiguity remains; see GUIDE.md for discussion.
quant_by_proj <- quant_rows[!is.na(pct), .(cva_percentage = min(sum(pct), 100) / 100), by = project_id]

# Standardise boolean answers
TRUE_VALUES <- c("true", "oui", "yes")
bool_rows[, bool_val := tolower(answer) %in% TRUE_VALUES]
bool_by_proj <- bool_rows[, .(cva = any(bool_val)), by = project_id]

# Reconcile overlaps between the two sources
# 0% quantitative: treat as cva = FALSE in the boolean table (if not already)
zero_pct <- quant_by_proj[cva_percentage == 0, .(project_id, cva = F)]
zero_pct <- zero_pct[!project_id %in% bool_by_proj$project_id]
bool_by_proj <- rbindlist(list(bool_by_proj, zero_pct))

# F boolean: add 0% row to quantitative table (if not already)
false_bool <- bool_by_proj[cva == F, .(project_id, cva_percentage = 0)]
false_bool <- false_bool[!project_id %in% quant_by_proj$project_id]
quant_by_proj <- rbindlist(list(quant_by_proj, false_bool))

# Combine and make the two columns consistent
cash_projects <- merge(quant_by_proj, bool_by_proj, by = "project_id", all = T)
cash_projects[cva_percentage > 0, cva := T]
cash_projects[cva_percentage == 0, cva := F]

fwrite(cash_projects, "projects/cash_projects.csv")

# Project text for ML input
project_text <- unique(all_projects[, .(project_id, text = paste(project_name, project_objective))])
fwrite(project_text, "projects/project_text.csv")

message("Done. Outputs written to projects/ and output/.")
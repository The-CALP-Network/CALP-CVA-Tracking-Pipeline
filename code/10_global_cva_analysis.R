# 10_global_cva_analysis.R
# Aggregates CVA amounts from FTS flows and survey data, removes sub-grant
# double-counting via multi-strategy name matching, and produces summary
# tables by organisation and org type.
#
# Inputs:
# output/fts_cva.csv
# reference_datasets/cva_survey_data.xlsx (sheets: Survey_data, 2, 3)
# reference_datasets/fts_survey_overlap.csv
# reference_datasets/cva_org_type.csv
#
# Outputs:
# output/cva_agg.csv — CVA by organisation (de-doubled)
# output/cva_agg_org_type.csv — CVA by org type × year (de-doubled)
#
# Run from the project root:
# Rscript code/10_global_cva_analysis.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table", "openxlsx", "stringdist")

if (!dir.exists("output"))
  dir.create("output")

# Load FTS CVA flows
fts_cva <- fread("output/fts_cva.csv")
queue <- fread("output/cva_to_manually_classify.csv")

manual_classified_file <- "output/cva_manually_classified.csv"
if (file.exists(manual_classified_file)) {
  manual <- fread(manual_classified_file)
  
  # Validate schema
  required_cols <- c("id", "CVAamount", "CVAamount_type")
  missing_cols <- setdiff(required_cols, names(manual))
  if (length(missing_cols))
    stop(
      "cva_manually_classified.csv is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  
  # Validate amounts
  bad_amounts <- manual[is.na(CVAamount) |
                          !is.finite(CVAamount) | CVAamount <= 0]
  if (nrow(bad_amounts))
    stop(
      nrow(bad_amounts),
      " row(s) in cva_manually_classified.csv have ",
      "missing or zero CVAamount. IDs: ",
      paste(bad_amounts$id, collapse = ", ")
    )
  
  # Warn about IDs not traceable to the current queue
  stale <- manual[!id %in% queue$id, id]
  if (length(stale))
    warning(
      length(stale),
      " manually classified ID(s) not found in current ",
      "cva_to_manually_classify.csv — these may be stale:\n ",
      paste(stale, collapse = ", ")
    )
  
  message(nrow(manual), " manually classified flow(s) incorporated.")
  
  # Remove verified rows from the queue so it only contains pending decisions
  queue_remaining <- queue[!id %in% manual$id]
  fwrite(queue_remaining, "output/cva_to_manually_classify.csv")
  
  message(
    nrow(queue) - nrow(queue_remaining),
    " row(s) removed from queue. ",
    nrow(queue_remaining),
    " still pending."
  )
  
  fts_cva <- rbindlist(list(fts_cva, manual),
                       fill = TRUE,
                       use.names = TRUE)
} else {
  warning(nrow(queue), " manual checks still pending.")
}

fts_cva <- fts_cva[destinationObjects_Organization.name !=
                     "International NGOs (Confidential)"]

# Load survey data (three sheets)
survey_data <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = "Survey_data"))
sub_grants <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = 2))
pc_tv_estimate <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = 3))
setnames(pc_tv_estimate, "CVA.data.year", "Year")

survey_data[, Organisation := trimws(Organisation.Reference)]
survey_data[, PC.USD.m := as.numeric(PC.USD.m)]
survey_data[, TV.USD.m := as.numeric(TV.USD.m)]
survey_data[, source := "Survey"]
survey_data[, newMoney := "FALSE"]

sub_grants <- sub_grants[tolower(Take.out) == "y"]

# Build FTS ↔ survey organisation name mapping
fts_survey_overlap <- fread("reference_datasets/fts_survey_overlap.csv", header = T)
name_mapping <- unique(fts_survey_overlap[, .(destinationObjects_Organization.name, Organisation = `Survey name`)])

# Identify (Organisation, Year) pairs covered by survey → exclude from FTS agg
survey_years <- unique(survey_data[, .(Organisation, Year)])
survey_years <- merge(
  survey_years,
  name_mapping,
  by = "Organisation",
  all.x = T,
  sort = F,
  allow.cartesian = T
)

missing_mapping <- unique(survey_years[is.na(destinationObjects_Organization.name), Organisation])
if (length(missing_mapping))
  message("No FTS name mapping for survey orgs: ",
          paste(missing_mapping, collapse = ", "))

survey_years <- survey_years[!is.na(destinationObjects_Organization.name)]
survey_overlap_keys <- survey_years[, paste(destinationObjects_Organization.name, Year)]

# Org type lookup
cva_org_type <- fread("reference_datasets/cva_org_type.csv")
setnames(cva_org_type, "cva_org_type", "Org_type")

# Aggregate FTS CVA by org × year
fts_cva_agg <- fts_cva[, .(PC.USD.m = sum(CVAamount, na.rm = T) / 1e6), by = .(
  Year = as.integer(year),
  newMoney,
  destinationObjects_Organization.name,
  destinationObjects_Organization.organizationSubTypes
)]

fts_cva_agg[, source := "FTS"]
fts_cva_agg <- merge(
  fts_cva_agg,
  name_mapping,
  by = "destinationObjects_Organization.name",
  all.x = T,
  sort = F
)
fts_cva_agg <- merge(
  fts_cva_agg,
  cva_org_type,
  by = "destinationObjects_Organization.organizationSubTypes",
  all.x = T,
  sort = F
)

fts_cva_agg[, Local_type := "International"]
fts_cva_agg[grepl("National|Local", destinationObjects_Organization.organizationSubTypes), Local_type := "National"]

# Remove FTS rows for (org, year) pairs already covered by the survey
fts_cva_agg[, .key := paste(destinationObjects_Organization.name, Year)]
fts_cva_agg <- fts_cva_agg[!.key %in% survey_overlap_keys][, .key := NULL]


# Combine FTS and survey
cva_agg <- rbindlist(list(survey_data, fts_cva_agg),
                     fill = T,
                     use.names = T)

# Impute TV from PC using the annual PC→TV ratio
cva_agg <- merge(
  cva_agg,
  pc_tv_estimate,
  by = "Year",
  all.x = T,
  sort = F
)

cva_agg[is.na(TV.USD.m), TV.USD.m := PC.USD.m*PC.average.used]
cva_agg[is.na(PC.USD.m), PC.USD.m := TV.USD.m/PC.average.used]

cva_agg[is.na(Organisation), Organisation := destinationObjects_Organization.name]

# Sub-grant name matching
# Sub-grant recipient names must be matched to organisations in cva_agg so we
# can subtract the received sub-grant amounts and avoid double-counting.
# Four strategies in priority order:
# 1. Exact match (after normalisation)
# 2. Fuzzy (Levenshtein distance ≤ 20% of string length)
# 3. Substring A — recipient name is a whole word in org name
# 4. Substring B — org name is a whole word in recipient name
# Followed by a small set of manual overrides for the residual cases.

normalise <- function(x)
  trimws(gsub("\\s+", " ", gsub("[[:punct:]]", " ", tolower(x))))
quotemeta <- function(x)
  gsub("(\\W)", "\\\\\\1", x, perl = T)

# Apply recipient substitutions BEFORE normalising
sub_grants[Recipient.org %in% c("Unknown", "Governments", "NGOs", "Local and national partners"), Recipient.org := Donor.org]
sub_grants[Recipient.org.type == "RCRC", Recipient.org := Donor.org]

sub_grants[, clean_name := normalise(Recipient.org)]
sub_grants[clean_name %in% c("unknown", "not provided potentially sensitive"), clean_name := NA_character_]

cva_agg[, clean_org := normalise(Organisation)]

uniq_sub <- na.omit(unique(sub_grants$clean_name))
uniq_sub <- uniq_sub[uniq_sub != ""]
uniq_orgs <- na.omit(unique(cva_agg$clean_org))
uniq_orgs <- uniq_orgs[uniq_orgs != ""]

match_dt <- data.table(
  subgrant_name = uniq_sub,
  perfect_match = NA_character_,
  fuzzy_match = NA_character_,
  fuzzy_dist = NA_integer_,
  substr_a_match = NA_character_,
  substr_b_match = NA_character_
)

# 1. Exact match
exact_idx <- match(match_dt$subgrant_name, uniq_orgs)
match_dt[!is.na(exact_idx), perfect_match := uniq_orgs[na.omit(exact_idx)]]
message(sprintf(
  "Exact match: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$perfect_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$perfect_match))
))

# 2. Fuzzy (Levenshtein, vectorised distance matrix)
dist_mat <- stringdistmatrix(match_dt$subgrant_name, uniq_orgs, method = "lv")
allowable <- pmax(ceiling(0.20 * nchar(match_dt$subgrant_name)), 1L)

best_dist <- apply(dist_mat, 1, min)
best_col <- apply(dist_mat, 1, which.min)
fuzzy_ok <- best_dist <= allowable

match_dt[fuzzy_ok, `:=`(fuzzy_match = uniq_orgs[best_col[fuzzy_ok]], fuzzy_dist = best_dist[fuzzy_ok])]

# Clear a known incorrect fuzzy hit before reporting
match_dt[subgrant_name == "drc", fuzzy_match := NA_character_]

message(sprintf(
  "Fuzzy match: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$fuzzy_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$fuzzy_match))
))

# 3. Substring A: recipient ⊆ org
for (i in seq_len(nrow(match_dt))) {
  regex <- paste0("\\b", quotemeta(match_dt$subgrant_name[i]), "\\b")
  hits <- which(grepl(regex, uniq_orgs, perl = T))
  if (length(hits))
    match_dt[i, substr_a_match := uniq_orgs[hits[which.min(nchar(uniq_orgs[hits]))]]]
}
message(sprintf(
  "Substring A: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$substr_a_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$substr_a_match))
))

# 4. Substring B: org ⊆ recipient (only for still-unmatched rows)
still_unmatched <- is.na(match_dt$perfect_match) &
  is.na(match_dt$fuzzy_match) &
  is.na(match_dt$substr_a_match)

for (j in seq_along(uniq_orgs)) {
  regex <- paste0("\\b", quotemeta(uniq_orgs[j]), "\\b")
  hits <- which(still_unmatched &
                  grepl(regex, match_dt$subgrant_name, perl = T))
  if (length(hits))
    match_dt[hits, substr_b_match := uniq_orgs[j]]
}
message(sprintf(
  "Substring B: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$substr_b_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$substr_b_match))
))

pct_combined <- 100 * mean(
  !is.na(match_dt$perfect_match) |
    !is.na(match_dt$fuzzy_match) |
    !is.na(match_dt$substr_a_match) |
    !is.na(match_dt$substr_b_match)
)
message(sprintf("Combined: %.0f%%", pct_combined))

# 5. Manual overrides
set_manual <- function(dt, pattern, target, regex = F) {
  idx <- if (regex)
    grepl(pattern, dt$subgrant_name, perl = T)
  else
    dt$subgrant_name == pattern
  dt[idx, perfect_match := target]
}

UNRWA_FULL <- paste0("united nations relief and works agency for ",
                     "palestine refugees in the near east")

set_manual(match_dt, "care bangladesh", "care international")
set_manual(match_dt, "wfp", "world food programme")
set_manual(match_dt,
           "save the childrensave the children",
           "save the children")
set_manual(match_dt, "wvi", "world vision international")
set_manual(match_dt,
           "world vision|vision mund",
           "world vision international",
           T)
set_manual(match_dt, "acf", "action against hunger")
set_manual(match_dt, "acf ethiopia", "action against hunger")
set_manual(match_dt, "action contre la faim espagne", "action against hunger")
set_manual(match_dt, "cww", "concern worldwide")
set_manual(match_dt, "dan church aid", "dca")
set_manual(match_dt, "drc", "danish refugee council")
set_manual(match_dt, "norwegian refugee council", "nrc")
set_manual(match_dt, "pin", "people in need")
set_manual(match_dt, "unrwa", UNRWA_FULL)
set_manual(match_dt, "unrwa united nations relief and wor", UNRWA_FULL)
set_manual(match_dt, "the united nations relief and works", UNRWA_FULL)
set_manual(match_dt, "united nations children s fund", "unicef")
set_manual(match_dt,
           "red (cross|crescent)",
           "red cross and red crescent movement",
           T)
set_manual(match_dt, "plan malawi", "plan international")
set_manual(match_dt,
           "adra romania",
           "adventist development and relief agency")
set_manual(match_dt, "somali cash consortium", "concern worldwide")

# Report any genuinely unresolved names
unresolved <- match_dt[is.na(perfect_match) & is.na(fuzzy_match) &
                         is.na(substr_a_match) &
                         is.na(substr_b_match), .(subgrant_name)]
if (nrow(unresolved)) {
  message("Unmatched sub-grant recipients (excluded from de-doubling):")
  print(unresolved)
}

# Resolve to best single match per recipient
match_dt[, best_match := perfect_match]
match_dt[is.na(best_match), best_match := fuzzy_match]
match_dt[is.na(best_match), best_match := substr_a_match]
match_dt[is.na(best_match), best_match := substr_b_match]

org_lookup <- setNames(match_dt$best_match, match_dt$subgrant_name)
sub_grants[, clean_org := org_lookup[clean_name]]
sub_grants[, newMoney := "FALSE"]

# Aggregate sub-grant amounts and subtract from recipient totals
sub_grants_agg <- sub_grants[!is.na(clean_org), .(PC.USD.m_subgrant = sum(Amount.USD, na.rm = T) / 1e6), by = .(clean_org, Year, newMoney)]

cva_agg <- merge(
  cva_agg,
  sub_grants_agg,
  by = c("clean_org", "Year", "newMoney"),
  all.x = T,
  sort = F
)
cva_agg[is.na(PC.USD.m_subgrant), PC.USD.m_subgrant := 0]
cva_agg[, PC.USD.m_undoubled := pmax(PC.USD.m - PC.USD.m_subgrant, 0)]

# Scale the TV deduction using the same PC→TV ratio as above
cva_agg[, TV.USD.m_subgrant := PC.USD.m_subgrant * PC.average.used]
cva_agg[, TV.USD.m_undoubled := pmax(TV.USD.m - TV.USD.m_subgrant, 0)]
cva_agg[, PC.average.used := NULL]

# Aggregate by org type × year
cva_agg_org_type <- cva_agg[, .(
  PC.USD.m = sum(PC.USD.m_undoubled, na.rm = T),
  TV.USD.m = sum(TV.USD.m_undoubled, na.rm = T)
), by = .(Year, Org_type, Local_type)]
setorder(cva_agg_org_type, Year, Org_type, Local_type, -PC.USD.m)

# Write outputs
fwrite(cva_agg, "output/cva_agg.csv")
fwrite(cva_agg_org_type, "output/cva_agg_org_type.csv")
message("Written output/cva_agg.csv and output/cva_agg_org_type.csv")

# Summary
message("\nGlobal CVA by year (PC USD billions, de-doubled):")
print(cva_agg[, .(PC_bn = round(sum(PC.USD.m_undoubled, na.rm = T) / 1e3, 2), TV_bn = round(sum(TV.USD.m_undoubled, na.rm = T) / 1e3, 2)), by = Year][order(Year)])

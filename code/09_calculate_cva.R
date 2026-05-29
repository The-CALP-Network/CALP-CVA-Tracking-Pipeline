# 09_calculate_cva.R
# Calculates the estimated CVA USD amount for each flagged FTS flow using a
# priority hierarchy:
#
# 1. Sector / method / cluster — Full or Partial (÷ cluster count)
# 2. Project CVA percentage — for flows not already assigned in step 1
# 3. High-confidence ML — ≥80 % confidence + common cash keyword
# 4. Prior manual decisions — from reference_datasets/historical_cva_decisions.csv
# 5. New manual queue — written to output/cva_to_manually_classify.csv
# (read back when output/cva_manually_classified.csv exists)
#
# Bug fixes vs. original:
# • CVAamount_type label is saved BEFORE CVAamount is updated (index reuse bug)
# • CTP flows cannot be downgraded to Partial (fixed in 08a)
# • relevance and sector_method_cluster_relevance are now consistent (fixed in 08a)
#
# Outputs:
# output/cva_to_manually_classify.csv — flows awaiting manual review
# output/fts_cva.csv — final dataset with CVAamount column
#
# Run from the project root:
# Rscript code/09_calculate_cva.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

if (!dir.exists("output"))
  dir.create("output")

fts_flagged <- fread("output/fts_output_CVA.csv")
fts_flagged[, amountUSD := as.numeric(amountUSD)]

# Initialise CVAamount columns
fts_flagged[, `:=`(CVAamount = 0, CVAamount_type = "")]

# Step 1: Full sector / method / cluster
idx <- fts_flagged$sector_method_cluster_relevance == "Full"
fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "Sector, method, cluster")]

# Step 2: Partial sector / cluster (÷ number of destination clusters)
idx <- fts_flagged$sector_method_cluster_relevance == "Partial"
fts_flagged[idx, `:=`(CVAamount = amountUSD / destinationClusterCount,
                      CVAamount_type = "Partial cluster")]

# Step 3: Project CVA percentage
# Bug fix: capture the qualifying index BEFORE updating CVAamount so the
# type label uses the same set of rows as the amount assignment.
idx <- which(fts_flagged$CVAamount == 0 &
               !is.na(fts_flagged$project_cva_percentage))
fts_flagged[idx, `:=`(CVAamount = amountUSD * project_cva_percentage,
                      CVAamount_type = "Project CVA percentage")]

# Step 4a: High-confidence Full ML prediction + common keyword
COMMON_KW_REGEX <- "\\b(cash|vouchers?|cva|coupon)\\b"
fts_flagged[, common_words_match :=
              grepl(COMMON_KW_REGEX, all_text, ignore.case = T)]

idx <- which(
  fts_flagged$CVAamount == 0 &
    !is.na(fts_flagged$predicted_confidence) &
    fts_flagged$predicted_confidence >= 0.8 &
    fts_flagged$common_words_match
)
fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "ML high predicted relevance")]

# Step 4b: High-confidence Partial ML prediction + common keyword (apply average proportion from known partials)
COMMON_KW_REGEX <- "\\b(cash|vouchers?|cva|coupon)\\b"
fts_flagged[, common_words_match :=
              grepl(COMMON_KW_REGEX, all_text, ignore.case = T)]

idx <- which(
  fts_flagged$CVAamount == 0 &
    !is.na(fts_flagged$predicted_confidence) &
    fts_flagged$predicted_confidence <= 0.2 &
    fts_flagged$common_words_match
)

partial_share <- fts_flagged[grepl("Partial", relevance), sum(CVAamount)/sum(amountUSD)]

fts_flagged[idx, `:=`(CVAamount = amountUSD*partial_share, CVAamount_type = "ML high predicted partial relevance")]

# Step 5a: Prior manual decisions
POSITIVE_DECISIONS <- c("Decision: accept; judgement", "Decision: include; judgement")

prior_manual_file <- "reference_datasets/historical_cva_decisions.csv"
if (file.exists(prior_manual_file)) {
  prior_manual <- fread(prior_manual_file)
  
  # Warn if any unrecognised decision strings exist — silent drops are a risk
  known_decisions <- unique(prior_manual$decision)
  unexpected <- setdiff(
    known_decisions,
    c(
      POSITIVE_DECISIONS,
      "Decision: exclude; judgement",
      "Decision: insufficient text; judgement",
      "Decision: false positive; judgement"
    )
  )
  if (length(unexpected))
    warning(
      "Unrecognised decision label(s) in ",
      prior_manual_file,
      ": ",
      paste(unexpected, collapse = ", "),
      "\n These rows will be EXCLUDED from CVA totals."
    )
  
  positive_ids <- prior_manual[decision %in% POSITIVE_DECISIONS, id]
  
  idx <- which(fts_flagged$CVAamount == 0 &
                 fts_flagged$id %in% positive_ids)
  fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "Manual")]
  
  # Add confirmed positive examples to classifier training data
  new_training <- fts_flagged[CVAamount_type == "Manual", .(id, text = all_text)][, label := 1L]
  classifier_data <- fread("classifier_code/CVA_flow_descriptions.csv")
  classifier_data[, text := gsub("\"", "", text)]
  new_training <- new_training[!id %in% classifier_data$id &
                                 !text %in% classifier_data$text]
  if (nrow(new_training)) {
    fwrite(rbindlist(list(classifier_data, new_training), fill = T),
           "classifier_code/CVA_flow_descriptions.csv", quote = T)
    message(nrow(new_training),
            " new training example(s) added to classifier data.")
  }
} else {
  message("No prior manual decisions file found at ",
          prior_manual_file,
          " — skipping.")
  prior_manual <- data.table(id = character(0))
}

# Step 5b: Queue remaining uncertain flows for manual review
# Flows with ML score 0.2-0.8 go to a manual review file. 
# Those already handled by the prior decisions file
# are excluded.
manual_queue_idx <- which(
  fts_flagged$CVAamount == 0 &
  !is.na(fts_flagged$predicted_confidence) &
  !fts_flagged$common_words_match &
  fts_flagged$predicted_confidence > 0.2 &
  fts_flagged$predicted_confidence < 0.8)

fts_manual_queue <- fts_flagged[manual_queue_idx]
fts_manual_queue[, CVAamount_type := "Manual"]
fts_manual_uncoded <- fts_manual_queue[!id %in% prior_manual$id]

fwrite(fts_manual_uncoded, "output/cva_to_manually_classify.csv")
message(
  nrow(fts_manual_uncoded),
  " flow(s) written to output/cva_to_manually_classify.csv for manual review."
)

fts_cva <- fts_flagged[CVAamount > 0 & is.finite(CVAamount)]

# Summary
message("\nCVA amount type breakdown:")
print(fts_cva[, .(flows = .N,
                  total_CVA_USD = sum(CVAamount, na.rm = T)), by = CVAamount_type][order(-total_CVA_USD)])

fwrite(fts_cva, "output/fts_cva.csv")
message("\nFinal dataset written to output/fts_cva.csv (",
        nrow(fts_cva),
        " flows)")

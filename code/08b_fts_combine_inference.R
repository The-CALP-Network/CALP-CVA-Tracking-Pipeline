# 08b_fts_combine_inference.R
# PART 2 of 2 — run AFTER the Python ML inference step.
#
# Loads the pre-ML dataset and the classifier output, applies the ML
# predictions to update relevance, and writes the final flagged CVA flows.
#
# Run from the project root:
# Rscript code/08b_fts_combine_inference.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

infer_file <- "classifier_code/fts_to_inference_output.csv"
pre_ml_file <- "output/fts_output_pre_ml.csv"

if (!file.exists(infer_file)) {
  stop(
    "ML inference output not found at ",
    infer_file,
    "\nPlease run: cd classifier_code && source venv/bin/activate && ",
    "python3 flow_inference.py"
  )
}

fts <- fread(pre_ml_file)
infer <- fread(infer_file)

# The inference CSV may contain a text column we no longer need
infer[, `:=` (text = NULL, id = as.character(id))]

fts <- merge(fts,
             infer,
             by = "id",
             all.x = T,
             sort = F)

# Apply ML predictions to still-unclassified flows
# Only update rows that are currently "None" AND were sent to the classifier
# (i.e. they had a keyword hit or a project CVA flag).

ml_candidate <- fts$relevance == "None" &
  (fts$keyword_match |
     (!is.na(fts$project_cva) & fts$project_cva)) &
  !is.na(fts$predicted_class)

fts[ml_candidate & predicted_class == "Partial", `:=`(
  relevance = "Partial",
  relevance_method = fifelse(keyword_match, "Keyword + ML", "Project API + ML")
)]

fts[ml_candidate & predicted_class == "Full", `:=`(
  relevance = "Full",
  relevance_method = fifelse(keyword_match, "Keyword + ML", "Project API + ML")
)]

# Diagnostic summaries
message("Relevance distribution:")
print(fts[, .N, by = relevance][order(-N)])
message("\nRelevance method breakdown (flagged flows only):")
print(fts[relevance != "None", .N, by = relevance_method][order(-N)])

# Keep only flagged flows
fts_flagged <- fts[relevance != "None"]
fwrite(fts_flagged, "output/fts_output_CVA.csv")
message("\n",
        nrow(fts_flagged),
        " flagged flows written to output/fts_output_CVA.csv")
message("Next step: Rscript code/09_calculate_cva.R")
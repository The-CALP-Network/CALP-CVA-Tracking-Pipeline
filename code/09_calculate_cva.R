list.of.packages <- c("data.table", "jsonlite","tidyverse", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())
setwd("..")

fts_flagged = fread("output/fts_output_CVA.csv")
fts_flagged$amountUSD = as.numeric(fts_flagged$amountUSD)
# Count the number of destination clusters
fts_flagged$destinationClusterCount = str_count(fts_flagged$destinationObjects_Cluster.name,";") + 1
fts_flagged$destinationClusterCount[which(fts_flagged$destinationObjects_Cluster.name=="")] = 0

# CVAamount for full is entire amountUSD
fts_flagged$CVAamount = 0
fts_flagged$CVAamount_type = ""
fts_flagged$CVAamount[which(fts_flagged$sector_method_cluster_relevance=="Full")] = 
  fts_flagged$amountUSD[which(fts_flagged$sector_method_cluster_relevance=="Full")]
fts_flagged$CVAamount_type[which(fts_flagged$sector_method_cluster_relevance=="Full")] = "Sector, method, cluster"

# CVAamount for partial is amountUSD divided by the number of destination clusters
fts_flagged$CVAamount[which(fts_flagged$sector_method_cluster_relevance=="Partial")] = 
  fts_flagged$amountUSD[which(fts_flagged$sector_method_cluster_relevance=="Partial")] /
  fts_flagged$destinationClusterCount[which(fts_flagged$sector_method_cluster_relevance=="Partial")]
fts_flagged$CVAamount_type[which(fts_flagged$sector_method_cluster_relevance=="Partial")] = "Partial cluster"


# CVAamount for projects with reported CVA percentages
fts_flagged$CVAamount[which(fts_flagged$CVAamount == 0 & !is.na(fts_flagged$project_cva_percentage))] = 
  fts_flagged$amountUSD[which(fts_flagged$CVAamount == 0 & !is.na(fts_flagged$project_cva_percentage))] /
  fts_flagged$project_cva_percentage[which(fts_flagged$CVAamount == 0 & !is.na(fts_flagged$project_cva_percentage))]
fts_flagged$CVAamount_type[which(fts_flagged$CVAamount == 0 & !is.na(fts_flagged$project_cva_percentage))] = "Project CVA percentage"

# CVAamount for flows with highly likely CVA predicted relevance and common words
fts_flagged$common_words_match = grepl("\\bcash\\b|\\bvoucher\\b|\\bvouchers\\b|\\bcva\\b|\\bcoupon\\b", fts_flagged$all_text, ignore.case=T)
high_confidence_index = which(fts_flagged$CVAamount == 0 & fts_flagged$predicted_confidence >= 0.8 & fts_flagged$common_words_match)
fts_flagged$CVAamount[high_confidence_index] = 
  fts_flagged$amountUSD[high_confidence_index]
fts_flagged$CVAamount_type[high_confidence_index] = "ML high predicted relevance"


# For remaining flows with CVAamount of zero that have predicted_confidence >= 0.5 and not (< 0.8 & common_word_match)
# go to manual 
manual_classify_index = which(
  fts_flagged$CVAamount == 0 &
    fts_flagged$predicted_confidence >= 0.5 &
    !(fts_flagged$predicted_confidence >= 0.8 & fts_flagged$common_words_match)
)
fts_manual = fts_flagged[manual_classify_index,]
fts_manual$CVAamount_type = "Manual"
# Read last manual file
fts_prior_manual = fread("reference_datasets/Mike_cva_decisions.csv")
positive_ids = subset(fts_prior_manual, decision %in% c("Decision: accept; judgement", "Decision: include; judgement"))

# Write out those that have not been previously manually reviewed
fts_manual_uncoded = subset(fts_manual, !id %in% fts_prior_manual$id)
fwrite(fts_manual_uncoded, "output/cva_to_manually_classify.csv")

# Treat those that have been manually reviewed as Full. Enhance training data
fts_flagged_manual_full = subset(fts_flagged, id %in% positive_ids$id)
fts_flagged_manual_full = fts_flagged_manual_full[,c("id", "all_text")]
setnames(fts_flagged_manual_full, "all_text", "text")
fts_flagged_manual_full$label = 1
classifier_data = fread("classifier_code/CVA_flow_descriptions.csv")
fts_flagged_manual_full = subset(fts_flagged_manual_full, !id %in% classifier_data$id)
fts_flagged_manual_full = subset(fts_flagged_manual_full, !text %in% classifier_data$text)
classifier_data = rbind(classifier_data, fts_flagged_manual_full)
fwrite(classifier_data, "classifier_code/CVA_flow_descriptions.csv")
fts_flagged$CVAamount[which(fts_flagged$CVAamount == 0 & fts_flagged$id %in% positive_ids$id)] =
  fts_flagged$amountUSD[which(fts_flagged$CVAamount == 0 & fts_flagged$id %in% positive_ids$id)]
fts_flagged$CVAamount_type[which(fts_flagged$CVAamount == 0 & fts_flagged$id %in% positive_ids$id)] = "Manual"

# Manual file is filled out prior to this step
fts_flagged_output = subset(fts_flagged, CVAamount > 0 & is.finite(CVAamount))
fts_manually_classified = fread("output/cva_manually_classified.csv")
fts_cva = rbind(fts_flagged_output, fts_manually_classified)
fwrite(fts_cva, "output/fts_cva.csv")

list.of.packages <- c("data.table","tidyverse", "openxlsx", "stringr", "stringdist")
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
source("04_fts_curated_flows.R")
setwd("..")

# Update 2024
years = 2024
path = "fts/"
fts_all <- fts_curated_flows(years, update_years=2024, dataset_path = path)
for(i in 1:length(years)){
  fwrite(fts_all[year == years[[i]]], paste0(path, "fts_curated_", years[[i]], ".csv"))
}

# Run data pipeline until fts_cva.csv is updated
file.remove("projects/project_data_2024.RData") # Remove 2024 project data to refresh
source("code/06_fetch_projects.R")
source("code/07_process_project_data.R")
file.remove("classifier_code/fts_to_inference_output.csv")
source("code/08_fts_keyword_searching_cash.R")
# python3 flow_inference.py
# We can just run this one twice after python updates the classifications
source("code/08_fts_keyword_searching_cash.R")
source("code/09_calculate_cva.R")

# Load fts_cva.csv
fts = fread("output/fts_cva.csv")

# Subset to 2024, non-pledge, and USA
usa_source_orgs = c(
  "United States of America, Government of",
  "United States Department of State",
  "United States Agency for International Development"
)
fts_2024 = subset(
  fts,
  year == 2024 & status != "pledge"
)
fts_usa_2024 = subset(
  fts_2024,
  sourceObjects_Organization.name %in% usa_source_orgs
)

# Aggregate to source location
fts_2024_location = fts_2024[,.(CVAamount=sum(CVAamount)), by=.(
  destinationObjects_Location.name
)]
fts_usa_2024_location = fts_usa_2024[,.(CVAamount_USA=sum(CVAamount)), by=.(
  destinationObjects_Location.name
)]

# Merge by location
fts_2024_usa_comp = merge(
  fts_2024_location,
  fts_usa_2024_location,
  by="destinationObjects_Location.name",
  all=T
)

# Fill in zero for missing values
fts_2024_usa_comp$CVAamount_USA[which(is.na(fts_2024_usa_comp$CVAamount_USA))] = 0

# Calculate USA percentage
fts_2024_usa_comp$CVAamount_USA_pc =
  fts_2024_usa_comp$CVAamount_USA /
  fts_2024_usa_comp$CVAamount

# Reorder alphabetically
fts_2024_usa_comp = fts_2024_usa_comp[order(fts_2024_usa_comp$destinationObjects_Location.name),]

# Write
fwrite(fts_2024_usa_comp, "output/fts_cva_USA_analysis.csv")
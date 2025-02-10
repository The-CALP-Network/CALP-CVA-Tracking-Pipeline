list.of.packages <- c("data.table","tidyverse", "xlsx", "stringr")
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

# Read previous output
fts_cva = fread("output/fts_cva.csv")

# Reads in the CVA survey data
survey_data = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheetIndex=1)
survey_data$Organisation = str_trim(survey_data$Organisation)
survey_data$PC.USD.m = as.numeric(survey_data$PC.USD.m)
survey_data$TV.USD.m = as.numeric(survey_data$TV.USD.m)
survey_data$Cash.USD = as.numeric(survey_data$Cash.USD)
survey_data$Vouchers.USD = as.numeric(survey_data$Vouchers.USD)
survey_data$CVAamount =
  rowSums(survey_data[,c("Cash.USD", "Vouchers.USD")], na.rm=T)
sub_grants = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheetIndex=2)
sub_grants = subset(sub_grants, tolower(Take.out)=="n")
pc_tv_estimate = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheetIndex=3)

# Reads in the fts_survey_overlap to use as mapping, overlap calculated by survey data automatically
fts_survey_overlap = fread("reference_datasets/fts_survey_overlap.csv", header=T)
name_mapping = fts_survey_overlap[,c("destinationObjects_Organization.name", "Survey name")]
setnames(name_mapping, "Survey name", "Organisation")
fts_survey_overlap_long = unique(survey_data[,c("Organisation", "Year")])
fts_survey_overlap_long = merge(fts_survey_overlap_long, name_mapping, by="Organisation", all.x=T)
# Missing names from mapping
unique(fts_survey_overlap_long$Organisation[which(is.na(fts_survey_overlap_long$destinationObjects_Organization.name))])
fts_survey_overlap_long = subset(fts_survey_overlap_long, !is.na(destinationObjects_Organization.name))
survey_overlap_combinations = paste(fts_survey_overlap_long$destinationObjects_Organization.name, fts_survey_overlap_long$Year)

# Reads in the attached cva_org_type file so that it can be joined with fts_cva by “destinationObjects_Organization.organizationSubTypes”
cva_org_type = fread("reference_datasets/cva_org_type.csv")

# Aggregate FTS for joining
sub_type_mapping = unique(fts_cva[,c("destinationObjects_Organization.name", "destinationObjects_Organization.organizationSubTypes")])
fts_cva_agg = fts_cva[,.(CVAamount=sum(CVAamount)), by=.(
  year,
  destinationObjects_Organization.name
)]
fts_cva_agg$source = "FTS"
fts_cva_agg = merge(fts_cva_agg, name_mapping, by="destinationObjects_Organization.name", all.x=T)

# Remove surveyed years
fts_cva_agg$org_year = paste(fts_cva_agg$destinationObjects_Organization.name, fts_cva_agg$year)
fts_cva_agg = subset(fts_cva_agg, !org_year %in% survey_overlap_combinations)
fts_cva_agg$org_year = NULL

# Add survey CVA
survey_data = merge(survey_data, name_mapping, by="Organisation", all.x=T)
survey_data_harmonized = survey_data[,c("Year", "destinationObjects_Organization.name", "Organisation", "CVAamount")]
setnames(survey_data_harmonized, "Year", "year")
survey_data_harmonized$source = "Survey"
cva_agg = rbind(fts_cva_agg, survey_data_harmonized)
cva_agg$Organisation[which(is.na(cva_agg$Organisation))] = 
  cva_agg$destinationObjects_Organization.name[which(is.na(cva_agg$Organisation))]

# Merge org types
cva_agg = merge(cva_agg, sub_type_mapping, by="destinationObjects_Organization.name", all.x=T)
cva_agg = merge(
  cva_agg, 
  cva_org_type, 
  by="destinationObjects_Organization.organizationSubTypes",
  all.x=T
)

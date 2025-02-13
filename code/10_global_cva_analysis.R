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
setwd("..")

# Read previous output
fts_cva = fread("output/fts_cva.csv")

# Exclude large confidental flows
fts_cva = subset(fts_cva, destinationObjects_Organization.name!="International NGOs (Confidential)")

# Reads in the CVA survey data
survey_data = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet=1)
survey_data$Organisation = str_trim(survey_data$Organisation)
survey_data$PC.USD.m = as.numeric(survey_data$PC.USD.m)
survey_data$TV.USD.m = as.numeric(survey_data$TV.USD.m)
sub_grants = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet=2)
sub_grants = subset(sub_grants, tolower(Take.out)=="y")
pc_tv_estimate = read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet=3)
setnames(pc_tv_estimate, "CVA.data.year", "year")

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
setnames(cva_org_type, "cva_org_type", "Org_type")

# Aggregate FTS for joining
fts_cva_agg = fts_cva[,.(PC.USD.m=sum(CVAamount) / 1e6), by=.(
  year,
  newMoney,
  destinationObjects_Organization.name,
  destinationObjects_Organization.organizationSubTypes
)]
fts_cva_agg$source = "FTS"
fts_cva_agg = merge(fts_cva_agg, name_mapping, by="destinationObjects_Organization.name", all.x=T)
fts_cva_agg = merge(fts_cva_agg, cva_org_type, by="destinationObjects_Organization.organizationSubTypes", all.x=T)

# Remove surveyed years
fts_cva_agg$org_year = paste(fts_cva_agg$destinationObjects_Organization.name, fts_cva_agg$year)
fts_cva_agg = subset(fts_cva_agg, !org_year %in% survey_overlap_combinations)
fts_cva_agg$org_year = NULL

# Impute TV
fts_cva_agg = merge(fts_cva_agg, pc_tv_estimate, by="year", all.x=T)
fts_cva_agg$TV.USD.m = fts_cva_agg$PC.USD.m * fts_cva_agg$PC.average.used
fts_cva_agg$PC.average.used = NULL

# Add FTS to survey CVA
setnames(
  fts_cva_agg,
  c("year"),
  c("Year")
)
survey_data$source = "Survey"
survey_data$newMoney = "FALSE"
cva_agg = rbindlist(list(survey_data, fts_cva_agg), fill=T)
cva_agg$Organisation[which(is.na(cva_agg$Organisation))] = 
  cva_agg$destinationObjects_Organization.name[which(is.na(cva_agg$Organisation))]

# Match subgrant names
quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

remove_punct = function(string){
  str_replace_all(string, "[[:punct:]]", " ")
}

collapse_whitespace = function(string){
  str_replace_all(string, "\\s+", " ")
}
sub_grants$clean_name = str_trim(collapse_whitespace(remove_punct(tolower(sub_grants$Recipient.org))))
sub_grants$clean_name[
  which(sub_grants$clean_name %in% c("unknown", "not provided potentially sensitive"))
] = NA
unique_subgrant_recipients = unique(sub_grants$clean_name)
unique_subgrant_recipients = unique_subgrant_recipients[which(!unique_subgrant_recipients %in% c(NA, ""))]
cva_agg$clean_org = str_trim(collapse_whitespace(remove_punct(tolower(cva_agg$Organisation))))
unique_org_names = unique(cva_agg$clean_org)
unique_org_names = unique_org_names[which(!unique_org_names %in% c(NA, ""))]

match_df = data.frame(subgrant_recipient_org=unique_subgrant_recipients)
match_df$perfect_match_name = NA
match_df$fuzzy_match_name = NA
match_df$fuzzy_match_distance = NA
match_df$substring_a_match_name = NA
match_df$substring_b_match_name = NA

# Try match on exact name
for(i in 1:nrow(match_df)){
  org_name = match_df[i,"subgrant_recipient_org"]
  if(org_name %in% unique_org_names){
    match_index = which(unique_org_names==org_name)
    match_name = unique_org_names[match_index]
    match_df$perfect_match_name[i] = match_name
  }
}

# Percentage matched by exact name
sum(!is.na(match_df$perfect_match_name)) / nrow(match_df)

# Match by string distance
allowable_percentage_difference = 0.20 # 20% of the string
for(i in 1:nrow(match_df)){
  org_name = match_df[i,"subgrant_recipient_org"]
  allowable_difference = ceiling(allowable_percentage_difference * nchar(org_name))
  allowable_difference = max(allowable_difference, 1) # minimum 1 character
  distance_vector = stringdist(org_name, unique_org_names)
  matches_by_distance = which(distance_vector <= allowable_difference)
  if(length(matches_by_distance) >= 1){
    distances = distance_vector[matches_by_distance]
    match_index = matches_by_distance[which.min(distances)] # Shortest distance
    match_name = unique_org_names[match_index]
    match_df$fuzzy_match_name[i] = match_name
    match_df$fuzzy_match_distance[i] = distance_vector[match_index]
  }
}

# Percentage matched by distance
sum(!is.na(match_df$fuzzy_match_name)) / nrow(match_df)

# Joint percentage matched so far
(
  sum(!is.na(match_df$perfect_match_name)| !is.na(match_df$fuzzy_match_name))
) / nrow(match_df)

fuzz = subset(match_df, !is.na(fuzzy_match_name))
fuzz = fuzz[order(-fuzz$fuzzy_match_distance),]
View(fuzz[,c("subgrant_recipient_org", "fuzzy_match_name", "fuzzy_match_distance")])
# Manual fix incorrect fuzzy match
match_df$fuzzy_match_name[which(match_df$subgrant_recipient_org=="drc")] = NA

# Match on subgrant org being a substring of org
for(i in 1:nrow(match_df)){
  org_name = match_df[i,"subgrant_recipient_org"]
  org_regex = paste0(
    "\\b",
    quotemeta(org_name),
    "\\b"
  )
  substring_matches = which(grepl(org_regex, unique_org_names, perl=T))
  if(length(substring_matches) >= 1){
    all_matching_names = unique_org_names[substring_matches]
    name_lengths = nchar(all_matching_names)
    match_index = substring_matches[which.min(name_lengths)]
    match_name = unique_org_names[match_index]
    match_df$substring_a_match_name[i] = match_name
  }
}


# Percentage matched by substring a
sum(!is.na(match_df$substring_a_match_name)) / nrow(match_df)

# Joint percentage matched so far
(
  sum(!is.na(match_df$perfect_match_name) |
        !is.na(match_df$fuzzy_match_name) |
        !is.na(match_df$substring_a_match_name)
  )
) / nrow(match_df)

# Match on org being a substring of subgrant org for remaining unmatched
match_df$unmatched = is.na(match_df$perfect_match_name) &
  is.na(match_df$fuzzy_match_name) &
  is.na(match_df$substring_a_match_name)

for(match_index in 1:length(unique_org_names)){
  match_name = unique_org_names[match_index]
  match_regex = paste0(
    "\\b",
    quotemeta(match_name),
    "\\b"
  )
  substring_matches = which(grepl(match_regex, match_df$subgrant_recipient_org, perl=T))
  if(length(substring_matches) >= 1){
    for(i in substring_matches){
      if(match_df$unmatched[i]){
        match_df$substring_b_match_name[i] = match_name
      }
    }
  }
}


# Percentage matched by substring b
sum(!is.na(match_df$substring_b_match_name)) / nrow(match_df)

# Joint percentage matched so far
(
  sum(!is.na(match_df$perfect_match_name) |
        !is.na(match_df$fuzzy_match_name) |
        !is.na(match_df$substring_a_match_name) |
        !is.na(match_df$substring_b_match_name)
  )
) / nrow(match_df)

# Manual matches
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="care bangladesh")
] = "care international"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="wfp")
] = "world food programme"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="save the childrensave the children")
] = "save the children"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="wvi")
] = "world vision international"
match_df$perfect_match_name[
  which(grepl("world vision|vision mund", match_df$subgrant_recipient_org))
] = "world vision international"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="acf")
] = "action against hunger"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="action contre la faim espagne")
] = "action against hunger"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="cww")
] = "concern worldwide"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="dan church aid")
] = "dca"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="drc")
] = "danish refugee council"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="norwegian refugee council")
] = "nrc"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="pin")
] = "people in need"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="unrwa")
] = "united nations relief and works agency for palestine refugees in the near east"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="unrwa united nations relief and wor")
] = "united nations relief and works agency for palestine refugees in the near east"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="the united nations relief and works")
] = "united nations relief and works agency for palestine refugees in the near east"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="united nations children s fund")
] = "unicef"
match_df$perfect_match_name[
  which(grepl("red (cross|crescent)", match_df$subgrant_recipient_org))
] = "red cross and red crescent movement"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="plan malawi")
] = "plan international"
match_df$perfect_match_name[
  which(match_df$subgrant_recipient_org=="adra romania")
] = "adventist development and relief agency"
match_df$unmatched = is.na(match_df$perfect_match_name) &
  is.na(match_df$fuzzy_match_name) &
  is.na(match_df$substring_a_match_name) &
  is.na(match_df$substring_b_match_name)
View(subset(match_df, unmatched, select="subgrant_recipient_org"))

subgrant_org_mapping = match_df$perfect_match_name
subgrant_org_mapping[which(is.na(subgrant_org_mapping))] = 
  match_df$fuzzy_match_name[which(is.na(subgrant_org_mapping))]
subgrant_org_mapping[which(is.na(subgrant_org_mapping))] = 
  match_df$substring_a_match_name[which(is.na(subgrant_org_mapping))]
subgrant_org_mapping[which(is.na(subgrant_org_mapping))] = 
  match_df$substring_b_match_name[which(is.na(subgrant_org_mapping))]
names(subgrant_org_mapping) = match_df$subgrant_recipient_org
sub_grants$clean_org = subgrant_org_mapping[sub_grants$clean_name]
sub_grants$newMoney = "FALSE"

# Aggregate
sub_grants_agg = data.table(sub_grants)[
  ,.(PC.USD.m_subgrant=sum(Amount.USD, na.rm=T)),
  by=.(clean_org, Year, newMoney)
]
sub_grants_agg = subset(sub_grants_agg, !is.na(clean_org))

# Merge and subtract
cva_agg = merge(cva_agg, sub_grants_agg, by=c("clean_org", "Year", "newMoney"), all.x=T)
cva_agg$PC.USD.m_subgrant[which(is.na(cva_agg$PC.USD.m_subgrant))] = 0
cva_agg$PC.USD.m_undoubled = cva_agg$PC.USD.m - cva_agg$PC.USD.m_subgrant
cva_agg$PC.USD.m_undoubled = pmax(cva_agg$PC.USD.m_undoubled, 0)
cva_agg_org_type = cva_agg[,.(PC.USD.m=sum(PC.USD.m_undoubled, na.rm=T)), by=.(Year, Org_type)]
setnames(pc_tv_estimate, "year", "Year")
cva_agg_org_type = merge(cva_agg_org_type, pc_tv_estimate, by="Year", all.x=T)
cva_agg_org_type$TV.USD.m = cva_agg_org_type$PC.USD.m * cva_agg_org_type$PC.average.used
cva_agg_org_type$PC.average.used = NULL

# Write
fwrite(cva_agg, "output/cva_agg.csv")
fwrite(cva_agg_org_type, "output/cva_agg_org_type.csv")

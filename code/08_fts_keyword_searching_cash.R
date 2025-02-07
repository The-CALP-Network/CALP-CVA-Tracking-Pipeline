list.of.packages <- c("data.table", "jsonlite","tidyverse")
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

##load in fts
years <- 2017:2024
fts_curated <- list()
for (i in 1:length(years)){
  year <- years[i]
  fts_curated[[i]] <- fread(paste0("fts/fts_curated_",year,".csv"))
  message(year)
  
}

fts <- rbindlist(fts_curated, use.names=T)
fts <- fts[as.character(year) >= 2017]

# Load in project data
project_metadata = fread("projects/cash_projects.csv")
project_metadata$project_id = as.character(project_metadata$project_id)
project_text = fread("projects/project_text.csv")
project_text$text = paste(project_text$project_name, project_text$project_objective)
project_text[,c("project_name", "project_objective")] = NULL
project_data = merge(project_text, project_metadata, all=T)
names(project_data) = c("destinationObjects_Project.id", "project_text", "project_cva_percentage", "project_cva")
setdiff(unique(fts$destinationObjects_Project.id), unique(project_data$destinationObjects_Project.id))

## Maybe add in keep function to have only required columns
fts$destinationObjects_Project.id = as.character(fts$destinationObjects_Project.id)
fts = merge(fts, project_data, by="destinationObjects_Project.id", all.x=T)

fts$all_text = paste(fts$description, fts$project_text)

#keywords are not case sensitive
cash.noncase.keywords <- c(
  "cash",
  "voucher",
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

#acronyms are case-sensitive
cash.acronyms <- c(
  "CCT",
  "UCT",
  "CTP",
  "CFW",
  "CFA",
  "SSN",
  "ESSN",
  "MPC",
  "MPCT",
  "CVA"
)

cash_regex = paste0(
  "\\b",
  paste(c(tolower(cash.noncase.keywords), tolower(cash.acronyms)), collapse="\\b|\\b"),
  "\\b"
)

##Relevant clusters from cluster mapping
cash_clusters <- c(
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

fts$sector_method_cluster_relevance <- "None"

## Define relevance based on sector and/or method
fts[method == "Cash transfer programming (CTP)", sector_method_cluster_relevance := "Full"]
fts[destinationObjects_Cluster.name %in% cash_clusters, sector_method_cluster_relevance := "Full"]

# Select partial sectors with cash cluster and
fts[grepl(";", destinationObjects_Cluster.name) == T & grepl(paste0(cash_clusters, collapse = "|"), destinationObjects_Cluster.name), sector_method_cluster_relevance := "Partial"]

#Count number of keywords appearing in description
fts$keyword_match = grepl(cash_regex, fts$all_text, ignore.case=T)
mean(fts$keyword_match > 0)
##below checks where relevance is none and there are or are not keywords
##second line below useful for identifying new keywords maybe missing
View(fts[sector_method_cluster_relevance == "None" & keyword_match][,"all_text"])
View(fts[sector_method_cluster_relevance != "None" & !keyword_match][,"all_text"])

# Start with sector/method/cluster relevance
fts$relevance = fts$sector_method_cluster_relevance
fts$relevance_method = "Sector/Method/Cluster"

# If percentage is greater than or equal to 0.75, mark full
fts$relevance_method[which(fts$project_cva_percentage >= 0.75)] = "Project CVA Percentage"
fts$relevance[which(fts$project_cva_percentage >= 0.75)] = "Full"

# If percentage is greater than 0 but less than 0.75, mark partial
fts$relevance_method[which(fts$project_cva_percentage > 0 & fts$project_cva_percentage < 0.75)] = "Project CVA Percentage"
fts$relevance[which(fts$project_cva_percentage > 0 & fts$project_cva_percentage < 0.75)] = "Partial"

# Save those that are either keyword match or marked at project level as CVA, but are None for ML
to_inference = subset(fts, (keyword_match | project_cva) & relevance == "None")
keep = c("id", "description")
to_inference = unique(to_inference[,keep,with=F])
setnames(to_inference, "description", "text")
fwrite(to_inference, "classifier_code/fts_to_inference.csv")

# Pause here to run Python inference code

# Load and join inference data
inference_output = fread("classifier_code/fts_to_inference_output.csv")
mean(inference_output$predicted_class=="Full")
hist(inference_output$predicted_confidence)
inference_output$text = NULL

fts = merge(fts, inference_output, by="id", all.x=T)

# Change Partial relevance by ML
fts$relevance_method[which(fts$keyword_match & fts$relevance == "None" & fts$predicted_class=="Partial")] = "Keyword + ML"
fts$relevance_method[which(fts$project_cva & fts$relevance == "None" & fts$predicted_class=="Partial")] = "Project API + ML"
fts$relevance[which((fts$keyword_match | fts$project_cva) & fts$relevance == "None" & fts$predicted_class=="Partial")] = "Partial"

# Change Full relevance by ML
fts$relevance_method[which(fts$keyword_match & fts$relevance == "None" & fts$predicted_class=="Full")] = "Keyword + ML"
fts$relevance_method[which(fts$project_cva & fts$relevance == "None" & fts$predicted_class=="Full")] = "Project API + ML"
fts$relevance[which((fts$keyword_match | fts$project_cva) & fts$relevance == "None" & fts$predicted_class=="Full")] = "Full"


fts_flagged <- fts[
  which(
    fts$relevance != "None"
  )
]

table(fts$relevance)
table(fts_flagged$relevance_method)

fwrite(fts_flagged, "output/fts_output_CVA.csv")

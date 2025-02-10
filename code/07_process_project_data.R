list.of.packages <- c("data.table", "jsonlite","tidyverse", "httr")
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

# Load and merge project-level data
project_list = list()
project_index = 1
for(year in 2017:2024){
  message(year)
  load(paste0("projects/project_data_",year,".RData"))
  project_list[[project_index]] = all_projects
  project_index = project_index + 1
}

all_projects <- rbindlist(
  project_list
)
questions <- unique(all_projects$question)
if(!dir.exists("output")){
  dir.create("output")
}
write.csv(questions, "output/questions.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

# Search for cash questions
cash.noncase.keywords <- c(
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
  "tokens",
  "coupons",
  "cupones",
  "transfert monétaire",
  "transfer monétaire",
  "transferencias monetarias",
  "public works programme",
  "social assistance",
  "social safety net",
  "social transfer",
  "social protection",
  "CVA",
  "CCT",
  "UCT",
  "CTP",
  "CFW",
  "CFA",
  "SSN",
  "ESSN",
  "MPC",
  "MPCT")

cash.noncase.keywords = paste0(
  "\\b",
  paste(cash.noncase.keywords, collapse="\\b|\\b"),
  "\\b"
)
potential_cash_questions <- questions[grepl(cash.noncase.keywords, questions, ignore.case=T)]

# Load pre-labeled
questions_labeled = fread("reference_datasets/cva_project_questions.csv")
new_potential_cash_questions = setdiff(potential_cash_questions, questions_labeled$Question)
if(length(new_potential_cash_questions) > 0){
  write.csv(questions, "output/potential_new_cash_questions.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)
}

quant_labeled_questions = subset(questions_labeled, `Question type` %in% c("quantC", "quantV"))$Question
flag_labeled_questions = subset(questions_labeled, `Question type` == "flagCVA")$Question

quant_cash_projects <- subset(all_projects, question %in% quant_labeled_questions)
boolean_cash_projects = subset(all_projects, question %in% flag_labeled_questions)

pattern <- "\\d+\\.\\d+|\\d+%|\\d+"
quant_cash_projects <- quant_cash_projects[grepl(pattern, answer)]

# Standardize answers
standardize_percentage <- function(x) {
  x <- trimws(tolower(x))
  if (grepl("%", x)) {
    num <- gsub(".*?(\\d+(\\.\\d+)?%).*", "\\1", x)  
    num <- gsub("%", "", num)  
  } else if (grepl("less than 1", x)) {
    num <- "0"
  } else if (grepl("percent", x)) {
    num <- gsub(".*?(\\d+(\\.\\d+)? percent).*", "\\1", x)  
    num <- gsub("percent", "", num)  
  } else if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
    num <- x
  } else {
    num <- gsub(".*?(\\d+(\\.\\d+)?%).*", "\\1", x)  
    if (num == "") {
      num <- NA
    } else {
      num <- gsub("%", "", num)  
    }
  }
    num <- gsub("[^0-9.]", "", num)
    num <- as.numeric(num)
    return(num)
}
quant_cash_projects <- quant_cash_projects[, standardized_percentage := sapply(answer, standardize_percentage)]

quant_cash_projects = quant_cash_projects[,.(cva_percentage = sum(standardized_percentage)), by=.(project_id)]
quant_cash_projects$cva_percentage[which(quant_cash_projects$cva_percentage > 100)] = 100
quant_cash_projects$cva_percentage = quant_cash_projects$cva_percentage / 100

standardize_boolean = function(x){
  if(tolower(x) %in% c("true", "qui", "yes")){
    return(T)
  }
  return(F)
}

boolean_cash_projects$boolean_answer = sapply(boolean_cash_projects$answer, standardize_boolean)

boolean_cash_projects = boolean_cash_projects[,.(cva=max(boolean_answer)==1), by=.(project_id)]

# Find and fix overlaps
zero_percents = subset(quant_cash_projects, cva_percentage == 0)
zero_to_bool = data.table(project_id = zero_percents$project_id, cva=F)
new_zero_ids = setdiff(zero_to_bool$project_id, boolean_cash_projects$project_id)
zero_to_bool = subset(zero_to_bool, project_id %in% new_zero_ids)
boolean_cash_projects = rbind(boolean_cash_projects, zero_to_bool)

false_bools = subset(boolean_cash_projects, !cva)
bool_to_zero = data.table(project_id = false_bools$project_id, cva_percentage=0)
new_bool_ids = setdiff(bool_to_zero$project_id, quant_cash_projects$project_id)
bool_to_zero = subset(bool_to_zero, project_id %in% new_bool_ids)
quant_cash_projects = rbind(quant_cash_projects, bool_to_zero)

cash_bool_and_percentage = merge(quant_cash_projects, boolean_cash_projects, all=T)
cash_bool_and_percentage$cva[which(cash_bool_and_percentage$cva_percentage > 0)] = T
cash_bool_and_percentage$cva[which(cash_bool_and_percentage$cva_percentage==0)] = F

fwrite(cash_bool_and_percentage, "projects/cash_projects.csv")

project_text = unique(all_projects[,c("project_id", "project_name", "project_objective")])
fwrite(project_text, "projects/project_text.csv")

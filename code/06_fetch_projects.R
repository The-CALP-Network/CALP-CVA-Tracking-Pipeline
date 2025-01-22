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

if(!dir.exists("projects")){
  dir.create("projects")
}

for(year in c(2017:2024)){
    message(year)
    if(!file.exists(paste0("projects/project_data_",year,".RData"))){
      base_path <- "fts/"
      filename = paste0(base_path, "fts_curated_", year, ".csv")
      fts <- fread(filename)
      
      unique_project_ids <- unique(fts$destinationObjects_Project.id)
      unique_project_ids <- unique_project_ids[complete.cases(unique_project_ids)]
      
      base_url = "https://api.hpc.tools/v2/public/project/"
      
      project_list <- list()
      project_index <- 1
      pb <- txtProgressBar(max = length(unique_project_ids), style = 3)
      for (i in 1: length(unique_project_ids)) {
        setTxtProgressBar(pb, i)
        project_id <- unique_project_ids[i]
        if(project_id == ""){
          next
        }
        project_url <- paste0(base_url, project_id)
        project_json <- fromJSON(project_url, simplifyVector = FALSE)
        
        
        
        project = project_json$data$projectVersion
        project_objective = ""
        if(!is.null(project$objective)){
          project_objective = project$objective
        }
        global_clusters_json = project$globalClusters
        global_clusters = c()
        for(global_cluster in global_clusters_json){
          global_clusters = c(global_clusters, global_cluster$name)
        }
        global_clusters_string = paste0(global_clusters, collapse=" | ")
        organisation_json = project$organizations
        organisation_ids = c()
        organisation_names = c()
        for(organisation in organisation_json){
          organisation_ids = c(organisation_ids, organisation$id)
          organisation_names = c(organisation_names, organisation$name)
        }
        organisation_ids_string = paste0(organisation_ids, collapse=" | ")
        organisation_names_string = paste0(organisation_names, collapse=" | ")
        field_definitions = list()
        for(def in project$plans[[1]]$conditionFields){
          field_definitions[[as.character(def$id)]] = def
        }
        
        field_values = project$projectVersionPlans[[1]]$projectVersionFields
        field_value_length = length(field_values)
        field_value_errors = 0
        if(field_value_length == 0){
          project_df = data.frame(
            "project_id" = project_id,
            "project_name" = project$name,
            "project_objective" = project_objective,
            "project_year" = year,
            "currently_requested_funds" = project$currentRequestedFunds,
            "plan_id" = project$plans[[1]]$planVersion$id,
            "plan_name" = project$plans[[1]]$planVersion$name,
            "global_clusters" = global_clusters_string,
            "organisation_ids" = organisation_ids_string,
            "organisation_names" = organisation_names_string,
            "question" = "No field questions",
            "answer" = "No field answers"
          )
          project_list[[project_index]] = project_df
          project_index = project_index + 1
        } else {
          for(field in field_values){
            def = field_definitions[[as.character(field$conditionFieldId)]]
            if(!is.null(def) & !is.null(field$value)){
              project_df = data.frame(
                "project_id" = project_id,
                "project_name" = project$name,
                "project_objective" = project_objective,
                "project_year" = year,
                "currently_requested_funds" = project$currentRequestedFunds,
                "plan_id" = project$plans[[1]]$planVersion$id,
                "plan_name" = project$plans[[1]]$planVersion$name,
                "global_clusters" = global_clusters_string,
                "organisation_ids" = organisation_ids_string,
                "organisation_names" = organisation_names_string,
                "question" = def$name,
                "answer" = field$value
              )
              project_list[[project_index]] = project_df
              project_index = project_index + 1
            } else {
              field_value_errors = field_value_errors + 1
            }
          }
        }
        # Non-zero length, but all of the fields are incorrectly referenced
        if(field_value_errors == field_value_length){
          project_df = data.frame(
            "project_id" = project_id,
            "project_name" = project$name,
            "project_objective" = project_objective,
            "project_year" = year,
            "currently_requested_funds" = project$currentRequestedFunds,
            "plan_id" = project$plans[[1]]$planVersion$id,
            "plan_name" = project$plans[[1]]$planVersion$name,
            "global_clusters" = global_clusters_string,
            "organisation_ids" = organisation_ids_string,
            "organisation_names" = organisation_names_string,
            "question" = "No field questions",
            "answer" = "No field answers"
          )
          project_list[[project_index]] = project_df
          project_index = project_index + 1
        }
      }
      
      close(pb)
      all_projects <- rbindlist(project_list)
      save(all_projects, file=paste0("projects/project_data_",year,".RData"))
    }
}



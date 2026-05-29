source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

lapply(c("code/util/util_deflators.R", "code/util/util_fts_curated_flows.R"), source)

fts_save_master <- function(years = 2018:2025, update_years = NA, base_year = 2024, path = "fts/"){
  fts_all <- fts_curated_flows(years, update_years = update_years, dataset_path = path)
  for(i in 1:length(years)){
    fwrite(fts_all[year == years[[i]]], paste0(path, "fts_curated_", years[[i]], ".csv"))
  }
}

fts_save_master()

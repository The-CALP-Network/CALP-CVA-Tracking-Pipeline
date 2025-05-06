### Function to download and curate FTS flows from the FTS API
# Queries are done by year, meaning the boundary column reflects this. We prioritise flows with incoming boundary classification where a flow is duplicated.

## Data which is removed
# Outgoing flows are removed.
# Duplicate flows which occur in multiple years are removed - the first occurrence is retained and then split equally between destination usage years.
# Pledges are removed.

## Transformations
# Multi-year flows (destination) are split equally between destination years.
# Deflation is done according to source organisation and destination year. Non-government source organisations use the OECD DAC deflator.
# Flows with multiple destinations are rendered 'Multi-recipient'.
# Organisation types (channels) are as given by FTS's API, except in a few limited cases where government agencies are manually reclassified as such.

list.of.packages <- c("tidyverse", "data.table", "jsonlite", "rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=TRUE))

getCurrentFileLocation <- function() {
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = 'right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

fts_curated_flows <- function(years = 2017:2024, update_years = NA, dataset_path = "fts", base_year = 2022, weo_ver = NULL) {
  
  code_dir <- getCurrentFileLocation()
  source(paste(code_dir, "01_fts_get_flows.R", sep="/"))
  source(paste(code_dir, "02_fts_split_rows.R", sep="/"))
  source(paste(code_dir, "03_deflators.R", sep="/"))
  
  if(!dir.exists(dataset_path)){
    dir.create(dataset_path)
  }
  
  fts_files <- list.files(path = dataset_path, pattern = "fts_")
  fts_list <- list()
  
  for(i in 1:length(years)){
    run <- TRUE
    if(!(paste0("fts_", years[i], ".csv") %in% fts_files) | years[i] %in% update_years){
      message(paste0("Downloading ", years[i]))
      while(run){
        tryCatch({
          fts <- fts_get_flows(year = years[i])
          run <- FALSE
        }, error = function(e) e)
        break
      }
      
      if (!is.null(fts$reportDetails) && length(fts$reportDetails) > 0) {
        reportDetails <- rbindlist(
          lapply(fts$reportDetails, function(x) lapply(x, function(y) paste0(y, collapse = "; "))),
          fill = TRUE
        )
        if (ncol(reportDetails) > 0) {
          setnames(reportDetails, paste0("reportDetails_", names(reportDetails)))
          fts <- cbind(fts, reportDetails)
        }
      }
      
      fts <- fts[, lapply(.SD, function(x) {
        if (is.character(x)) {
          x[x == "NULL"] <- NA
        }
        return(x)
      })]
      
      fwrite(fts, paste0(dataset_path, "/fts_", years[i], ".csv"))
    }
    
    message(paste0("Reading ", years[i]))
    fts_list[[i]] <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"), encoding = "UTF-8")
  }
  
  fts <- rbindlist(fts_list, use.names = TRUE, fill = TRUE)
  rm(fts_list)
  
  message("Curating data...")
  
  col_order <- names(fts)
  
  fts <- fts[boundary != "outgoing"]
  
  shared <- rbind(
    fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id],
    fts[onBoundary == "shared" & boundary == "internal" & !(id %in% fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id]$id), .SD[1], by = id]
  )
  fts <- rbind(fts[onBoundary != "shared"], shared)
  
  fts[, year := destinationObjects_UsageYear.name]
  fts[, multiyear := grepl(";", destinationObjects_UsageYear.name)]
  fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = TRUE)
  
  fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "destinationObjects_Location.name", split.pattern = "; ", remove.unsplit = TRUE)
  
  isos <- fread("reference_datasets/isos.csv", encoding = "UTF-8", showProgress = FALSE)
  fts <- merge(fts, isos[, .(countryname_fts, destination_org_iso3 = iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = TRUE, sort = FALSE)
  fts[, destination_org_country := destinationObjects_Location.name]
  fts[grepl(";", destination_org_country), `:=` (destination_org_country = "Multi-destination_org_country", destination_org_iso3 = "MULTI")]
  
  fts_orgs_raw <- fromJSON("https://api.hpc.tools/v1/public/organization")$data
  fts_locs_raw <- fromJSON("https://api.hpc.tools/v1/public/location")$data
  fts_orgs <- data.table(fts_orgs_raw)
  fts_locs <- data.table(fts_locs_raw)
  
  fts_orgs[, source_org_type := sapply(categories, function(x) {
    if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$name)) x[[1]]$name else NA
  })]
  
  fts_orgs[, source_org_country := sapply(locations, function(x) {
    if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$name)) x[[1]]$name else NA
  })]
  
  fts_orgs[, source_org_country_id := sapply(locations, function(x) {
    if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$id)) x[[1]]$id else NA
  })]
  
  fts_orgs[, source_org_country_id := as.character(source_org_country_id)]
  fts_locs[, id := as.character(id)]
  
  fts_orgs <- merge(
    fts_orgs,
    fts_locs[, .(id, iso3)],
    by.x = "source_org_country_id",
    by.y = "id",
    all.x = TRUE,
    sort = FALSE
  )
  
  fts_orgs <- fts_orgs[, .(
    sourceObjects_Organization.id = as.character(id),
    source_org_country,
    source_org_iso3 = iso3,
    FTS_source_orgtype = source_org_type
  )]
  
  message("🧹 Merging organization metadata into FTS...")
  message("🔍 Available columns in fts_orgs: ", paste(names(fts_orgs), collapse = ", "))
  message("🔍 Preview of sourceObjects_Organization.id in FTS: ", paste(unique(fts$sourceObjects_Organization.id)[1:5], collapse = ", "))
  
  fts[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  fts_orgs[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  
  if (!"source_org_iso3" %in% names(fts)) {
    fts <- merge(fts, fts_orgs, by = "sourceObjects_Organization.id", all.x = TRUE, sort = FALSE)
  }
  
  if (!"source_org_iso3" %in% names(fts)) {
    stop("❌ 'source_org_iso3' column is missing after merge with fts_orgs.")
  } else {
    message("✅ 'source_org_iso3' successfully added to FTS after merge.")
  }
  
  if(!file.exists("reference_datasets/deflators.csv")){
    deflators <- get_deflators(base_year = base_year, currency = "USD", weo_ver = weo_ver, approximate_missing = TRUE)
    fwrite(deflators, "reference_datasets/deflators.csv")
  } else {
    deflators <- fread("reference_datasets/deflators.csv")
  }
  
  deflators <- deflators[, .(source_org_iso3 = ISO, year = as.character(year), deflator = gdp_defl)]
  
  missing_cols <- setdiff(c("source_org_iso3", "year"), names(fts))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s) in FTS for merging: ", paste(missing_cols, collapse = ", "))
  }
  
  fts <- merge(fts, deflators, by = c("source_org_iso3", "year"), all.x = TRUE, sort = FALSE)
  
  fts_missing_deflator <- merge(
    fts[is.na(deflator)],
    deflators[source_org_iso3 == "DAC"],
    by = "year",
    all.x = TRUE,
    suffixes = c("", ".dac")
  )
  fts[is.na(deflator), deflator := fts_missing_deflator$deflator.dac]
  
  fts[, `:=` (amountUSD_defl = amountUSD / deflator, amountUSD_defl_millions = (amountUSD / deflator) / 1000000)]
  
  col_order <- union(col_order, names(fts)[order(names(fts))])
  fts <- fts[, col_order, with = FALSE]
  
  return(fts)
}

# Tracking humanitarian cash and voucher assistance

# Overview and purpose

This methodology and step-by-step guide outlines the process formerly followed by Development Initiatives (DI) and the CALP Network to produce a best possible estimate on global financial volumes of humanitarian cash and voucher assistance. It explains the technical steps alongside their reasoning from data extraction to identifying CVA data, combining it from different datasets and finally analysing the output.

The guide is intended to capture the methodological knowledge and methods used by former DI staff, following the insolvency of DI, and thereby ensure that the work can be adapted and continued if the demand for it continues to exist. It therefore also highlights the different aspects of the methodology that need to be reviewed on an ongoing basis and concludes with suggestions for further improvement.

## A note on this version of the guide

Since the methodology described in this guide was first implemented, the codebase has been substantially restructured and several bugs have been fixed. The code blocks throughout this guide have been updated to match the current repository state. Key changes:

- **Script reorganisation.** The original numbered scripts `01_fts_get_flows.R` through `04_fts_curated_flows.R` are now utility functions under `code/util/` (`util_fts_get_flows.R`, `util_fts_split_rows.R`, `util_deflators.R`, `util_fts_curated_flows.R`), sourced by the numbered pipeline scripts rather than run standalone. The original `08_fts_keyword_searching_cash.R` has been split into `08a_fts_prepare_for_inference.R` (everything before the ML step) and `08b_fts_combine_inference.R` (everything after), making the pause for the Python classifier an explicit script boundary rather than a mid-file comment.
- **Deflators are now precomputed.** `util_deflators.R` is run separately (not on every pipeline execution) and its output is saved to `deflators_2024USD.csv`, which `util_fts_curated_flows.R` reads directly. It also now blends IMF WEO-derived deflators with OECD DAC deflator data, preferring the latter for DAC donors and the DAC aggregate where available.
- **The FTS curation step has grown substantially.** Beyond the original deduplication, multi-year splitting, and deflation logic, `util_fts_curated_flows.R` now also applies standardised Development Initiatives (DI) organisation-channel and cluster coding, flags domestic response/new-to-country/new-to-plan/new-to-sector/COVID-related flows, adds dummy reverse flows to net out intra-country transfers, and excludes a manually reviewed list of erroneous flow IDs.
- **Two classification bugs were fixed in the CVA relevance step** (now in `08a`): a flow explicitly reported with `method == "Cash transfer programming (CTP)"` could previously be downgraded from `Full` to `Partial` by the multi-cluster rule if it ran after the CTP rule; and the `relevance` label could diverge from the `sector_method_cluster_relevance` value used to actually calculate `CVAamount`, because project-percentage overrides were applied unconditionally rather than only to flows not already classified by sector/cluster. Both are now fixed by re-ordering rules so CTP is applied last and project-percentage overrides only ever apply to flows still marked `"None"`.
- **The ML classifier now runs locally.** `flow_inference.py` loads a locally saved fine-tuned model rather than pulling from the Hugging Face Hub, and `train_flow_classifier_weighted.py` saves its best checkpoint to disk rather than pushing to the Hub. No Hugging Face account is required to run the pipeline.
- **A new CVA amount step was added** for flows where the ML classifier is confidently predicting `Partial` (confidence ≤ 0.2 for the `Full` class) and a common CVA keyword is present: these are now assigned a CVA amount based on the average observed Partial-to-total ratio elsewhere in the dataset, rather than being routed straight to manual review.
- **Manual review incorporation moved to the global analysis step.** Loading and validating `output/cva_manually_classified.csv` now happens at the start of `10_global_cva_analysis.R` rather than at the end of `09_calculate_cva.R`. This step now also validates the file's schema and values, warns about IDs that don't match the current review queue, and automatically clears resolved IDs from `output/cva_to_manually_classify.csv`.
- **The historical decisions reference file was renamed** from `Mike_cva_decisions.csv` to `reference_datasets/historical_cva_decisions.csv`.
- **Organisation aggregation now distinguishes locality.** `10_global_cva_analysis.R` adds a `Local_type` field (national/local vs. international) alongside the existing `Org_type`, and PC/TV figures are now imputed bidirectionally (previously only TV was derived from PC; now PC is also derived from TV where only the latter was reported).

# Data extraction

There are two different groups of CVA data sources included in this guide. Those required to calculate a global estimate on the financial volume of humanitarian CVA (i.e., the survey data, FTS and projects module), and those that contain CVA data that is or could in future be relevant to other CVA analyses (i.e., IATI or WFP CASHboard).

## CVA survey

In recent years, at least 90% of the total value of the global CVA estimate was based on data collected via surveys directly from agencies implementing/delivering humanitarian CVA that are members of the CALP Network. The survey was largely modelled after the [minimum agreements on tracking CVA](https://www.calpnetwork.org/publication/tracking-cash-and-voucher-assistance-agreements-recommendations-and-minimum-requirements-from-the-grand-bargain-cash-workstream/) endorsed by the Grand Bargain cash workstream to collect the required data with a minimal reporting burden. Data is usually collected on the most recently completed financial year from each implementing agency, except for agencies with missing data, for whom multiple years of data might be requested. The survey structure has varied slightly year-on-year but the usual data categories are:

| Data field | Explanation |
| :---- | :---- |
| Organisation name |  |
| Organisation type | Select from: International NGO; Local/national NGO; UN agency; Red Cross and Red Crescent Movement; Private sector; Other |
| Currency | The currency used for all financial values in the survey. |
| Overall cash & voucher programming costs | If the respondent does not have the specific value of overall programming cost, then they are asked to provide an estimation in the 'Comments' of its size relative to what is transferred to the recipients (for example: "An average of around 80% of the CVA programme budgets are transferred to beneficiaries") .  |
| Transfers to recipients | The value of CVA transfers to recipients. |
| Cash assistance | The value of transfers to recipients as cash. |
| Vouchers | The value of transfers to recipients as vouchers. |
| Value of sub-grants received for CVA | The value captured under other provided data on CVA in the survey that was received as sub-grant from another implementing agency. |
| Value of sub-grants provided for CVA | The value captured under other provided data on CVA in the survey that was provided as sub-grant to another implementing agency. |
| Comments | Any relevant caveats to or comments on the provided data, including on possible reasons for increases or decreases in volumes. We also request the breakdown of provided or received sub-grants for CVA so that we can avoid double-counting across different survey respondents. |

The main difference between the minimum agreements on tracking CVA and our survey is the inclusion of sub-grant data. This is because it is common practice for a large recipient of funding for CVA from government donors (e.g., WFP, or UNHCR) to then sub-grant all or aspects of the delivery of that CVA to recipients to another implementing agency. Given we request data from those large actors as well as smaller agencies that receive funding from them, we need the sub-grant data to ensure that there is no double-counting across survey data from both ([see below](#global-estimated-volumes-of-humanitarian-cva)). Sub-grant data also provides on means of being able to track funding for CVA that is implemented by local and national organisations.


## Financial Tracking Service

The [Financial Tracking Service](https://fts.unocha.org/) (FTS) by UN OCHA is the most comprehensive source of global humanitarian financing flows in close to real time. It was originally set up to track progress on the funding requirements of UN-coordinated humanitarian response plans, but has since expanded its ambition to track all international humanitarian funding flows whether in- or outside of those plans.

The central data element of the FTS data structure is the financial flow between organisations. These financial flows have characteristics assigned to them on their source and destination (e.g., organisation, location, cluster, or year), and in addition have a set of characteristics that are central to that flow (e.g., the flow status on whether it is pledged, committed or paid, whether it is a financial flow or in-kind support, or what aid modality the financial flow supports). Some of those characteristics are more comprehensively reported on than others.

Before identifying the FTS data that is relevant to CVA, we first have to extract the financial flows data from FTS. 

The first step of that process is to retrieve the flow data for the year of interest from the FTS API. The request to the API can also be tailored for specific plans, emergencies, global clusters, recipient countries or more. This logic lives in `code/util/util_fts_get_flows.R` (previously a standalone `01_fts_get_flows.R`) and is sourced by the curation function described below rather than run directly:
<details>

<summary>code/util/util_fts_get_flows.R</summary>

```R
fts_get_flows <- function(year = NULL, planid = NULL, emergencyid = NULL, globalclusterid = NULL, destinationlocationid = NULL, unnest = T){
lapply(c("data.table", "jsonlite", "httr"), require, character.only=T)
if(!is.null(year)){
    year <- paste0("year=", paste0(year, collapse=","))
}
if(!is.null(planid)){
    planid <- paste0("planid=", paste0(planid, collapse=","))
}
if(!is.null(emergencyid)){
    emergencyid <- paste0("emergencyid=", paste0(emergencyid, collapse=","))
}
if(!is.null(globalclusterid)){
    globalclusterid <- paste0("globalclusterid=", paste0(globalclusterid, collapse=","))
}
if(!is.null(destinationlocationid)){
    destinationlocationid <- paste0("destinationlocationid:", paste0(destinationlocationid, collapse=","))
}

call.filter <- NULL
if(!is.null(destinationlocationid)){
    call.filter <- paste0("&filterby=", destinationlocationid)
}

hpc <- "https://api.hpc.tools/v1/public/fts/flow?"
call.param <- paste(year, planid, emergencyid, globalclusterid, call.filter, "format=json&limit=1000", sep="&")
call <- paste0(hpc, call.param)
fts <- fromJSON(content(GET(call), type = "text", encoding = "UTF-8"), flatten = T)

flowslist <- list()
flowslist[[1]] <- (fts$data$flows)
i <- 2
while (!is.null(fts$meta$nextLink)){
    nextLink <- fts$meta$nextLink
    fts <- fromJSON(content(GET(nextLink), type = "text", encoding = "UTF-8"), flatten = T)
    flowslist[[i]] <- (fts$data$flows)
    i <- i + 1
}

flows <- rbindlist(flowslist, fill=T, use.names = T)

if(unnest){
    message("Un-nesting output. This may take some time.")
    fts_unnest_flows <- function(fts, cols = c("sourceObjects", "destinationObjects"), splits = "type", remove.nested = T, group.same = T){
    require(data.table)
    if(length(cols) != length(splits) & length(splits) != 1) stop("There must be one split for each nested col, or a single common split for all nested cols.", call.=F)
    fts <- as.data.table(fts)
    expand.splits <- data.table(cols = cols, splits = splits)
    for(i in 1:nrow(expand.splits)){
        col <- expand.splits[i]$cols
        split <- expand.splits[i]$splits
        if(group.same){
        expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(t(unlist(split(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")), as.data.table(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")))[, ..split]))))), fill=T)
        } else {
        expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(unlist(split(x, as.data.table(x)[, ..split])))), fill=T)
        }
        names(expanded) <- paste(col, names(expanded), sep="_")
        split.cols <- unique(names(expanded)[grepl(paste0("[.]", split, "\\d*$"), names(expanded))])
        expanded[, (split.cols) := NULL]
        expanded[, (split.cols) := NULL]
        expanded <- expanded[,which(unlist(lapply(expanded, function(x)!(all(is.na(x))|all(is.null(x)))))),with=F]
        fts <- cbind(fts, expanded)
        if(remove.nested) fts[, (col) := NULL][]
    }
    return(fts)
    }
    
    flows <- fts_unnest_flows(flows)
    
}

return(flows)
}
```

</details>

The following function will be required to split FTS flows that run across different years by each year, assuming an even distribution over time (the distribution of these instances of multi-year funding across years may not be evenly distributed in practice, but this assumption is made in the absense of annualised data to be able to account for this funding in each year it spans across without double counting). This lives in `code/util/util_fts_split_rows.R`:

<details>

<summary>code/util/util_fts_split_rows.R</summary>

```R
fts_split_rows <- function(data, value.cols = "amountUSD", split.col = "destinationObjects_UsageYear.name", split.pattern = "; ", remove.unsplit = T){
  split.pattern <- trimws(split.pattern)
  temp <- data[, .(trimws(unlist(strsplit(as.character(get(split.col)), split.pattern))), as.numeric(get(value.cols))/(1+ifelse(is.na(get(split.col)), 0, nchar(get(split.col))-nchar(gsub(split.pattern, "", get(split.col)))))), by=list(rownames(data))]
  if(remove.unsplit){
    names(temp) <- c("rownames", split.col, value.cols)
    data[, (split.col) := NULL]
    data[, (value.cols) := NULL]
  } else {
    names(temp) <- c("rownames", paste0(split.col, ".split"), paste0(value.cols, ".split"))
  }
  data <- merge(data[, rownames := rownames(data)], temp, by = "rownames")
  data[, rownames := NULL]
  return(data)
}
```

</details>

The splitting of rows by year was necessary to deflate funding amounts by year later in the next step (for more detail on the reasoning behind deflating and possible applications [see below](#deflators)). Deflator calculation lives in `code/util/util_deflators.R` and is not automatically as part of every pipeline execution — its output is precomputed and saved to `deflators_2024USD.csv`, which the curation step below reads directly. The full deflator calculation code is shown in the [Deflators](#deflators) section further down this guide.

The curation function that ties the above steps together lives in `code/util/util_fts_curated_flows.R`. Since the original version of this guide was written, it has grown to also apply standardised organisation-channel and cluster coding, flag domestic/COVID/new-to-context flows, net out intra-country transfers, and exclude a small number of manually reviewed erroneous flow IDs. Each of these additions is explained inline in the code below:

<details>

<summary>code/util/util_fts_curated_flows.R</summary>

```R
###Function to download and curate FTS flows from the FTS API
#Queries are done by year, meaning the boundary column reflects this. We prioritise flows with incoming boundary classification where a flow is duplicated.

##Data which is removed
#Outgoing flows are removed.
#Duplicate flows which occur in multiple years are removed - the first occurrence is retained and then split equally between destination usage years.
#Pledges are removed.

##Transformations
#Negative dummy flows are added to remove the effect of internal plan flows, ensuring the total inputs and outputs are correct when aggregated.
#Multi-year flows (destination) are split equally between destination years.
#Deflation is done according to source organisation and destination year. Non-government source organisations use the OECD DAC deflator.
#Flows with multiple destinations are rendered 'Multi-recipient'.
#Organisation types (channels) are as given by FTS's API, except in a few limited cases where government agencies are manually reclassified as such.
#European Commission Institutions are coded manually as donor country "European Commission" and use the OECD EU Institutions deflator.
fts_curated_flows <- function(years = 2000:2029, update_years = NA, dataset_path = "Datasets/FTS - Full Dataset/Datasets", deflators_path = "reference_datasets/deflators_2024USD.csv", base_year = 2024, weo_ver = NULL, dummy_intra_country_flows = T){
  suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr"), require, character.only=T))
  
  #Load FTS utility functions and deflators
  lapply(c("code/util_fts_get_flows.R", "code/util_fts_split_rows.R"), source)
  
  if(!dir.exists(dataset_path)){
    dir.create(dataset_path)
  }
  fts_files <- list.files(path = dataset_path, pattern = "fts_")
  fts_list <- list()
  for(i in 1:length(years)){
    run <- T
    if(!(paste0("fts_", years[i], ".csv") %in% fts_files) | years[i] %in% update_years){
      message(paste0("Downloading ", years[i]))
      while(run){
        tryCatch({
          fts <- fts_get_flows(year = years[i])
          run <- F
        },
        error = function(e) e
        )
        break
      }
      reportDetails <- rbindlist(lapply(fts$reportDetails, function(x) lapply(x, function(y) paste0(y, collapse = "; "))))
      names(reportDetails) <- paste0("reportDetails_", names(reportDetails))
      fts <- cbind(fts, reportDetails)
      fts[, reportDetails := NULL]
      fts[is.null(fts) | fts == "NULL"] <- NA
      fwrite(fts, paste0(dataset_path, "/fts_", years[i], ".csv"))
    }
    message(paste0("Reading ", years[i]))
    fts <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"), encoding = "UTF-8")
    fts[, reportDetails_date := NULL]
    fts_list[[i]] <- fts
    
  }
  
  fts <- rbindlist(fts_list, use.names = T, fill = T)
  rm(fts_list)
  
  #Begin transformation
  message("Curating data...")
  
  #Retain column order
  col_order <- names(fts)
  
  #Remove flows which are outgoing on boundary
  fts <- fts[boundary != "outgoing"]
  
  #Remove duplicates which have a shared boundary, and preserve 'incoming' over 'internal' on boundary type
  shared <- rbind(fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id], fts[onBoundary == "shared" & boundary == "internal" & !(id %in% fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id]$id), .SD[1], by = id])
  fts <- rbind(fts[onBoundary != "shared"], shared)
  
  #Split rows into individual years by destination usage year where multiple are recorded 
  fts[, year := destinationObjects_UsageYear.name]
  fts[, multiyear := grepl(";", destinationObjects_UsageYear.name)]
  fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)
  
  #Set multi-country flows to 'multi-destination_org_country' in destination_org_country column
  isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", showProgress = F)
  isos[iso3 == "COD", countryname_fts := "Democratic Republic of the Congo"]
  isos[iso3 == "PSE", countryname_fts := "Occupied Palestinian Territory"]
  fts <- merge(fts, isos[, .(countryname_fts, destination_org_iso3 = iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T, sort = F)
  fts[, destination_org_country := destinationObjects_Location.name]
  fts[grepl(";", destination_org_country), `:=` (destination_org_country = "Multi-destination_org_country", destination_org_iso3 = "MULTI")]
  
  #Deflate by source location and destination year
  fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
  fts_locs <- data.table(fromJSON("https://api.hpc.tools/v1/public/location")$data)
  fts_orgs[, `:=` (source_org_type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), source_org_country = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name), source_org_country_id = ifelse(is.null(locations[[1]]$id), NA, locations[[1]]$id)), by = id]
  fts_orgs <- merge(fts_orgs, fts_locs[, .(id, iso3)], by.x = "source_org_country_id", by.y = "id", all.x = T, sort = F)
  fts_orgs <- fts_orgs[, .(sourceObjects_Organization.id = as.character(id), source_org_country, source_org_iso3 = iso3, FTS_source_orgtype = source_org_type)]
  
  #Merge DI coded org types
  source_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/source_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  source_org_dicode <- merge(fts_orgs, source_org_dicode[, .(sourceObjects_Organization.id = as.character(sourceObjects_Organization.id), source_orgtype, source_privatemoney)], by = "sourceObjects_Organization.id", all.x = T)
  
  destination_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/destination_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  
  #Merge source orgs
  fts[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  fts <- merge(fts, source_org_dicode, by = "sourceObjects_Organization.id", all.x = T, sort = F)
  fts[!(FTS_source_orgtype == "Governments" | (source_orgtype %in% c("DAC governments", "NDD"))) | is.na(FTS_source_orgtype) | is.na(source_org_country), `:=` (source_org_country = "Total DAC", source_org_iso3 = "DAC")]
  fts[, FTS_source_orgtype := NULL]
  
  #Manual EU institution classifications
  euc_id <- c("8523","2966","8524","6789","2176","8525","8556","8650","8541","8421", "12078", "6936", "12609", "13154")
  fts[sourceObjects_Organization.id %in% euc_id, `:=` (source_org_country = "European Commission", source_org_iso3 = "EUI")]
  
  #Merge dest orgs
  fts <- merge(fts, destination_org_dicode[!is.na(destinationObjects_Organization.id), .(destinationObjects_Organization.id = as.character(destinationObjects_Organization.id), destination_orgtype, destination_ngotype, destination_deliverychannel)], by = "destinationObjects_Organization.id", all.x = T, sort = F)
  
  #Fill gaps in DI org coding with FTS
  fts[is.na(source_orgtype) | source_orgtype == "", source_orgtype := gsub("\\bNGO\\b", "NGOs", sourceObjects_Organization.organizationTypes)]
  fts[, source_orgtype := gsub("\\bUN agency\\b", "UN Multi", source_orgtype)]
  
  fts[is.na(source_privatemoney) | source_privatemoney == "", source_privatemoney := ifelse(sourceObjects_Organization.organizationTypes == "Private organization/foundation", "private", "no")]
  fts[is.na(destination_orgtype) | destination_orgtype == "", destination_orgtype := gsub("\\bNGO\\b", "NGOs", destinationObjects_Organization.organizationTypes)]
  fts[, destination_orgtype := gsub("UN agency", "UN Multi", destination_orgtype)]
  
  fts[(is.na(destination_ngotype) | destination_ngotype == "") & destinationObjects_Organization.organizationTypes == "NGOs", destination_ngotype := paste0(gsub(" NGO| organization/foundation/individual|s/CSOs", "", destinationObjects_Organization.organizationSubTypes), " NGO")]
  fts[, destination_ngotype := gsub("^Affiliated", "Internationally Affiliated", destination_ngotype)]
  fts[is.na(destination_ngotype) & destinationObjects_Organization.organizationTypes == "NGOs", destination_ngotype := "Undefined NGO"]
  
  #Merge GHA channels
  gha_channels <- setnames(
    data.table(t(data.table(
      c("NGOs and CSOs", "NGO"),
      c("UN Multi", "UN agency"),
      c("UN Multi", "Pooled fund"),
      c("Public Sector", "Government"),
      c("Public Sector", "Inter-governmental"),
      c("RCRC", "Red Cross/Red Crescent"),
      c("Other", "Private organization/foundation"),
      c("Other", "Other"),
      c("Other multi", "Financial institution"),
      c("Uncategorized", "Uncategorized"),
      c("Multi-channel", "Multi-channel")
    )
    )
    ), c("FTS_matched_gha_channel", "destinationObjects_Organization.organizationTypes"))
  fts <- merge(fts, gha_channels, by = "destinationObjects_Organization.organizationTypes", all.x = T, sort = F)
  
  fts[is.na(destination_deliverychannel) | destination_deliverychannel == "", destination_deliverychannel := FTS_matched_gha_channel]
  fts[, FTS_matched_gha_channel := NULL]
  
  #Aggregate multiple matched columns
  fts[grepl(";", source_orgtype), source_orgtype := ifelse(length(unique(strsplit(source_orgtype, "; ")[[1]])) == 1, unique(strsplit(source_orgtype, "; ")[[1]]), "Other")]
  fts[grepl(";", destination_orgtype), destination_orgtype := ifelse(length(unique(strsplit(destination_orgtype, "; ")[[1]])) == 1, unique(strsplit(destination_orgtype, "; ")[[1]]), "Other") ]
  fts[grepl(";", destination_ngotype), destination_ngotype := ifelse(length(unique(strsplit(destination_ngotype, "; ")[[1]])) == 1, unique(strsplit(destination_ngotype, "; ")[[1]]), "Other") ]
  
  #Merge DI coded clusters
  cluster_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/cluster_mapping_DIcode.csv", showProgress = F)
  
  fts[, upper_cluster := toupper((sourceObjects_Cluster.name))]
  fts <- merge(fts, unique(cluster_dicode[, .(upper_cluster, source_globalcluster = destination_globalcluster)]), by = "upper_cluster", all.x = T, sort = F)
  fts[, upper_cluster := NULL]
  
  fts[, upper_cluster := toupper((destinationObjects_Cluster.name))]
  fts <- merge(fts, unique(cluster_dicode[, .(upper_cluster, destination_globalcluster)]), by = "upper_cluster", all.x = T, sort = F)
  fts[, upper_cluster := NULL]
  
  #Overwrite DI coding with FTS global cluster where exists
  fts[!is.na(destinationObjects_GlobalCluster.name) & destinationObjects_GlobalCluster.name != "" & is.na(destination_globalcluster), destination_globalcluster := destinationObjects_GlobalCluster.name]
  fts[!is.na(sourceObjects_GlobalCluster.name) & sourceObjects_GlobalCluster.name != "" & is.na(source_globalcluster), source_globalcluster := sourceObjects_GlobalCluster.name]
  
  #Identify multi-cluster flows
  fts[grepl(";", destination_globalcluster) | (is.na(destination_globalcluster) & grepl(";", destinationObjects_Cluster.name)), destination_globalcluster := "Multiple clusters specified"]
  fts[grepl(";", source_globalcluster) | (is.na(source_globalcluster) & grepl(";", sourceObjects_Cluster.name)), source_globalcluster := "Multiple clusters specified"]
  
  #Identify unspecified cluster flows
  fts[is.na(destination_globalcluster) | destination_globalcluster == "", destination_globalcluster := "Unspecified"]
  fts[is.na(source_globalcluster) | source_globalcluster == "", source_globalcluster := "Unspecified"]
  
  #Domestic response
  fts[, domestic_response := F]
  fts[grepl("Government", sourceObjects_Organization.organizationTypes) & source_org_iso3 == destination_org_iso3, domestic_response := T]
  
  #New to country
  fts[, new_to_country := T]
  fts[sourceObjects_Location.id == destinationObjects_Location.id | destinationObjects_Location.id == "", new_to_country := F]

  #New to plan
  fts[, new_to_plan := T]
  if("sourceObjects_Plan.id" %in% names(fts)){
    fts[sourceObjects_Plan.id == destinationObjects_Plan.id | destinationObjects_Plan.id == "", new_to_plan := F]
  }
  
  #New to sector
  fts[, new_to_sector := T]
  fts[sourceObjects_GlobalCluster.id == destinationObjects_GlobalCluster.id | destinationObjects_GlobalCluster.id == "", new_to_sector := F]

  #COVID
  fts[, COVID := F]
  fts[grepl("COVID", paste0(destinationObjects_Cluster.name, destinationObjects_GlobalCluster.name, destinationObjects_Plan.name, destinationObjects_Emergency.name), ignore.case = T), COVID := T]
  
  #Deflate
  #deflators <- get_deflators(base_year = base_year, currency = "USD", weo_ver = weo_ver, approximate_missing = T)
  deflators <- fread(deflators_path)
  deflators <- deflators[, .(source_org_iso3 = ISO, year = as.character(year), deflator = gdp_defl)]
  
  fts <- merge(fts, deflators, by = c("source_org_iso3", "year"), all.x = T, sort = F)
  fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[source_org_iso3 == "DAC"], by = "year", all.x = T, sort = F)$deflator
  fts[, `:=` (amountUSD_defl = amountUSD/deflator, amountUSD_defl_millions = (amountUSD/deflator)/1000000)]
  
  #Add dummy reverse flows to cancel-out intra-country flows
  fts[, newMoney_dest := newMoney]
  fts[, dummy := F]
  if(dummy_intra_country_flows){
    fts[sourceObjects_Location.id == destinationObjects_Location.id, newMoney_dest := TRUE]
    fts_intracountry <- fts[sourceObjects_Location.id == destinationObjects_Location.id]
    source_cols <- grep("source", names(fts_intracountry))
    destination_cols <- grep("destination", names(fts_intracountry))
    names(fts_intracountry)[source_cols] <- gsub("source", "destination", names(fts_intracountry)[source_cols])
    names(fts_intracountry)[destination_cols] <- gsub("destination", "source", names(fts_intracountry)[destination_cols])
    fts_intracountry[, `:=` (amountUSD = -amountUSD, amountUSD_defl = -amountUSD_defl, amountUSD_defl_millions = -amountUSD_defl_millions, dummy = T, destination_privatemoney = NULL, source_deliverychannel = NULL, source_ngotype = NULL)]
    
    fts <- rbind(fts, fts_intracountry, fill = T)
  }
  
  #Add excluded flag based on LLM analysis
  fts[id %in% c(183809,271852,336815,228012,336814,259979,336813,274415,292845,337721,336423,339844,339845,340701,340730,361083,361234,365896,370591,370604,372528,375587,375588), excluded := T]
  
  #Remove partial multi-year flows outside of requested range and pledges 
  fts <- fts[
    year %in% years 
    & status %in% c("paid", "commitment")
    ]
  
  #Reorder columns nicely
  col_order <- union(col_order, names(fts)[order(names(fts))])
  fts <- fts[, col_order, with = F]

  return(fts)
}
```

</details>

The final bit of code relating to the FTS data creates a 'master' dataset of all the extracted FTS data across all years for further analysis, executing `fts_curated_flows()` and writing one CSV per year. Given FTS updates daily, it is recommended to regularly update the most recent and ongoing year (by specifying that year for `update_years`):

<details>

<summary>code/05_fts_curated_master.R</summary>

```R
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
```

</details>


## Projects module

The [Projects module](https://projects.hpc.tools/) by UN OCHA is part of the Humanitarian Programme Cycle and it facilitates the project submission, review and approval cycle in countries with project-based Humanitarian Response Plans. This is to ensure that the needs and populations that are planned to be addressed by different agencies in a specific context are reviewed by the respective clusters to enhance coordination and coherence across those projects. A large amount of data on each project is collected as part of this process, including data that is relevant to CVA.

It is important to recognise that this data reflects the planning stage and may not be updated retrospectively if project aspects change during the implementation. [Below](#global-estimated-volumes-of-humanitarian-cva), we will combine it with FTS data on funding flows to be able to identify how much funding went to projects that planned for the delivery of CVA.

The following script (`code/06_fetch_projects.R`) fetches project data for every unique project ID referenced in the curated FTS flows, caching results per year so re-runs only fetch newly seen projects:

<details>

<summary>code/06_fetch_projects.R</summary>

```R
source("code/util/utils.R")
enforce_project_root()
load_packages("data.table", "jsonlite", "httr")

if (!dir.exists("projects")) {
  dir.create("projects")
}

for (year in c(2018:2025)) {
  message(year)
  if (!file.exists(paste0("projects/project_data_", year, ".RData"))) {
    base_path <- "fts/"
    filename = paste0(base_path, "fts_curated_", year, ".csv")
    fts <- fread(filename)
    
    unique_project_ids <- unique(fts$destinationObjects_Project.id)
    unique_project_ids <- unlist(strsplit(as.character(unique_project_ids), "; "))
    unique_project_ids <- unique_project_ids[complete.cases(unique_project_ids)]
    
    base_url = "https://api.hpc.tools/v2/public/project/"
    
    project_list <- list()
    project_index <- 1
    pb <- txtProgressBar(max = length(unique_project_ids), style = 3)
    for (i in 1:length(unique_project_ids)) {
      setTxtProgressBar(pb, i)
      project_id <- unique_project_ids[i]
      if (project_id == "") {
        next
      }
      project_url <- paste0(base_url, project_id)
      project_json <- fromJSON(project_url, simplifyVector = FALSE)
      
      
      
      project = project_json$data$projectVersion
      project_objective = ""
      if (!is.null(project$objective)) {
        project_objective = project$objective
      }
      global_clusters_json = project$globalClusters
      global_clusters = c()
      for (global_cluster in global_clusters_json) {
        global_clusters = c(global_clusters, global_cluster$name)
      }
      global_clusters_string = paste0(global_clusters, collapse = " | ")
      organisation_json = project$organizations
      organisation_ids = c()
      organisation_names = c()
      for (organisation in organisation_json) {
        organisation_ids = c(organisation_ids, organisation$id)
        organisation_names = c(organisation_names, organisation$name)
      }
      organisation_ids_string = paste0(organisation_ids, collapse = " | ")
      organisation_names_string = paste0(organisation_names, collapse = " | ")
      field_definitions = list()
      for (def in project$plans[[1]]$conditionFields) {
        field_definitions[[as.character(def$id)]] = def
      }
      
      field_values = project$projectVersionPlans[[1]]$projectVersionFields
      field_value_length = length(field_values)
      field_value_errors = 0
      if (field_value_length == 0) {
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
        for (field in field_values) {
          def = field_definitions[[as.character(field$conditionFieldId)]]
          if (!is.null(def) & !is.null(field$value)) {
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
      if (field_value_errors == field_value_length) {
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
    save(all_projects,
         file = paste0("projects/project_data_", year, ".RData"))
  }
}
```

</details>

The notable change from earlier versions of this script is the addition of `unlist(strsplit(as.character(unique_project_ids), "; "))` when building the list of project IDs to fetch. FTS flows can record more than one project ID in a single semicolon-delimited field (e.g. a flow split across two projects); without this split, the script would attempt to fetch a single, invalid "project" with an ID like `"123; 456"` and silently fail for every such flow. The year range has also been extended to `2018:2025` to match the rest of the pipeline.


## International Aid Transparency Initiative

The [International Aid Transparency Initiative](https://iatistandard.org/en/) (IATI) provides a reporting standard for donors and implementing agencies to publish data relating to aid projects, budgets and transactions in close to real-time (or even forward-looking of planned activities or budgets). It originated in the development sector and many of the largest humanitarian donors and agencies publish some data to this standard.

A crucial difference to other aid reporting platforms like FTS or the OECD DAC Creditor Reporting System is that IATI is a data standard and not a database. This means that through the [IATI datastore](https://datastore.iatistandard.org/) it is possible to download open source aid data published by donors or implementers that meet certain search criteria, but it is up to the user on whether or how to aggregate this data to avoid double-counting. For instance, donors publish data on outgoing disbursements relating to their aid projects and implementers publish data on incoming commitments or disbursements, meaning that close attention needs to be paid when aggregating financial volumes to what type of transaction is used for what group of actors. In addition, the data quality varies significantly across different IATI publishers and sometimes even for the same publisher across different time periods. This means that data quality checks might be required before using the data for analysis or advocacy messaging.

In terms of CVA data, one critical advantage of the IATI standard is that it is designed to enable implementing agencies to publish data on their project expenditure. This is an advantage compared to FTS, which seeks to capture transactions between organisations, given the delivery of CVA represents project expenditure to the organisation providing it to the end recipients.

In 2019, the IATI standard [introduced](https://www.iaticonnect.org/group/standard-management-consultations-0/discussion/added-proposal-add-cash-transfer-and-voucher) the option for publishers using the 2.03 version (or later) of the standard to publish data on cash and voucher assistance by adding it as an optional aid type. This is possible for all their aid activities and not limited to humanitarian assistance. It allows for two ways of publishing CVA data to IATI:

1. As an aid type to an IATI activity, which would allow IATI publishers to flag whether an activity/project includes cash transfers, vouchers or neither.  
2. As an aid type assigned to IATI transactions, including project-related expenditure. This would allow IATI publishers to specify the financial value of any of their project expenditure as cash transfers or vouchers provided to recipients.

If agencies delivering humanitarian (or any other) CVA to recipients used the second way of publishing CVA data to IATI, this would provide the most accurate representation of transfers to recipients as cash or vouchers as part of routine reporting on the overall project characteristics (including total budgets, donors, cluster/sector, etc.). However, hardly any implementing agency currently uses this option of reporting to IATI on CVA in that way. Most current uses of this IATI codelist seem to represent misreporting, where this codelist was used to represent other forms of cash transfers (between organisations or other cash expenditure).

The easiest way to access and check this data would be through the [IATI datastore](https://datastore.iatistandard.org/) advanced search. See the screenshot below for the appropriate filters:

![IATI datastore advanced search filter](/assets/iati_datastore.png)

Running this search would yield all IATI activities where publishers have included either the CVA aid type as flag for the activity or a CVA aid type as characteristic of any transaction related to that activity.

Given the very few instances of intended applications of the cash and voucher modality codelist in IATI, there is not IATI data used in the current version of this methodology for quantifying humanitarian cash and voucher assistance. It is included in this guide given its potential for accurately tracking transfers to recipients should the uptake of transaction-level reporting of cash and voucher assistance by implementing agencices to IATI improve in future.

## WFP CASHboard Analytics

The World Food Programme (WFP) maintains its own online dashboard of what it describes as ‘cash-based transfers and commodity vouchers’ with data on WFP’s CVA operations from 2018 to, at the time of writing, 2024\. This crucially includes a breakdown of data by country, which is not evident from the global CVA data collected from WFP (and all other CALP Network members) via survey. It can therefore be useful to incorporate for analysis disaggregated by country, but is not currently used in the global calculation of humanitarian CVA volumes.

The [WFP CASHboard](https://unwfp.maps.arcgis.com/apps/dashboards/5e403a8944104b328117c67ae4afa11e) data can be extracted through the following lines of code:

```R
library(jsonlite)  
json_response = fromJSON("https://services3.arcgis.com/t6lYS2Pmd8iVx1fy/arcgis/rest/services/global_CBT_operations_by_country/FeatureServer/4/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=OBJECTID%20ASC&resultOffset=0&resultRecordCount=1000&cacheHint=true&quantizationParameters=%7B%22mode%22%3A%22edit%22%7D")  
wfp_cash_map_data = json_response$features$attributes
```

## Supplementary data

There are a few supplementary datasets that are not directly related to CVA but required to convert aspects of the financial data for better comparability.

### Exchange rates

All currency units tend to be converted to US$ to have a uniform currency across different financial amounts. There are a number of possible data sources to use for this conversion. In previous iterations of this methodology, DI chose to align the exchange rates for this analysis with that used in other parts of DI’s [Global Humanitarian Assistance Reports]([https://devinit.github.io/resources/falling-short-humanitarian-funding-reform/]) for consistent currency conversions across analyses. This might be relevant when trying to estimate the share of total international humanitarian assistance (IHA) made up by CVA ([see below](#relative-share-of-cva-as--of-iha)).

If following that approach, the exchanges used in different years to convert different currencies into US$ are primarily sourced from the OECD, and supplemented by the World Bank and then the IMF IFS for currencies missing from the OECD data (each source only fills gaps left by the higher-priority source). This logic lives in `code/util/util_exchange_rates.R` and queries the OECD's SDMX API directly rather than the older `imfr`-based approach:

<details>

<summary>code/util/util_exchange_rates.R</summary>

```R
source("code/utils.R")
enforce_project_root()
load_packages("data.table", "jsonlite", "httr", "imfapi")

years <- 1950:2025

isos <- fread("reference_datasets/isos.csv", encoding = "UTF-8", na.strings = "")

all_ex <- data.table(expand.grid(iso3 = c(isos$iso3, "EUI"), year = years))

##OECD
api_url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=1950&dimensionAtObservation=AllDimensions"

if (!file.exists("reference_datasets/oecd_ex.csv")) {
  res <- GET(
    api_url, 
    accept("application/vnd.sdmx.data+csv; charset=utf-8")
  )
  
  csv_content <- content(res, as = "text", encoding = "UTF-8")
  oecd_ex <- fread(text = csv_content)
  
  setnames(oecd_ex, 
           old = c("REF_AREA", "TIME_PERIOD", "OBS_VALUE"), 
           new = c("iso3", "year", "value"), 
           skip_absent = TRUE)
  
  fwrite(oecd_ex, "reference_datasets/oecd_ex.csv")
  
} else {
  oecd_ex <- fread("reference_datasets/oecd_ex.csv")
}

oecd_ex[iso3 == "EA20", iso3 := "EUI"]
oecd_ex <- oecd_ex[!is.na(value) & value != 0]

##WORLD BANK
if(!file.exists("reference_datasets/wb_ex.csv")){
  wb_ex <- data.table(fromJSON("https://api.worldbank.org/v2/country/all/indicator/PA.NUS.ATLS?date=1950:2025&format=json&per_page=20000")[[2]])
  fwrite(wb_ex, "reference_datasets/wb_ex.csv")
}else{
  wb_ex = fread("reference_datasets/wb_ex.csv")
}
wb_ex <- wb_ex[, .(iso3 = countryiso3code, year = date, value = value)]

wb_ex <- wb_ex[!is.na(value) & (!(paste0(iso3, year) %in% oecd_ex[, paste0(iso3, year)]))]

##IFS
if(!file.exists("reference_datasets/ifs_ex.csv")){
  ifs_ex <- data.table(imf_get(
    dataflow_id  = "ER",
    dimensions   = list(
      INDICATOR = "ECU_XDC",
      FREQUENCY = "A"
    )
  ))
  
  fwrite(ifs_ex, "reference_datasets/ifs_ex.csv")
} else {
  ifs_ex <- fread("reference_datasets/ifs_ex.csv")
}

setnames(ifs_ex, c("COUNTRY", "OBS_VALUE", "TIME_PERIOD"), c("iso3","value", "year"))
ifs_ex = ifs_ex[!is.na(iso3) & !is.na(year) & !is.na(value)]
ifs_ex <- ifs_ex[!is.na(value) & !(paste0(iso3, year) %in% c(oecd_ex[, paste0(iso3, year)], wb_ex[, paste0(iso3, year)]))]

##All
oecd_ex <- oecd_ex[,c("iso3", "year", "value")]
ifs_ex <- ifs_ex[,c("iso3", "year", "value")]

all_wd_ex <- rbind(oecd_ex, wb_ex, ifs_ex)[, .(iso3, year = as.integer(year), value)]

all_ex <- merge(all_ex, all_wd_ex, all.x = T)

fwrite(all_ex, "reference_datasets/usd_exchange_rates.csv")
```

</details>

Each source's results are cached locally (`reference_datasets/oecd_ex.csv`, `wb_ex.csv`, `ifs_ex.csv`) so the script only queries the live APIs the first time it's run, or after those cached files are deleted.

### Deflators

Currently, the global volumes of humanitarian CVA are presented in current prices, i.e., without adjusting for inflation/rising costs. The main reasons for this are that it would be a slightly arbitrary choice of which set of deflators to use for the implementing agencies’ data (the price level in donor countries or in recipient locations (if known)?) and that adjusting for inflation would require manipulating the implementers’ data so that they potentially do not recognise themselves in the trend anymore.

However, this means that the increase in global volumes of humanitarian CVA is likely inflated by increasing costs/price levels in both donor and recipient countries. It therefore does not represent an increase of x% across years (depending on the years of comparison) all else being equal.

For time-series analysis of financial data over a long time period, it can make sense to adjust financial data in each year for inflation to have better comparability over time. For example, in current prices, the total bilateral official development assistance from OECD DAC countries increased by 324% between 2002 and 2022, but when adjusting for inflation by deflating both to constant 2022 prices, this changes to an increase by only 189%.

Deflators are mostly relevant for this guide as consideration for future methodological adjustments ([see below](#suggestions-for-future-improvements)) and for calculating CVA as % of IHA, the latter previously calculated by DI in constant prices ([see below](#relative-share-of-cva-as--of-iha)). The deflator calculation lives in `code/util/util_deflators.R` and, since the original version of this guide, has been extended to blend two sources: IMF World Economic Outlook (WEO) GDP deflators (calculated from current- and constant-price GDP series) for most countries, and OECD DAC's own published deflator series — preferred over the WEO-derived figure wherever available — for DAC donor countries and the DAC aggregate specifically. This matters because the DAC deflator series is the one actually used to convert IHA figures back to current prices for the "CVA as % of IHA" calculation described later in this guide, so its accuracy is more consequential than the WEO-derived deflators used for other (non-DAC) source-organisation countries:

<details>

<summary>code/util/util_deflators.R</summary>

```R
source("code/util/utils.R")
load_packages("data.table", "httr", "jsonlite","lubridate", "readxl")

base_year = 2024
currency = "USD"
weo_ver = NULL
approximate_missing = T

if(is.null(weo_ver)){
  
  tyear <- year(Sys.Date())
  tmonth <- month(Sys.Date())
  
  weo_month <- ifelse(tmonth <= 10 & tmonth >= 4, 4, 10)
  weo_year <- ifelse(tmonth < 4, tyear-1, tyear)
  
  weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
}

##WEO data
pweo_ver <- as.Date(paste0("1", weo_ver), "%d%b%Y")
weo_year <- year(pweo_ver)
weo_month <- month(pweo_ver)
weo_month_text <- as.character(lubridate::month(pweo_ver,label = TRUE, abbr = FALSE))

while(T){
  url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year,"/",weo_month_text, "/WEO", weo_ver ,"all.xls")
  response <- GET(url)
  if(response$headers$`content-type` == "application/vnd.ms-excel") break
  
  if(weo_month <= 10 & weo_month > 4){
    weo_month <- 4
  } else {
    if(weo_month <= 4){
      weo_year <- weo_year - 1
    }
    weo_month <- 10
  }
  weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
  weo_month_text <- as.character(lubridate::month(weo_month,label = TRUE, abbr = FALSE))
  
}

message("Using IMF WEO version ", weo_ver, ".")

content <- rawToChar(response$content[content !='00'])
weo <- suppressWarnings(fread(text = content))

country_codes <- unique(weo[, .(ISO, Country)])

data_cols <- c("ISO", "WEO Subject Code", grep("^\\d{4}$", names(weo), value = T))

weo <- melt(weo[, ..data_cols], id.vars = c("ISO", "WEO Subject Code"), variable.factor = F)
weo[, value := as.numeric(gsub(",", "", value))]

#Fix PSE ISO code
weo[ISO == "WBG", ISO := "PSE"]

#GDP in current prices
if(currency == "USD"){
  weo_gdp_cur <- weo[`WEO Subject Code` == "NGDPD"]
}
if(currency == "LCU"){
  weo_gdp_cur <- weo[`WEO Subject Code` == "NGDP"]
}
if(currency == "PPP"){
  weo_gdp_cur <- weo[`WEO Subject Code` == "PPPGDP"]
}

weo_gdp_cur <- weo_gdp_cur[, .(ISO, variable, gdp_cur = value)]

#GDP real growth rates
weo_gdp_pcg <- weo[`WEO Subject Code` == "NGDP_RPCH"]

#GDP cumulative growth rates
weo_gdp_pcg <- weo_gdp_pcg[, gdp_cg := 1+ifelse(is.na(value), 0, value/100), by = ISO]
weo_gdp_pcg[, gdp_cg := ifelse(!(!is.na(value) | !is.na(shift(value, -1))), NA, cumprod(gdp_cg)), by = ISO]
weo_gdp_pcg[, gdp_cg := gdp_cg/gdp_cg[variable == base_year], by = ISO][, value := NULL]

#GDP in constant prices
weo_gdp_con <- merge(weo_gdp_pcg[, .(ISO, variable, gdp_cg)], weo_gdp_cur)
weo_gdp_con[, `:=` (gdp_con = gdp_cg*gdp_cur[variable == base_year]), by= ISO]

#GDP deflators from WEO
weo_deflators <- weo_gdp_con[, .(gdp_defl = gdp_cur/gdp_con), by = .(ISO, variable)]
weo_deflators <- cbind(weo_deflators, source = "WEO", ver = weo_ver)

##Other data sources
dacdefl <- data.table(read_excel("reference_datasets/Deflators-base-2024.xlsx", skip = 2))
setnames(dacdefl, "...66", "2024")
setnames(dacdefl, "...1", "name")
dacdefl[, `:=` (...67 = NULL, ...68 = NULL)]
dacdefl <- melt(dacdefl, id.vars = c("name"), value.name = "OBS_VALUE")

dacdefl <- dacdefl[complete.cases(dacdefl)]

dac1 <- fread("G:/My Drive/Work/Consultancies/2025/ALNAP/Analysis/Datasets/OECD DAC Table 1 and 2a/Table1_Data.csv", encoding = "UTF-8")
dac1 <- dac1[`Fund flows` == "Net Disbursements" & AIDTYPE == 1010 & AMOUNTTYPE %in% c("A", "D")]
dacdefl2 <- dac1[, .(OBS_VALUE = Value[AMOUNTTYPE == "A"]/Value[AMOUNTTYPE == "D"]), by = .(name = Donor, variable = Year)]
dacdefl2 <- dacdefl2[!(name %in% dacdefl$name)]

dacdefl <- rbind(dacdefl, dacdefl2)
dacdefl[, gdp_defl := OBS_VALUE/OBS_VALUE[variable == base_year], by = name]

dacdefl <- merge(dacdefl, country_codes, by.x = "name", by.y = "Country", all.x = T)

dacdefl[name == "TOTAL DAC", ISO := "DAC"]
dacdefl[name == "EU Institutions", ISO := "EUI"]
dacdefl[name == "Chinese Taipei", ISO := "TWN"]
dacdefl[name == "Czechia", ISO := "CZE"]
dacdefl[name == "Liechtenstein", ISO := "LIE"]
dacdefl[name == "Monaco", ISO := "MCO"]

if(nrow(dacdefl[is.na(ISO)]) > 0) warning("Country name mismatch between WEO and OECD.")
dacdefl <- dacdefl[!is.na(ISO)]

if(currency != "USD"){
  dacdefl <- dacdefl[ISO %in% c("DAC", "USA")]
}

#Calculate Total DAC for missing data years
weo_gdp_con_dac <- weo_gdp_con[ISO %in% c("AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "FIN", "FRA", "DEU", "GRC",
                                          "HUN", "ISL", "IRL", "ITA", "JPN", "KOR", "LUX", "NLD", "NZL", "NOR",
                                          "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "USA"
                                          ) & !(variable %in% dacdefl[ISO == "DAC" & !is.na(gdp_defl)]$variable)]
weo_totaldac_defl <- weo_gdp_con_dac[, .(ISO = "DAC", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]

weo_deflators <- rbind(weo_deflators, weo_totaldac_defl)

#Calculate EUI for missing data years
weo_gdp_con_eui <- weo_gdp_con[ISO %in% c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)& !(variable %in% dacdefl[ISO == "EUI" & !is.na(gdp_defl)]$variable)]
weo_eui_defl <- weo_gdp_con_eui[, .(ISO = "EUI", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]

weo_deflators <- rbind(weo_deflators, weo_eui_defl)

#Replace WEO DAC data with OECD data
dacdefl <- dacdefl[!is.na(gdp_defl), .(ISO, variable, gdp_defl, source = "OECD", ver = format(Sys.Date(), "%b%Y"))]
deflators <- rbind(weo_deflators[!(paste0(ISO, variable) %in% paste0(dacdefl$ISO, dacdefl$variable))], dacdefl)

if(!(base_year %in% dacdefl$variable)) warning("Cannot return OECD deflators for this base year; using all WEO data.")

deflators[, variable := as.numeric(as.character(variable))]

#GBR copies
GBR_copies <- c("AIA", "MSR", "SHN")
deflators <- rbind(deflators[!(ISO %in% GBR_copies)], rbindlist(lapply(GBR_copies, function(x) copy(deflators)[ISO == "GBR"][, ISO := x])))

#NZL copies
NZL_copies <- c("COK", "NIU", "TKL")
deflators <- rbind(deflators[!(ISO %in% NZL_copies)], rbindlist(lapply(NZL_copies, function(x) copy(deflators)[ISO == "NZL"][, ISO := x])))

#FRA copies
FRA_copies <- c("WLF")
deflators <- rbind(deflators[!(ISO %in% FRA_copies)], rbindlist(lapply(FRA_copies, function(x) copy(deflators)[ISO == "FRA"][, ISO := x])))

#DAC copies
if("DAC" %in% deflators$ISO){
  DAC_copies <- c("CUB", "PRK", "SYR", "LBN", "ERI", "AFG", "LKA")
  deflators <- rbind(deflators[!(ISO %in% DAC_copies)], rbindlist(lapply(DAC_copies, function(x) copy(deflators)[ISO == "DAC"][, ISO := x])))
}

##Approximate missing
if(approximate_missing){
  missing <- deflators[, .SD[any(is.na(gdp_defl))], by = ISO]
  missing_weo_gdp <- weo_gdp_con[ISO %in% missing$ISO]
  missing_weo_gdp[, variable := as.numeric(variable)]
  missing_weo_gr <- suppressWarnings(missing_weo_gdp[, .(gdp_avg_curg = (gdp_cur[!is.na(gdp_cur) & variable == max(variable[!is.na(gdp_cur)])]/gdp_cur[!is.na(gdp_cur) & variable == min(variable[!is.na(gdp_cur)])])^(1/(max(variable[!is.na(gdp_cur)])-min(variable[!is.na(gdp_cur)]))),
                                                         gdp_avg_cong = (gdp_con[!is.na(gdp_con) & variable == max(variable[!is.na(gdp_con)])]/gdp_con[!is.na(gdp_con) & variable == min(variable[!is.na(gdp_con)])])^(1/(max(variable[!is.na(gdp_con)])-min(variable[!is.na(gdp_con)]))))
                                                     , by = ISO])
  missing_weo_gr <- missing_weo_gr[, .(defg = gdp_avg_curg/gdp_avg_cong), by = ISO]
  
  missing_defl <- merge(deflators[ISO %in% missing$ISO], missing_weo_gr, by = "ISO")
  
  missing_defl_f <- suppressWarnings(missing_defl[, .SD[is.na(gdp_defl) & variable > max(variable[!is.na(gdp_defl)])], by = ISO])
  missing_defl_b <- suppressWarnings(missing_defl[, .SD[is.na(gdp_defl) & variable < min(variable[!is.na(gdp_defl)])], by = ISO])
  
  missing_defl_b[, defg := rev(cumprod(1/defg)), by = ISO]
  missing_defl_f[, defg := cumprod(defg), by = ISO]
  
  missing_defl_b <- merge(missing_defl_b[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_b$ISO, .SD[variable == min(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO")
  missing_defl_f <- merge(missing_defl_f[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_f$ISO, .SD[variable == max(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO")
  
  missing_defl <- rbind(missing_defl_b[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)], missing_defl_f[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)])
  
  missing_defl[, `:=` (source = paste0(source, "_est"))]
  
  deflators <- rbind(deflators[!(paste0(ISO, variable) %in% paste0(missing_defl$ISO, missing_defl$variable))], missing_defl)
}

#Final out
deflators <- deflators[, .(ISO, year = variable, base_year, currency, source, ver, gdp_defl)][order(ISO, year)]

fwrite(deflators, "deflators_2024USD.csv")
```

</details>

**This script is no longer wired into `util_fts_curated_flows.R`'s live execution path** — its output (`deflators_2024USD.csv`) is read as a static file instead (see [the FTS curation section above](#financial-tracking-service)), so it needs to be re-run manually whenever a new base year or WEO release is wanted.

# Parsing CVA data

Following the procurement of the required source data in the previous steps of this guide, this section lays out how to isolate the data relevant to CVA from those datasets.

## Projects module CVA fields

First, the relevant CVA fields from the projects module need to be identified so that the CVA information on those projects can be merged with the FTS funding data, given the projects module at this stage only represents planning figures.

Former DI staff already went through all the unique project questions from all response plans in English, French and Spanish up to 2023 to identify those relevant to CVA. These are saved in the CSV file 'cva_project_questions'. The relevant questions were also already classified into whether their answers represent a yes/no flag of whether CVA is part of the project (`flagCVA`), or whether they provide a quantitative indicator of the planned project budget share of cash (`quantC`) or vouchers (`quantV`). This list of relevant questions needs to be maintained and reviewed every year for possible additions — the script automates this review process by searching for CVA-related keywords (in English, French, and Spanish) across all unique questions seen in a given year's data, and writing out any that aren't yet in the labelled reference file for manual review.

The following script processes the project data fetched from the projects API, identifying projects related to CVA based on the labelled questions, and standardises their answers for further analysis. This is a substantially restructured version of the original script — using `data.table` operations rather than row-by-row loops — but produces the same logical outputs:

<details>

<summary>code/07_process_project_data.R</summary>

```R
source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

years <- 2018:2025
if (!dir.exists("output"))
  dir.create("output")

# Load and combine all project years
project_list <- lapply(years, function(yr) {
  load(paste0("projects/project_data_", yr, ".RData")) # loads all_projects
  all_projects
})
all_projects <- rbindlist(project_list, fill = T, use.names = T)
rm(project_list)
gc()

# Identify questions potentially related to CVA
all_questions <- unique(all_projects$question)
fwrite(data.table(question = all_questions), "output/questions.csv")

cash_kw <- paste0("\\b(", paste(
  c(
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
    "monétaire",
    "tokens",
    "coupons",
    "cupones",
    "public works programme",
    "social assistance",
    "social safety net",
    "social transfer",
    "social protection",
    # acronyms — case-sensitive match below
    "CVA",
    "CCT",
    "UCT",
    "CTP",
    "CFW",
    "CFA",
    "SSN",
    "ESSN",
    "MPC",
    "MPCT"
  ),
  collapse = "|"
), ")\\b")

potential_cash_qs <- all_questions[grepl(cash_kw, all_questions, ignore.case = T)]

labeled_qs <- fread("reference_datasets/cva_project_questions.csv", encoding = "UTF-8")
new_qs <- setdiff(potential_cash_qs, labeled_qs$Question)
if (length(new_qs)) {
  fwrite(data.table(question = new_qs),
         "output/potential_new_cash_questions.csv")
  message(
    length(new_qs),
    " potentially new CVA question(s) written to ",
    "output/potential_new_cash_questions.csv — please review and label."
  )
}

# Separate quantitative (percentage) and boolean (yes/no) questions
quant_qs <- labeled_qs[`Question type` %in% c("quantC", "quantV"), Question]
bool_qs <- labeled_qs[`Question type` == "flagCVA", Question]

quant_rows <- all_projects[question %in% quant_qs]
bool_rows <- all_projects[question %in% bool_qs]

# Standardise percentage answers
standardize_pct <- function(x) {
  x <- trimws(tolower(x))
  num <- NA_real_
  
  if (grepl("%", x, fixed = T)) {
    # "40%", "40 %", "about 40%", etc.
    m <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)?(?=\\s*%)", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
    
  } else if (grepl("percent", x, fixed = T)) {
    m <- regmatches(x,
                    regexpr("[0-9]+(?:\\.[0-9]+)?(?=\\s*percent)", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
    
  } else if (grepl("^[0-9]+(?:\\.[0-9]+)?$", x, perl = T)) {
    num <- as.numeric(x)
    
  } else if (grepl("less than 1", x, fixed = T)) {
    num <- 0
    
  } else {
    # Last resort: extract first numeric token
    m <- regmatches(x, regexpr("[0-9]+(?:\\.[0-9]+)?", x, perl = T))
    if (length(m))
      num <- as.numeric(m)
  }
  
  num
}

# Only keep answers that contain at least one numeric token
quant_rows <- quant_rows[grepl("[0-9]", answer)]
quant_rows[, pct := vapply(answer, standardize_pct, numeric(1))]

# Sum cash + voucher percentages per project; cap at 100
# NOTE: summing quantC and quantV can exceed 100 if both refer to overlapping
# portions of the budget. The cap prevents obvious over-counting but the
# underlying ambiguity remains; see GUIDE.md for discussion.
quant_by_proj <- quant_rows[!is.na(pct), .(cva_percentage = min(sum(pct), 100) / 100), by = project_id]

# Standardise boolean answers
TRUE_VALUES <- c("true", "oui", "yes")
bool_rows[, bool_val := tolower(answer) %in% TRUE_VALUES]
bool_by_proj <- bool_rows[, .(cva = any(bool_val)), by = project_id]

# Reconcile overlaps between the two sources
# 0% quantitative: treat as cva = FALSE in the boolean table (if not already)
zero_pct <- quant_by_proj[cva_percentage == 0, .(project_id, cva = F)]
zero_pct <- zero_pct[!project_id %in% bool_by_proj$project_id]
bool_by_proj <- rbindlist(list(bool_by_proj, zero_pct))

# F boolean: add 0% row to quantitative table (if not already)
false_bool <- bool_by_proj[cva == F, .(project_id, cva_percentage = 0)]
false_bool <- false_bool[!project_id %in% quant_by_proj$project_id]
quant_by_proj <- rbindlist(list(quant_by_proj, false_bool))

# Combine and make the two columns consistent
cash_projects <- merge(quant_by_proj, bool_by_proj, by = "project_id", all = T)
cash_projects[cva_percentage > 0, cva := T]
cash_projects[cva_percentage == 0, cva := F]

fwrite(cash_projects, "projects/cash_projects.csv")

# Project text for ML input
project_text <- unique(all_projects[, .(project_id, text = paste(project_name, project_objective))])
fwrite(project_text, "projects/project_text.csv")

message("Done. Outputs written to projects/ and output/.")
```

</details>

A couple of points worth noting about the current behaviour:

- The `standardize_pct()` function's last-resort branch extracts the first numeric token found anywhere in an answer that doesn't match any of the more specific patterns above it. This means a string like `"phase 2, 30% of which is cash"` would correctly extract 30, but a string like `"see annex 2 for breakdown"` (with no actual percentage figure) would incorrectly extract 2 and treat it as 2%. Answers that don't match a clear percentage pattern should arguably return `NA` rather than guessing at a number — this is a candidate for tightening in a future revision.
- The `cva_percentage` cap at 100 is a safety net rather than a methodologically justified choice: it's possible for a project to legitimately report, say, 60% cash and 50% vouchers as separate budget lines (summing to 110% before the cap), in which case capping to 100% may understate the combined CVA share, or it may correctly catch a genuine double-count — the data alone doesn't distinguish these cases.

## Combining FTS and projects module CVA data

Now that we have isolated the planned project budget percentage for cash/vouchers/CVA for projects with available data, we can use the unique project IDs to merge this information with FTS funding flow data to the same projects. We thereby make the assumption that the delivered share of CVA of the received funding for each project matches that of the planned CVA project budget share. However, in countries where country-level data disaggregated by cluster or implementing agency is available on the actual volumes of CVA delivered (usually from cash working groups), this can serve as useful check for whether this CVA data from the planning stage provides an over- or under-estimate in the given country of interest ([see below](#suggestions-for-future-improvements)).

We also import the project text for projects that received funding on FTS and combine it with the FTS description, which tends to be brief. This will provide more text data for the machine learning algorithm later on to classify the CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

This step, along with everything else up to (but not including) the machine-learning inference call, lives in `code/08a_fts_prepare_for_inference.R` — the original single `08_fts_keyword_searching_cash.R` script has been split in two around the point where execution pauses for the Python classifier.

## Identifying CVA relevance of funding

There are several possible ways to identify financial flows on FTS relevant to CVA, now that we have enriched it with projects data:

1. The 'method' column: this can contain either 'Traditional aid' as default value or, if reported to FTS, 'Cash transfer programming (CTP)'. There are however a number of flows that evidently support CVA as per the other methods of identifying relevant FTS data, indicating that this reporting field is unfortunately not in consistent use.
2. The 'destinationObjects_Cluster.name' column: this is a free-text field that represents the field cluster. It can be in English, French or Spanish. A number of response-plans include a multi-purpose cash cluster, which would be listed in this field, though in a number of different spellings or languages (though always the same spelling and language for the same response plan in the same year). The full list of recognised cash clusters is maintained as the `CASH_CLUSTERS` vector in `code/08a_fts_prepare_for_inference.R` and needs to be reviewed every year for updates.
3. The 'project_cva_percentage' column: This has been added from the projects dataset ([see above](#projects-module)) and represents the planned budget percentage of CVA for the project supported by this financial flow.
4. The 'all_text' column: this is a free-text field that often contains a description of the activity supported by the financial flow (merged from the flow description and project text). This can be scanned for CVA keywords and then classified by a machine learning algorithm for its CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

In the existing methodology, the choice was made to distinguish for each FTS financial flow whether its CVA relevance is full, partial or nonexistent. This was in recognition of a number of large financial flows, especially from the US, that as per their description supported a range of activities including CVA alongside other modalities. It would therefore be an overestimate to count the full value of those flows towards CVA and they are marked as partial. Financial flows with no identifiable CVA characteristics as per the three criteria listed above were marked as not relevant.

Otherwise, the categorisation into full/partial/none for the three categories works as follows:

1. 'Method': Marked as 'Full' if reported as 'Cash transfer programming (CTP)' and 'None' otherwise.
2. 'destinationObjects_Cluster.name': Marked as 'Full' if a relevant CVA field cluster is reported as the only destination cluster. Marked as 'Partial' if a CVA field cluster is reported as one of multiple destination clusters for the same flow. Marked as 'None' otherwise.
3. 'project_cva_percentage': Marked as 'Full' if greater than 75%, marked as 'Partial' if between 0 and 75%, marked as 'None' if equal to zero or blank.
4. [See below](#machine-learning-to-classify-cva-flow-descriptions) on machine-learning to classify flow and project descriptions.

**Two ordering bugs present in earlier versions of this logic have been fixed.** First, a flow explicitly reported with `method == "Cash transfer programming (CTP)"` could previously be silently downgraded from 'Full' to 'Partial' if the multi-cluster rule ran after the CTP rule and the flow happened to also be tagged with multiple clusters including a cash one — the CTP classification is applied *last*, so it always wins. Second, the project-percentage rules used to apply unconditionally to every flow, which meant a flow already correctly classified 'Full' by its cluster could have its `relevance` label silently overwritten to 'Partial' by a low project percentage, while the amount calculated later (in step 09, [see below](#calculating-the-cva-relevant-funding-amounts)) would still use the cluster-based 'Full' logic — so the displayed `relevance` no longer matched how the flow's CVA amount was actually calculated. The project-percentage rules only ever apply to flows still marked 'None', so `relevance` and the downstream calculation can never diverge.

The following code chunk executes this classification process with FTS flow data for steps 1 to 3, picking up directly from the merge step above:

<details>

<summary>code/08a_fts_prepare_for_inference.R</summary>

```R
# 08a_fts_prepare_for_inference.R
# PART 1 of 2 — run BEFORE the Python ML inference step.
#
# 1. Loads curated FTS flows and joins project CVA metadata.
# 2. Classifies each flow by sector / method / cluster and project percentage.
# 3. Writes the subset that needs ML inference to classifier_code/fts_to_inference.csv.
# 4. Writes the fully-flagged dataset (pre-ML) to output/fts_output_pre_ml.csv.
#
# After this script, run:
# cd classifier_code && source venv/bin/activate && python3 flow_inference.py && cd ..
# Then run 08b_fts_combine_inference.R.
#
# Run from the project root:
# Rscript code/08a_fts_prepare_for_inference.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

years <- 2018:2025
if (!dir.exists("output"))
  dir.create("output")

# Load curated FTS flows
fts <- rbindlist(lapply(years, function(yr)
  fread(paste0(
    "fts/fts_curated_", yr, ".csv"
  ))),
  use.names = T,
  fill = T)
fts <- fts[as.integer(year) >= 2018L]

# Join project CVA metadata
proj_meta <- fread("projects/cash_projects.csv")
proj_text <- fread("projects/project_text.csv")
proj_data <- merge(proj_text, proj_meta[, project_id := as.character(project_id)], by = "project_id", all = T)
setnames(
  proj_data,
  c("project_id", "text", "cva_percentage", "cva"),
  c(
    "destinationObjects_Project.id",
    "project_text",
    "project_cva_percentage",
    "project_cva"
  )
)

fts[, destinationObjects_Project.id := as.character(destinationObjects_Project.id)]
proj_data[, destinationObjects_Project.id :=
            as.character(destinationObjects_Project.id)]

fts <- merge(fts,
             proj_data,
             by = "destinationObjects_Project.id",
             all.x = T,
             sort = F)

# Combine FTS description with project text for richer ML input
fts[, all_text := fcase(
  !is.na(description) & !is.na(project_text), paste(trimws(description), trimws(project_text)),
  !is.na(description), trimws(description),
  !is.na(project_text), trimws(project_text),
  default = NA_character_
)]

# CVA cash clusters list
CASH_CLUSTERS <- c(
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
cash_cluster_regex <- paste(paste0(
  "(^|;\\s*)",
  gsub("([][(){}+*?|^$\\\\.])", "\\\\\\1", CASH_CLUSTERS),
  "(\\s*;|$)"
), collapse = "|")

# Step 1: classify by sector / method / cluster
# Bug fix: apply CTP LAST so it cannot be downgraded by the multi-cluster rule.
fts[, sector_method_cluster_relevance := "None"]

# Single cash cluster → Full
fts[destinationObjects_Cluster.name %in% CASH_CLUSTERS, sector_method_cluster_relevance := "Full"]

# Multiple clusters including a cash cluster → Partial
fts[grepl(";", destinationObjects_Cluster.name, fixed = T) &
      grepl(cash_cluster_regex, destinationObjects_Cluster.name), sector_method_cluster_relevance := "Partial"]

# CTP method → always Full, overriding the multi-cluster Partial rule above
fts[method == "Cash transfer programming (CTP)", sector_method_cluster_relevance := "Full"]

# Step 2: derive per-cluster count for Partial flows
fts[, destinationClusterCount :=
      fifelse(
        is.na(destinationObjects_Cluster.name) |
          destinationObjects_Cluster.name == "",
        0L,
        1L + nchar(destinationObjects_Cluster.name) -
          nchar(gsub(
            ";", "", destinationObjects_Cluster.name, fixed = T
          ))
      )]

# Step 3: initial relevance = sector/cluster, then project % may override
# Bug fix: only apply project-percentage overrides to rows NOT already
# classified by sector/cluster, preventing the relevance label from diverging
# from the actual amount calculation.
fts[, relevance := sector_method_cluster_relevance]
fts[, relevance_method := "Sector/Method/Cluster"]

fts[sector_method_cluster_relevance == "None" &
      !is.na(project_cva_percentage) &
      project_cva_percentage >= 0.75, `:=`(relevance = "Full", relevance_method = "Project CVA Percentage")]

fts[sector_method_cluster_relevance == "None" &
      !is.na(project_cva_percentage) &
      project_cva_percentage > 0 &
      project_cva_percentage < 0.75, `:=`(relevance = "Partial", relevance_method = "Project CVA Percentage")]
```

</details>

### Machine-learning to classify CVA flow descriptions

After applying the three above steps to classify the CVA relevance of FTS funding flows, this still leaves the possibility of identifying funding for CVA through unstructured text. The former DI team trained a machine-learning classifier on a manually classified dataset of several hundred FTS flows for their CVA relevance. This algorithm is then applied to text for FTS flows that either contain a keyword relevant to CVA in their flow or project text (the keyword lists `KW_NONCASE` and `KW_ACRONYMS` are in `code/08a_fts_prepare_for_inference.R` and can be adapted as required) or those that represent funding to projects flagged as CVA in the projects data ([see above](#projects-module-cva-fields)), but without data on the planned budget share of CVA.

The following code, still in `code/08a_fts_prepare_for_inference.R`, compiles the relevant text data and writes it out before we process it with the classifier:

<details>

<summary>code/08a_fts_prepare_for_inference.R (continued)</summary>

```R
# Step 4: keyword flag for ML candidate selection
# Non-case-sensitive keywords (matched case-insensitively)
KW_NONCASE <- c(
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
# Acronyms: match case-insensitively too (they appear in many casings in text)
KW_ACRONYMS <- c("CCT",
                 "UCT",
                 "CTP",
                 "CFW",
                 "CFA",
                 "SSN",
                 "ESSN",
                 "MPC",
                 "MPCT",
                 "CVA")
kw_regex <- paste0("\\b(", paste(c(KW_NONCASE, KW_ACRONYMS), collapse = "|"), ")\\b")
fts[, keyword_match := grepl(kw_regex, all_text, ignore.case = T)]

# Write ML input: flows needing inference
# Candidates: keyword hit or project-flagged as CVA, but still "None" relevance
to_infer <- unique(fts[(keyword_match |
                          !is.na(project_cva) & project_cva) &
                         relevance == "None", .(id, text = all_text)])

fwrite(to_infer, "classifier_code/fts_to_inference.csv")
message(nrow(to_infer),
        " flows written to classifier_code/fts_to_inference.csv")

# Save pre-ML flagged dataset
fwrite(fts, "output/fts_output_pre_ml.csv")
message("Pre-ML dataset written to output/fts_output_pre_ml.csv")
message("\nNext step: run the Python ML classifier, then 08b_fts_combine_inference.R")
```

</details>

The interactive `View()` calls used in earlier versions of this script to spot-check borderline classifications (flows with no sector/cluster/method match but a keyword hit, and vice versa) have been removed, since they would error or hang when the script is run non-interactively via `Rscript` rather than in RStudio. The classifier also receives `all_text` (flow description plus project text) rather than the flow description alone, matching the text that the classifier's training data (`CVA_flow_descriptions.csv`) was built from — in earlier versions the training data and the inference input used different text, which would have made the model's confidence scores somewhat less reliable than intended. One remaining inconsistency: `to_infer` is de-duplicated on the combination of `id` and `text` rather than on `id` alone, so if the same flow ID were ever to appear twice with two different `all_text` values (possible after the multi-year splitting step upstream), it would be sent to the classifier twice and could end up duplicated in the merged output — de-duplicating on `id` alone (taking the first `text` value) would be more robust.

The CSV data from this code serves as input for the classifier, which is run in Python. The classifier loads a locally saved fine-tuned model rather than pulling from the Hugging Face Hub — see [classifier_code/README.md](classifier_code/README.md) for training details. The following script processes the text descriptions of flows and predicts their CVA relevance based on the model's classification:

<details>

<summary>classifier_code/flow_inference.py</summary>

```python
from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch
import pandas as pd
from tqdm import tqdm

card = "../classifier_code/cva-flow-weighted-classifier2/best_model"
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
tokenizer = AutoTokenizer.from_pretrained(card)
model = AutoModelForSequenceClassification.from_pretrained(card).to(device)

def inference(example):
    inputs = tokenizer(example['text'], return_tensors="pt", truncation=True, max_length=512)
    inputs = {k: v.to(device) for k, v in inputs.items()}
    with torch.no_grad():
        logits = model(**inputs).logits
    predicted_class_id = logits.argmax().item()
    example['predicted_class'] = model.config.id2label[predicted_class_id]
    class_confidence = float(torch.softmax(logits[0], dim=0)[1])
    probs = torch.softmax(logits[0], dim=0)
    example['predicted_confidence'] = float(probs[model.config.label2id["Full"]])
    return example

def main():
    df = pd.read_csv("fts_to_inference.csv")
    results = [inference({"text": text}) for text in tqdm(df["text"], desc="Running inference")]
    df["predicted_class"] = [r["predicted_class"] for r in results]
    df["predicted_confidence"] = [r["predicted_confidence"] for r in results]
    df.to_csv("fts_to_inference_output.csv", index=False)

if __name__ == '__main__':
    main()
```

</details>

The algorithm predicts, for each candidate flow, whether it has full or partial relevance to CVA based on the text input, along with the model's confidence in the `"Full"` class. This resulting prediction data is then merged into the FTS flow dataset by `code/08b_fts_combine_inference.R` — the second half of the original `08_fts_keyword_searching_cash.R` script, run after the Python step completes:

<details>

<summary>code/08b_fts_combine_inference.R</summary>

```R
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
```

</details>

This step is functionally the same as the original code, restricted to genuine ML candidates (`ml_candidate`) rather than re-checking the keyword/project conditions inline at each `relevance` assignment, and adds a couple of diagnostic summaries printed to the console for a quick sanity check before moving on to step 09.

### Calculating the CVA-relevant funding amounts

Finally, what remains is to calculate the estimated CVA US$ amount in terms of total programming costs supported by each financial flow in the dataset. Given, as described above, FTS captures financial flows between organisations (i.e., from donors to implementers, or more rarely sub-grants from one implementer to another), instances of funding to CVA projects that are classified as 'Full' and thereby fully included in terms of their CVA amounts also include programming costs. For financial flows with planning information on the share of the project budget for CVA, it is possible that those shares represent transfer values only. However, it is not straightforward to ascertain this, given the CVA project questions are often ambiguously worded and do not explicitly ask for the share of CVA in terms of transfer value or programming costs. The many instances of projects indicating CVA project budget shares of 90% or higher suggest that at least some organisations interpret this question to also refer to programming costs. This methodology therefore assumes that instances of funding on FTS to CVA include transfers and overall CVA programming costs, whether counted fully or partially according to the logic laid out below.

The logical steps for calculating CVA amounts are laid out as follows:

1. Including the full current USD amount if 'method' is reported as 'Cash transfer programming (CTP)', or if there is only one destination field cluster and that is relevant to CVA (usually multi-purpose cash)
2. For remaining flows with relevant field clusters, including a proportion of the current USD amount if there are multiple destination field clusters, one of which is relevant to CVA. The proportion is estimated by taking the fraction of one divided by the number of total field clusters reported for that financial flow.
3. For remaining flows with CVA project budget shares, including the current USD amount multiplied by those budget shares.
4. For remaining flows with a predicted CVA relevance from the machine learning model, including all of the current USD amount if the predicted confidence for the 'Full' class is 80% or higher and if the text field includes one of a small set of common keywords (cash/voucher/cva/coupon) — this common-keyword check acts as a final sanity check on top of the model's own confidence, since the model can occasionally be confident about flows whose text doesn't obviously mention CVA at all.
5. For remaining flows where the model is instead confidently predicting 'Partial' (confidence for the 'Full' class of 20% or lower) and the same common-keyword check passes, a CVA amount is assigned automatically — calculated as the current USD amount multiplied by the empirical average ratio of `CVAamount` to `amountUSD` observed elsewhere in the dataset among flows already classified 'Partial' by the rules above. This step did not exist in earlier versions of the methodology, which left these flows to fall through to manual review; it is a reasonable approximation but, unlike steps 1–4, doesn't derive from any property of the individual flow itself.
6. Remaining flows with a predicted CVA relevance from the machine learning model strictly between 20% and 80% (and not matching steps 4 or 5) that do not meet the criteria of the previous steps are compiled as a list of financial flows that require manual review of their text field for whether they seem to fully or partially support CVA, or whether they represent false positives, or whether the text is insufficient to make that assessment (then also excluded). A sample file with examples of reasoning behind each of these different cases is included in the reference datasets in this repository.

To save time when executing this guide and methodology, the script joins the manual review decisions from former DI staff, stored in `reference_datasets/historical_cva_decisions.csv`, to the dataset. The remaining flows that have not yet been coded and that remain after step 6 need to be reviewed and classified manually — see the "Manual review" section of the [README](README.md#manual-review) for the current workflow. The following code chunk executes the calculation steps above:

<details>

<summary>code/09_calculate_cva.R</summary>

```R
# 09_calculate_cva.R
# Calculates the estimated CVA USD amount for each flagged FTS flow using a
# priority hierarchy:
#
# 1. Sector / method / cluster — Full or Partial (÷ cluster count)
# 2. Project CVA percentage — for flows not already assigned in step 1
# 3. High-confidence ML — ≥80 % confidence + common cash keyword
# 4. Prior manual decisions — from reference_datasets/historical_cva_decisions.csv
# 5. New manual queue — written to output/cva_to_manually_classify.csv
# (read back when output/cva_manually_classified.csv exists)
#
# Bug fixes vs. original:
# • CVAamount_type label is saved BEFORE CVAamount is updated (index reuse bug)
# • CTP flows cannot be downgraded to Partial (fixed in 08a)
# • relevance and sector_method_cluster_relevance are now consistent (fixed in 08a)
#
# Outputs:
# output/cva_to_manually_classify.csv — flows awaiting manual review
# output/fts_cva.csv — final dataset with CVAamount column
#
# Run from the project root:
# Rscript code/09_calculate_cva.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table")

if (!dir.exists("output"))
  dir.create("output")

fts_flagged <- fread("output/fts_output_CVA.csv")
fts_flagged[, amountUSD := as.numeric(amountUSD)]

# Initialise CVAamount columns
fts_flagged[, `:=`(CVAamount = 0, CVAamount_type = "")]

# Step 1: Full sector / method / cluster
idx <- fts_flagged$sector_method_cluster_relevance == "Full"
fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "Sector, method, cluster")]

# Step 2: Partial sector / cluster (÷ number of destination clusters)
idx <- fts_flagged$sector_method_cluster_relevance == "Partial"
fts_flagged[idx, `:=`(CVAamount = amountUSD / destinationClusterCount,
                      CVAamount_type = "Partial cluster")]

# Step 3: Project CVA percentage
# Bug fix: capture the qualifying index BEFORE updating CVAamount so the
# type label uses the same set of rows as the amount assignment.
idx <- which(fts_flagged$CVAamount == 0 &
               !is.na(fts_flagged$project_cva_percentage))
fts_flagged[idx, `:=`(CVAamount = amountUSD * project_cva_percentage,
                      CVAamount_type = "Project CVA percentage")]

# Step 4a: High-confidence Full ML prediction + common keyword
COMMON_KW_REGEX <- "\\b(cash|vouchers?|cva|coupon)\\b"
fts_flagged[, common_words_match :=
              grepl(COMMON_KW_REGEX, all_text, ignore.case = T)]

idx <- which(
  fts_flagged$CVAamount == 0 &
    !is.na(fts_flagged$predicted_confidence) &
    fts_flagged$predicted_confidence >= 0.8 &
    fts_flagged$common_words_match
)
fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "ML high predicted relevance")]

# Step 4b: High-confidence Partial ML prediction + common keyword (apply average proportion from known partials)
COMMON_KW_REGEX <- "\\b(cash|vouchers?|cva|coupon)\\b"
fts_flagged[, common_words_match :=
              grepl(COMMON_KW_REGEX, all_text, ignore.case = T)]

idx <- which(
  fts_flagged$CVAamount == 0 &
    !is.na(fts_flagged$predicted_confidence) &
    fts_flagged$predicted_confidence <= 0.2 &
    fts_flagged$common_words_match
)

partial_share <- fts_flagged[grepl("Partial", relevance), sum(CVAamount)/sum(amountUSD)]

fts_flagged[idx, `:=`(CVAamount = amountUSD*partial_share, CVAamount_type = "ML high predicted partial relevance")]

# Step 5a: Prior manual decisions
POSITIVE_DECISIONS <- c("Decision: accept; judgement", "Decision: include; judgement")

prior_manual_file <- "reference_datasets/historical_cva_decisions.csv"
if (file.exists(prior_manual_file)) {
  prior_manual <- fread(prior_manual_file)
  
  # Warn if any unrecognised decision strings exist — silent drops are a risk
  known_decisions <- unique(prior_manual$decision)
  unexpected <- setdiff(
    known_decisions,
    c(
      POSITIVE_DECISIONS,
      "Decision: exclude; judgement",
      "Decision: insufficient text; judgement",
      "Decision: false positive; judgement"
    )
  )
  if (length(unexpected))
    warning(
      "Unrecognised decision label(s) in ",
      prior_manual_file,
      ": ",
      paste(unexpected, collapse = ", "),
      "\n These rows will be EXCLUDED from CVA totals."
    )
  
  positive_ids <- prior_manual[decision %in% POSITIVE_DECISIONS, id]
  
  idx <- which(fts_flagged$CVAamount == 0 &
                 fts_flagged$id %in% positive_ids)
  fts_flagged[idx, `:=`(CVAamount = amountUSD, CVAamount_type = "Manual")]
  
  # Add confirmed positive examples to classifier training data
  new_training <- fts_flagged[CVAamount_type == "Manual", .(id, text = all_text)][, label := 1L]
  classifier_data <- fread("classifier_code/CVA_flow_descriptions.csv")
  classifier_data[, text := gsub("\"", "", text)]
  new_training <- new_training[!id %in% classifier_data$id &
                                 !text %in% classifier_data$text]
  if (nrow(new_training)) {
    fwrite(rbindlist(list(classifier_data, new_training), fill = T),
           "classifier_code/CVA_flow_descriptions.csv", quote = T)
    message(nrow(new_training),
            " new training example(s) added to classifier data.")
  }
} else {
  message("No prior manual decisions file found at ",
          prior_manual_file,
          " — skipping.")
  prior_manual <- data.table(id = character(0))
}

# Step 5b: Queue remaining uncertain flows for manual review
# Flows with ML score 0.2-0.8 go to a manual review file. 
# Those already handled by the prior decisions file
# are excluded.
manual_queue_idx <- which(
  fts_flagged$CVAamount == 0 &
  !is.na(fts_flagged$predicted_confidence) &
  !fts_flagged$common_words_match &
  fts_flagged$predicted_confidence > 0.2 &
  fts_flagged$predicted_confidence < 0.8)

fts_manual_queue <- fts_flagged[manual_queue_idx]
fts_manual_queue[, CVAamount_type := "Manual"]
fts_manual_uncoded <- fts_manual_queue[!id %in% prior_manual$id]

fwrite(fts_manual_uncoded, "output/cva_to_manually_classify.csv")
message(
  nrow(fts_manual_uncoded),
  " flow(s) written to output/cva_to_manually_classify.csv for manual review."
)

fts_cva <- fts_flagged[CVAamount > 0 & is.finite(CVAamount)]

# Summary
message("\nCVA amount type breakdown:")
print(fts_cva[, .(flows = .N,
                  total_CVA_USD = sum(CVAamount, na.rm = T)), by = CVAamount_type][order(-total_CVA_USD)])

fwrite(fts_cva, "output/fts_cva.csv")
message("\nFinal dataset written to output/fts_cva.csv (",
        nrow(fts_cva),
        " flows)")
```

</details>

A few further notes on the current behaviour of this script:

- **`output/fts_cva.csv` no longer includes manually classified flows.** In earlier versions, this script also read `output/cva_manually_classified.csv` (if present) and appended it to the output here. That step has moved to the start of `10_global_cva_analysis.R` instead, along with new validation of the manually classified file and automatic clean-up of the review queue once decisions are incorporated — see [Global estimated volumes of humanitarian CVA](#global-estimated-volumes-of-humanitarian-cva) below.
- **The training-data write uses a quote-stripping workaround rather than a structural fix.** `classifier_data[, text := gsub("\"", "", text)]` strips any existing quotation marks from previously stored training examples before appending new ones and writing the whole file back out with `quote = TRUE`. This avoids a `data.table::fwrite`/pandas interoperability issue where re-quoting already-quoted text caused doubled quote characters that broke downstream CSV parsing — but it also means any genuine quotation marks in the original flow or project text are permanently destroyed on each write. A structural fix (e.g. switching `CVA_flow_descriptions.csv` to a format like Parquet that doesn't need character-level escaping) would avoid this trade-off, but has not been implemented.
- **The historical decisions file was renamed** from `Mike_cva_decisions.csv` to `reference_datasets/historical_cva_decisions.csv`, reflecting that it's no longer maintained by one specific person.

# CVA data analysis

## Global estimated volumes of humanitarian CVA

The primary use of the above data analysis, adapted from its early iteration from the ['Counting Cash' paper](https://odi.org/en/publications/counting-cash-tracking-humanitarian-expenditure-on-cash-based-programming/) in 2016, is to calculate an estimate of the global value of humanitarian cash and voucher assistance delivered in any given year. There are usually two sets of figures:

1. The overall programming costs, including transfer values, for delivering CVA. The rationale behind calculating this is that funding is required for more than just the CVA transfer value to facilitate those transfers.
2. The transfer values of the delivered CVA, disaggregated if possible by cash and vouchers.

The CVA survey requests both from implementing agencies, though only a small share of respondents provides both and most only provide data on the transfer values. FTS data, as mentioned above, is assumed to provide an indication of funding amounts to overall CVA programming. Given the need to therefore be able to convert CVA programming costs into transfer values and vice versa, every year the methodology was executed in the past, a percentage was calculated based on data from organisations that provided both in their surveys (programming costs and transfer values) of what the ratio was from the latter to the former for the entire sample. This is included as a third tab in the survey data file. For organisations that did not provide CVA programming costs in the survey data, this set of percentages is used to calculate the estimated programming costs for each provided transfer value, **and the reverse imputation is now also applied** — organisations that reported a transfer value but not a programming cost have their programming cost derived from the same ratio, which was not the case in earlier versions of this script (which only ever imputed transfer value from programming cost, never the other way round).

The sub-grant data in the survey file includes a column 'Take out', which indicates whether the recipient organisation of each sub-grant has also provided survey data and therefore should be taken into consideration to avoid double-counting when aggregating that survey data. Filling in this column has in the past been a manual process of first reviewing all survey submissions (in the 'Survey_data' tab) and then going through the received data on sub-grants line by line to highlight which of the sub-grant recipients also provided survey data.

What then remains is to follow the steps below to aggregate the CVA data in both datasets while avoiding double-counting:

1. Starting with the survey data, we aggregate all of the programming costs and the transfer values by organisation type and years.
2. From the calculated program costs, we subtract all the aggregate sub-grant values by source organisation type and year that were highlighted to be taken out to avoid double-counting. We remove double-counted funding by source organisation type to ensure that they are allocated to the organisation type that does the last mile of CVA delivery. For the calculated transfer values in step 1, we do the same but multiply the sub-grant amounts by the corresponding percentage of the transfer value/programming cost estimate for each year.
3. For FTS data, we aggregate all the CVA amounts by cva_org_type (to match survey data) and year, ensuring we only include data for destination organisations in years for which those organisations did not also submit a survey. This provides the programming costs CVA estimate from FTS data additional to the CVA survey. We multiply those values by the corresponding percentage of the transfer value/programming cost estimate for each year to obtain the FTS estimates on CVA transfer values supported by this funding.
4. We add the values calculated from survey and FTS data for both programming costs and transfer values by organisation type and year to obtain the final set of global CVA estimates.

The sub-grant recipient matching step (subtracting step 2 above) has been substantially rewritten since earlier versions of this guide. Previously, recipient organisation names were matched against the combined organisation list using a sequence of nested `for` loops calling `stringdist()` once per recipient — workable for the dataset's current size but slow and difficult to follow. The current version computes the full pairwise Levenshtein distance matrix in one vectorised call and otherwise follows the same four-strategy cascade (exact match → fuzzy match → recipient-name-is-substring-of-org-name → org-name-is-substring-of-recipient-name), with all percentage-matched diagnostics printed to the console at each stage so a reviewer can quickly see how much of the matching is being done automatically versus falling through to manual overrides.

The following chunk of code executes all of these steps — manual review incorporation, FTS/survey combination, bidirectional PC/TV imputation, sub-grant de-duplication, and final aggregation — in a single script:

<details>

<summary>code/10_global_cva_analysis.R</summary>

```R
# 10_global_cva_analysis.R
# Aggregates CVA amounts from FTS flows and survey data, removes sub-grant
# double-counting via multi-strategy name matching, and produces summary
# tables by organisation and org type.
#
# Inputs:
# output/fts_cva.csv
# reference_datasets/cva_survey_data.xlsx (sheets: Survey_data, 2, 3)
# reference_datasets/fts_survey_overlap.csv
# reference_datasets/cva_org_type.csv
#
# Outputs:
# output/cva_agg.csv — CVA by organisation (de-doubled)
# output/cva_agg_org_type.csv — CVA by org type × year (de-doubled)
#
# Run from the project root:
# Rscript code/10_global_cva_analysis.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table", "openxlsx", "stringdist")

if (!dir.exists("output"))
  dir.create("output")

# Load FTS CVA flows
fts_cva <- fread("output/fts_cva.csv")
queue <- fread("output/cva_to_manually_classify.csv")

manual_classified_file <- "output/cva_manually_classified.csv"
if (file.exists(manual_classified_file)) {
  manual <- fread(manual_classified_file)
  
  # Validate schema
  required_cols <- c("id", "CVAamount", "CVAamount_type")
  missing_cols <- setdiff(required_cols, names(manual))
  if (length(missing_cols))
    stop(
      "cva_manually_classified.csv is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  
  # Validate amounts
  bad_amounts <- manual[is.na(CVAamount) |
                          !is.finite(CVAamount) | CVAamount <= 0]
  if (nrow(bad_amounts))
    stop(
      nrow(bad_amounts),
      " row(s) in cva_manually_classified.csv have ",
      "missing or zero CVAamount. IDs: ",
      paste(bad_amounts$id, collapse = ", ")
    )
  
  # Warn about IDs not traceable to the current queue
  stale <- manual[!id %in% queue$id, id]
  if (length(stale))
    warning(
      length(stale),
      " manually classified ID(s) not found in current ",
      "cva_to_manually_classify.csv — these may be stale:\n ",
      paste(stale, collapse = ", ")
    )
  
  message(nrow(manual), " manually classified flow(s) incorporated.")
  
  # Remove verified rows from the queue so it only contains pending decisions
  queue_remaining <- queue[!id %in% manual$id]
  fwrite(queue_remaining, "output/cva_to_manually_classify.csv")
  
  message(
    nrow(queue) - nrow(queue_remaining),
    " row(s) removed from queue. ",
    nrow(queue_remaining),
    " still pending."
  )
  
  fts_cva <- rbindlist(list(fts_cva, manual),
                       fill = TRUE,
                       use.names = TRUE)
} else {
  warning(nrow(queue), " manual checks still pending.")
}

fts_cva <- fts_cva[destinationObjects_Organization.name !=
                     "International NGOs (Confidential)"]

# Load survey data (three sheets)
survey_data <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = "Survey_data"))
sub_grants <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = 2))
pc_tv_estimate <- as.data.table(read.xlsx("reference_datasets/cva_survey_data.xlsx", sheet = 3))
setnames(pc_tv_estimate, "CVA.data.year", "Year")

survey_data[, Organisation := trimws(Organisation.Reference)]
survey_data[, PC.USD.m := as.numeric(PC.USD.m)]
survey_data[, TV.USD.m := as.numeric(TV.USD.m)]
survey_data[, source := "Survey"]
survey_data[, newMoney := "FALSE"]

sub_grants <- sub_grants[tolower(Take.out) == "y"]

# Build FTS ↔ survey organisation name mapping
fts_survey_overlap <- fread("reference_datasets/fts_survey_overlap.csv", header = T)
name_mapping <- unique(fts_survey_overlap[, .(destinationObjects_Organization.name, Organisation = `Survey name`)])

# Identify (Organisation, Year) pairs covered by survey → exclude from FTS agg
survey_years <- unique(survey_data[, .(Organisation, Year)])
survey_years <- merge(
  survey_years,
  name_mapping,
  by = "Organisation",
  all.x = T,
  sort = F,
  allow.cartesian = T
)

missing_mapping <- unique(survey_years[is.na(destinationObjects_Organization.name), Organisation])
if (length(missing_mapping))
  message("No FTS name mapping for survey orgs: ",
          paste(missing_mapping, collapse = ", "))

survey_years <- survey_years[!is.na(destinationObjects_Organization.name)]
survey_overlap_keys <- survey_years[, paste(destinationObjects_Organization.name, Year)]

# Org type lookup
cva_org_type <- fread("reference_datasets/cva_org_type.csv")
setnames(cva_org_type, "cva_org_type", "Org_type")

# Aggregate FTS CVA by org × year
fts_cva_agg <- fts_cva[, .(PC.USD.m = sum(CVAamount, na.rm = T) / 1e6), by = .(
  Year = as.integer(year),
  newMoney,
  destinationObjects_Organization.name,
  destinationObjects_Organization.organizationSubTypes
)]

fts_cva_agg[, source := "FTS"]
fts_cva_agg <- merge(
  fts_cva_agg,
  name_mapping,
  by = "destinationObjects_Organization.name",
  all.x = T,
  sort = F
)
fts_cva_agg <- merge(
  fts_cva_agg,
  cva_org_type,
  by = "destinationObjects_Organization.organizationSubTypes",
  all.x = T,
  sort = F
)

fts_cva_agg[, Local_type := "International"]
fts_cva_agg[grepl("National|Local", destinationObjects_Organization.organizationSubTypes), Local_type := "National"]

# Remove FTS rows for (org, year) pairs already covered by the survey
fts_cva_agg[, .key := paste(destinationObjects_Organization.name, Year)]
fts_cva_agg <- fts_cva_agg[!.key %in% survey_overlap_keys][, .key := NULL]


# Combine FTS and survey
cva_agg <- rbindlist(list(survey_data, fts_cva_agg),
                     fill = T,
                     use.names = T)

# Impute TV from PC using the annual PC→TV ratio
cva_agg <- merge(
  cva_agg,
  pc_tv_estimate,
  by = "Year",
  all.x = T,
  sort = F
)

cva_agg[is.na(TV.USD.m), TV.USD.m := PC.USD.m*PC.average.used]
cva_agg[is.na(PC.USD.m), PC.USD.m := TV.USD.m/PC.average.used]

cva_agg[is.na(Organisation), Organisation := destinationObjects_Organization.name]

# Sub-grant name matching
# Sub-grant recipient names must be matched to organisations in cva_agg so we
# can subtract the received sub-grant amounts and avoid double-counting.
# Four strategies in priority order:
# 1. Exact match (after normalisation)
# 2. Fuzzy (Levenshtein distance ≤ 20% of string length)
# 3. Substring A — recipient name is a whole word in org name
# 4. Substring B — org name is a whole word in recipient name
# Followed by a small set of manual overrides for the residual cases.

normalise <- function(x)
  trimws(gsub("\\s+", " ", gsub("[[:punct:]]", " ", tolower(x))))
quotemeta <- function(x)
  gsub("(\\W)", "\\\\\\1", x, perl = T)

# Apply recipient substitutions BEFORE normalising
sub_grants[Recipient.org %in% c("Unknown", "Governments", "NGOs", "Local and national partners"), Recipient.org := Donor.org]
sub_grants[Recipient.org.type == "RCRC", Recipient.org := Donor.org]

sub_grants[, clean_name := normalise(Recipient.org)]
sub_grants[clean_name %in% c("unknown", "not provided potentially sensitive"), clean_name := NA_character_]

cva_agg[, clean_org := normalise(Organisation)]

uniq_sub <- na.omit(unique(sub_grants$clean_name))
uniq_sub <- uniq_sub[uniq_sub != ""]
uniq_orgs <- na.omit(unique(cva_agg$clean_org))
uniq_orgs <- uniq_orgs[uniq_orgs != ""]

match_dt <- data.table(
  subgrant_name = uniq_sub,
  perfect_match = NA_character_,
  fuzzy_match = NA_character_,
  fuzzy_dist = NA_integer_,
  substr_a_match = NA_character_,
  substr_b_match = NA_character_
)

# 1. Exact match
exact_idx <- match(match_dt$subgrant_name, uniq_orgs)
match_dt[!is.na(exact_idx), perfect_match := uniq_orgs[na.omit(exact_idx)]]
message(sprintf(
  "Exact match: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$perfect_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$perfect_match))
))

# 2. Fuzzy (Levenshtein, vectorised distance matrix)
dist_mat <- stringdistmatrix(match_dt$subgrant_name, uniq_orgs, method = "lv")
allowable <- pmax(ceiling(0.20 * nchar(match_dt$subgrant_name)), 1L)

best_dist <- apply(dist_mat, 1, min)
best_col <- apply(dist_mat, 1, which.min)
fuzzy_ok <- best_dist <= allowable

match_dt[fuzzy_ok, `:=`(fuzzy_match = uniq_orgs[best_col[fuzzy_ok]], fuzzy_dist = best_dist[fuzzy_ok])]

# Clear a known incorrect fuzzy hit before reporting
match_dt[subgrant_name == "drc", fuzzy_match := NA_character_]

message(sprintf(
  "Fuzzy match: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$fuzzy_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$fuzzy_match))
))

# 3. Substring A: recipient ⊆ org
for (i in seq_len(nrow(match_dt))) {
  regex <- paste0("\\b", quotemeta(match_dt$subgrant_name[i]), "\\b")
  hits <- which(grepl(regex, uniq_orgs, perl = T))
  if (length(hits))
    match_dt[i, substr_a_match := uniq_orgs[hits[which.min(nchar(uniq_orgs[hits]))]]]
}
message(sprintf(
  "Substring A: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$substr_a_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$substr_a_match))
))

# 4. Substring B: org ⊆ recipient (only for still-unmatched rows)
still_unmatched <- is.na(match_dt$perfect_match) &
  is.na(match_dt$fuzzy_match) &
  is.na(match_dt$substr_a_match)

for (j in seq_along(uniq_orgs)) {
  regex <- paste0("\\b", quotemeta(uniq_orgs[j]), "\\b")
  hits <- which(still_unmatched &
                  grepl(regex, match_dt$subgrant_name, perl = T))
  if (length(hits))
    match_dt[hits, substr_b_match := uniq_orgs[j]]
}
message(sprintf(
  "Substring B: %d / %d (%.0f%%)",
  sum(!is.na(match_dt$substr_b_match)),
  nrow(match_dt),
  100 * mean(!is.na(match_dt$substr_b_match))
))

pct_combined <- 100 * mean(
  !is.na(match_dt$perfect_match) |
    !is.na(match_dt$fuzzy_match) |
    !is.na(match_dt$substr_a_match) |
    !is.na(match_dt$substr_b_match)
)
message(sprintf("Combined: %.0f%%", pct_combined))

# 5. Manual overrides
set_manual <- function(dt, pattern, target, regex = F) {
  idx <- if (regex)
    grepl(pattern, dt$subgrant_name, perl = T)
  else
    dt$subgrant_name == pattern
  dt[idx, perfect_match := target]
}

UNRWA_FULL <- paste0("united nations relief and works agency for ",
                     "palestine refugees in the near east")

set_manual(match_dt, "care bangladesh", "care international")
set_manual(match_dt, "wfp", "world food programme")
set_manual(match_dt,
           "save the childrensave the children",
           "save the children")
set_manual(match_dt, "wvi", "world vision international")
set_manual(match_dt,
           "world vision|vision mund",
           "world vision international",
           T)
set_manual(match_dt, "acf", "action against hunger")
set_manual(match_dt, "acf ethiopia", "action against hunger")
set_manual(match_dt, "action contre la faim espagne", "action against hunger")
set_manual(match_dt, "cww", "concern worldwide")
set_manual(match_dt, "dan church aid", "dca")
set_manual(match_dt, "drc", "danish refugee council")
set_manual(match_dt, "norwegian refugee council", "nrc")
set_manual(match_dt, "pin", "people in need")
set_manual(match_dt, "unrwa", UNRWA_FULL)
set_manual(match_dt, "unrwa united nations relief and wor", UNRWA_FULL)
set_manual(match_dt, "the united nations relief and works", UNRWA_FULL)
set_manual(match_dt, "united nations children s fund", "unicef")
set_manual(match_dt,
           "red (cross|crescent)",
           "red cross and red crescent movement",
           T)
set_manual(match_dt, "plan malawi", "plan international")
set_manual(match_dt,
           "adra romania",
           "adventist development and relief agency")
set_manual(match_dt, "somali cash consortium", "concern worldwide")

# Report any genuinely unresolved names
unresolved <- match_dt[is.na(perfect_match) & is.na(fuzzy_match) &
                         is.na(substr_a_match) &
                         is.na(substr_b_match), .(subgrant_name)]
if (nrow(unresolved)) {
  message("Unmatched sub-grant recipients (excluded from de-doubling):")
  print(unresolved)
}

# Resolve to best single match per recipient
match_dt[, best_match := perfect_match]
match_dt[is.na(best_match), best_match := fuzzy_match]
match_dt[is.na(best_match), best_match := substr_a_match]
match_dt[is.na(best_match), best_match := substr_b_match]

org_lookup <- setNames(match_dt$best_match, match_dt$subgrant_name)
sub_grants[, clean_org := org_lookup[clean_name]]
sub_grants[, newMoney := "FALSE"]

# Aggregate sub-grant amounts and subtract from recipient totals
sub_grants_agg <- sub_grants[!is.na(clean_org), .(PC.USD.m_subgrant = sum(Amount.USD, na.rm = T) / 1e6), by = .(clean_org, Year, newMoney)]

cva_agg <- merge(
  cva_agg,
  sub_grants_agg,
  by = c("clean_org", "Year", "newMoney"),
  all.x = T,
  sort = F
)
cva_agg[is.na(PC.USD.m_subgrant), PC.USD.m_subgrant := 0]
cva_agg[, PC.USD.m_undoubled := pmax(PC.USD.m - PC.USD.m_subgrant, 0)]

# Scale the TV deduction using the same PC→TV ratio as above
cva_agg[, TV.USD.m_subgrant := PC.USD.m_subgrant * PC.average.used]
cva_agg[, TV.USD.m_undoubled := pmax(TV.USD.m - TV.USD.m_subgrant, 0)]
cva_agg[, PC.average.used := NULL]

# Aggregate by org type × year
cva_agg_org_type <- cva_agg[, .(
  PC.USD.m = sum(PC.USD.m_undoubled, na.rm = T),
  TV.USD.m = sum(TV.USD.m_undoubled, na.rm = T)
), by = .(Year, Org_type, Local_type)]
setorder(cva_agg_org_type, Year, Org_type, Local_type, -PC.USD.m)

# Write outputs
fwrite(cva_agg, "output/cva_agg.csv")
fwrite(cva_agg_org_type, "output/cva_agg_org_type.csv")
message("Written output/cva_agg.csv and output/cva_agg_org_type.csv")

# Summary
message("\nGlobal CVA by year (PC USD billions, de-doubled):")
print(cva_agg[, .(PC_bn = round(sum(PC.USD.m_undoubled, na.rm = T) / 1e3, 2), TV_bn = round(sum(TV.USD.m_undoubled, na.rm = T) / 1e3, 2)), by = Year][order(Year)])
```

</details>

The resulting global total for humanitarian CVA is larger than totals calculated solely based on FTS and projects data. This is primarily due to the inclusion of survey data submitted by implementing agencies on their global organisational total CVA figures, and given those implementing agencies do not comprehensively report on all their volumes of humanitarian CVA to FTS and because projects data is not available for all crisis contexts.

One unit assumption in the sub-grant subtraction worth flagging: `Amount.USD` in the sub-grants sheet is divided by `1e6` to match the `PC.USD.m` units used everywhere else, on the assumption that it's recorded in raw USD rather than already in millions. This should be reconfirmed against the source workbook each year, since an error here would silently scale the de-duplication adjustment by a factor of a million in either direction without raising any error.

## CVA data by cluster

There are two main avenues to explore CVA data by cluster with the data compiled in this guide. 

The first involves the FTS CVA dataset generated above. Within that, we could analyse funding for CVA by cluster across a large number of contexts. The ‘destinationObjects\_GlobalCluster.name’ column standardises the field clusters into a set of global clusters and allows for easier analysis (though currently there is no designated ‘multi-purpose cash’ category within that column). However, given none of the possible ways of reporting on CVA to FTS are used comprehensively or consistently, this data will inevitably be partial. It might therefore be harder to use as a basis for advocacy with clusters to change the share of CVA within each cluster if that true share is only partially known.

The second possible way involves using the project planned budget data ([see above](#projects-module-cva-fields)) before it is combined with FTS data. Thie has the advantage that it provides a complete account of planning figures for each HRP with available data (usually over 20 plans each year). This should therefore provide information with fairly high confidence about the planned significance of CVA by cluster in those response plans. This data would be available alongside information on which organisations or organisation types plan for smaller or greater shares of CVA within their activities, and what those activities are. It also has the advantage for being planning data that it can be used for forward-looking advocacy on activities that are yet to be implemented, unlike most other CVA data, which is mostly retrospective.

## CVA data by country

The dataset compiled above from FTS and projects data allows for a partial analysis of global CVA volumes by country. Based on those two data sources, CVA data is likely to be more comprehensive (though still not complete) for countries with  data on the planned share of CVA within project budgets and/or with a multi-purpose cash field cluster. The comprehensiveness can be further improved by incorporating data from the WFP CASHboard ([see above](#wfp-cashboard-analytics)) to get a complete representation of WFP’s CVA delivery by country. This would then require excluding WFP from the FTS/projects data to avoid double-counting. The disadvantage for the WFP data is however that it does not include donor information.

Another possible avenue for CVA data analysis by country is to collate context-specific data from Cash Working Groups (CWGs). If covering the CVA delivery across all clusters and market-based delivery modalities in the  context, CWG data is likely more comprehensive than the FTS/projects data for that context. However, there are a few challenges with using CWG data:

* The data is not standardised across CWGs in different contexts, making comparable or aggregated global/regional analysis challenging.
* Most CWGs share data in the form of static, PDF or dynamic, PowerBI dashboards. Neither however tends to come with the possibility to download disaggregated data by implementer. This makes scraping this data labour intensive or not possible, sometimes requiring to reach out to CWGs directly.
* Most CWGs focus on the delivery and the 3/4Ws of CVA data, but do not include information on CVA donors.

## Relative share of CVA as % of IHA

There tends to be interest, especially from within the CALP Network, on what share of international humanitarian assistance (IHA) is delivered as CVA. Calculating this is currently flawed and only a best estimate given it involves comparing financial inputs to the humanitarian system with its outputs (with CVA a delivery modality). Given the lack of comprehensive public reporting on how much and when humanitarian activities funding from international funding deliver CVA, there is no sufficient data to connect the two. There can also likely be a mismatch (both ways) of funding being disbursed by a donor in one year and it being delivered as CVA in another, making it harder to compare data on financial inputs and outputs in the same year.

Still, given the demand for this calculation, the method used so far to do it was to take the global CVA estimate for programming costs produced in this guide ([see above](#global-estimated-volumes-of-humanitarian-cva)) and to divide it by the best possible estimate of IHA available for each year. This also requires first excluding some CVA survey data that would not be included by IHA funding data, which currently is humanitarian CVA delivered domestically in countries that are international humanitarian net donors. Currently, the two known instances of that in this methodology are CVA delivered by RCRC national societies in their respective (donor) countries and humanitarian CVA delivered by GiveDirectly domestically in the US. The breakdown of the CVA transfer value data submitted by the RCRC Movement via survey is available at the [RCRC Cash Hub](https://cash-hub.org/resources/cash-maps/) and amounts delivered in donor countries need to be calculated manually for years with available data. These years can then serve as estimate for years withouta a publicly available breakdown of CVA by national society. Data on the breakdown of humanitarian CVA delivered domestically in the US and internationally was in the past provided by GiveDirectly for some years.

The IHA figures used are those calculated by DI, which are partly based on a labour-intensive process to compile data on private humanitarian funding alongside an analysis of donor funding amounts based on mostly DAC data for DAC member donors and otherwise FTS data for other donors. IHA figures need to be converted to current prices to be comparable to CVA figures, which have not been deflated in this methodology ([see above](#deflators)). The historical IHA data in current prices calculated by DI is provided alongside this guide to aid calculating this percentage.

The estimated relative share of CVA as percentage of total IHA can then be calculated by:

1. Taking the global volumes of humanitarian CVA calculated [above](#global-estimated-volumes-of-humanitarian-CVA).
2. Subtracting from that humanitarian CVA amounts delivered in countries that are net humanitarian donors as explained in this section of the guide. This is because those amounts would not be included in the denominator (which represents **international** humanitarian assistance).
3. Dividing for each year the resulting value of global humanitarian CVA by the total volumes of IHA in current prices.

# Methodological limitations

For the global estimate of humanitarian CVA, the bulk of the data underlying this estimate is derived from survey data provided by implementing agencies. However, given the survey is designed to present a minimal reporting burden on respondents ([see above](#cva-survey)), it provides very few data points beyond global totals to further investigate CVA trends (e.g., lack of donor, country or cluster data).

The FTS and project data have the advantage that they embed reporting on CVA within other routine reporting on funding flows or projects, allowing for potentially much richer CVA analysis. However, given there is not a single donor nor implementer that comprehensively reports on their CVA portfolio to those data platforms, it only provides a partial view with representing between around a third to half of the global volumes of humanitarian CVA (as estimated based on survey data of global totals), though coverage is higher for countries with better CVA reporting ([see above](#cva-data-by-country)). 

Further, FTS is designed to represents transfers between organisations, while CVA transfers to recipients are a form of organisational expenditure. This means that it would require significant changes not just to the level of reporting but also the scope and structure of FTS to be able to more accurately capture such expenditure, given it currently does not include any reporting on expenditure whatsoever. 

Finally, while the project data can yield a quantiative view of the planned share of project budgets to be delivered as CVA, this only works for response plans with project-level data. In 2021, those plans absorbed around a third of funding reported to FTS for that year and of that, only around 60% of funding to those plans was reported with project IDs and can therefore be mapped against these planned CVA project budget shares.

# Suggestions for future improvements

The most important lever for more granular publicly available data on CVA is better reporting by implementing agencies of CVA. If the [minimum agreements on tracking CVA](https://www.calpnetwork.org/publication/tracking-cash-and-voucher-assistance-agreements-recommendations-and-minimum-requirements-from-the-grand-bargain-cash-workstream/) were followed by the many agencies that endorsed them within the Grand Bargain, much of the workarounds due to partial CVA data laid out in this guide would be superfluous and there would be timely, disaggregated data on CVA as part of other routine reporting processes. This data would allow for better coordination of CVA delivery, reduce the need for parallel CVA reporting processes in country, enable greater accountability for donors and implementers that committed to scaling up CVA, and more. 

Short of these much needed but wider-ranging changes to reporting, a low hanging fruit might be for cash working groups to improve the interoperability of the CVA data they collect by including unique identifiers, such as project IDs, and by making the data underlying their dashboards publicly available (subject to security screenings of sensitive information).

Related to humanitarian CVA and not covered in this guide, there still is a blind spot on the scale of social protection payments in crisis countries to crisis-affected populations. Those payments arguably represent humanitarian CVA delivered by or aligned with the national government. However, there is no comparable global data source on those payments that allows for the identification of transfer volumes to crisis-affected areas or populations. This is unlikely to be feasible given the many differences of social protection payment schemes and systems between countries. It might be possible though to shed more light on such payments and their current and future utility to crisis-affected populations in specific contexts with available data.

Other, technical improvements to the above guide might include:
 
* Changing the sequence of logical steps for CVA amount calculation depending on what way of reporting on CVA to FTS or the projects module is deemed to be most/least reliable ([see above](#calculating-the-cva-relevant-funding-amounts))
* Triangulating the robustness of planned CVA project budget shares for specific countries with data from CWGs on CVA delivered by agency/cluster if available. Depending on the analysis focus and if the required data is available from cash working groups in the country/countries of interest directly, it might be more comprehensive in those instances to rely on data of actual CVA transfers in those countries over estimates based on partial FTS and planned project data.
* Adjusting CVA financial volumes for inflation, given the time horizon of the global analysis is approaching one decade and therefore increases in prices over that entire time period inflate the growing significance of CVA within humanitarian responses as those become more expensive. However, the choice of deflators would likely be arbitrary and flawed. ([see above](#relative-share-of-cva-as--of-iha))

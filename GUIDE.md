# Tracking humanitarian cash and voucher assistance

# Overview and purpose

This methodology and step-by-step guide outlines the process formerly followed by Development Initiatives (DI) and the CALP Network to produce a best possible estimate on global financial volumes of humanitarian cash and voucher assistance. It explains the technical steps alongside their reasoning from data extraction to identifying CVA data, combining it from different datasets and finally analysing the output.

The guide is intended to capture the methodological knowledge and methods by former DI staff, following the insolvency of DI, and thereby ensure that the work can be adapted and continued if the demand for it continues to exist. It therefore also highlights the different aspects of the methodology that need to be reviewed on an ongoing basis and concludes with suggestions for further improvement.

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

The main difference between the minimum agreements on tracking CVA and our survey is the inclusion of sub-grant data. This is because it is common practice for a large recipient of funding for CVA from government donors (e.g., WFP, or UNHCR) to then sub-grant all or aspects of the delivery of that CVA to recipients to another implementing agency. Given we request data from those large actors as well as smaller agencies that receive funding from them, we need the sub-grant data to ensure that there is no double-counting across survey data from both ([see below](#global-estimated-volumes-of-humanitarian-cva)).


## Financial Tracking Service

The [Financial Tracking Service](https://fts.unocha.org/) (FTS) by UN OCHA is the most comprehensive source of global humanitarian financing flows in close to real time. It was originally set up to track progress on the funding requirements of UN-coordinated humanitarian response plans, but has since expanded its ambition to track all international humanitarian funding flows whether in- or outside of those plans.

The central data element of the FTS data structure is the financial flow between organisations. These financial flows have characteristics assigned to them at the source and destination (e.g., organisation, location, cluster, or year) on where there are from or to and more, and in addition have a set of characteristics that are central to that flow (e.g., the flow status on whether it is pledged, committed or paid, whether it is a financial flow or in-kind support, or what aid modality the financial flow supports). Some of those characteristics are more comprehensively reported on than others.

Before identifying the FTS data that is relevant to CVA, we first have to extract the financial flows data from FTS. 

The first step of that process is to retrieve the flow data for the year of interest from the FTS API. The request to the API can also be tailored for specific plans, emergencies, global clusters, recipient countries or more. The following code accesses the FTS API to get flows data:
<details>

<summary>01_fts_get_flows.R</summary>

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

The following function will be required to split FTS flows that run across different years by each year, assuming an even distribution over time:

<details>

<summary>02_fts_split_rows.R</summary>

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

The splitting of rows by year was necessary to deflate funding amounts by year later in the next step:

<details>

<summary>03_deflators.R</summary>

```R
get_deflators <- function(base_year = 2021, currency = "USD", weo_ver = NULL, approximate_missing = T){
  suppressPackageStartupMessages(lapply(c("data.table", "httr", "jsonlite","lubridate"), require, character.only=T))
  
  if(!dir.exists("weo")){
    dir.create("weo")
  }
  
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
  weo_filename = paste0("weo/",weo_ver ,"all.xls")
  if(!file.exists(weo_filename)){
    while(T){
      url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year,"/",weo_month_text, "/WEO", weo_ver ,"all.ashx")
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
    }
    download.file(url, destfile=weo_filename)
  }
  
  message("Using IMF WEO version ", weo_ver, ".")
  
  weo <- read.delim(weo_filename,sep="\t",na.strings=c("","n/a","--"),check.names=F, fileEncoding="utf-16")
  weo = data.table(weo)
  country_codes <- unique(weo[, .(ISO, Country)])
  country_codes = country_codes[complete.cases(country_codes),]
  
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
  
  #Calculate Total DAC
  oecd_dac_iso3 <- c(
    "AUS", # Australia
    "AUT", # Austria
    "BEL", # Belgium
    "CAN", # Canada
    "CZE", # Czech Republic
    "DNK", # Denmark
    "EST", # Estonia
    "FIN", # Finland
    "FRA", # France
    "DEU", # Germany
    "GRC", # Greece
    "HUN", # Hungary
    "ISL", # Iceland
    "IRL", # Ireland
    "ITA", # Italy
    "JPN", # Japan
    "KOR", # South Korea
    "LTU", # Lithuania
    "LUX", # Luxembourg
    "NLD", # Netherlands
    "NZL", # New Zealand
    "NOR", # Norway
    "POL", # Poland
    "PRT", # Portugal
    "SVK", # Slovakia
    "SVN", # Slovenia
    "ESP", # Spain
    "SWE", # Sweden
    "CHE", # Switzerland
    "GBR", # United Kingdom
    "USA"  # United States
  )
  weo_gdp_con_dac <- weo_gdp_con[ISO %in% oecd_dac_iso3]
  weo_totaldac_defl <- weo_gdp_con_dac[, .(ISO = "DAC", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]
  
  weo_deflators <- rbind(weo_deflators, weo_totaldac_defl)
  
  deflators <- weo_deflators

  deflators[, variable := as.numeric(variable)]
  
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
    DAC_copies <- c("CUB", "PRK", "SYR")
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
    
    missing_defl_b <- suppressWarnings(merge(missing_defl_b[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_b$ISO, .SD[variable == min(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO"))
    missing_defl_f <- suppressWarnings(merge(missing_defl_f[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_f$ISO, .SD[variable == max(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO"))
    
    missing_defl <- rbind(missing_defl_b[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)], missing_defl_f[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)])
    
    missing_defl[, `:=` (source = paste0(source, "_est"))]
    
    deflators <- rbind(deflators[!(paste0(ISO, variable) %in% paste0(missing_defl$ISO, missing_defl$variable))], missing_defl)
  }
  
  #Final out
  deflators <- deflators[, .(ISO, year = variable, base_year, currency, source, ver, gdp_defl)][order(ISO, year)]
  return(deflators)
}

```

</details>

The following code then executes the three steps that were defined above, on top of further manipulations to tidy up the downloaded FTS data (e.g., removing duplicates across API requests across multiple years):

<details>

<summary>04_fts_curated_flows.R</summary>

```R
list.of.packages <- c("tidyverse", "data.table", "jsonlite","rstudioapi")
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

fts_curated_flows <- function(years = 2017:2024, update_years = NA, dataset_path = "fts", base_year = 2022, weo_ver = NULL){

  #Load FTS utility functions and deflators
  code_dir = getCurrentFileLocation()
  source(paste(code_dir, "01_fts_get_flows.R", sep="/"))
  source(paste(code_dir, "02_fts_split_rows.R", sep="/"))
  source(paste(code_dir, "03_deflators.R", sep="/"))

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
    fts_list[[i]] <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"), encoding = "UTF-8")
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
  isos <- fread("reference_datasets/isos.csv", encoding = "UTF-8", showProgress = F)
  fts <- merge(fts, isos[, .(countryname_fts, destination_org_iso3 = iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T, sort = F)
  fts[, destination_org_country := destinationObjects_Location.name]
  fts[grepl(";", destination_org_country), `:=` (destination_org_country = "Multi-destination_org_country", destination_org_iso3 = "MULTI")]
  
  #Deflate by source location and destination year
  fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
  fts_locs <- data.table(fromJSON("https://api.hpc.tools/v1/public/location")$data)
  fts_orgs[, `:=` (source_org_type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), source_org_country = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name), source_org_country_id = ifelse(is.null(locations[[1]]$id), NA, locations[[1]]$id)), by = id]
  fts_orgs <- merge(fts_orgs, fts_locs[, .(id, iso3)], by.x = "source_org_country_id", by.y = "id", all.x = T, sort = F)
  fts_orgs <- fts_orgs[, .(sourceObjects_Organization.id = as.character(id), source_org_country, source_org_iso3 = iso3, FTS_source_orgtype = source_org_type)]
  fts <- merge(fts, fts_orgs, by = "sourceObjects_Organization.id", all.x = T, sort = F)
  
  #Deflate
  deflators <- get_deflators(base_year = base_year, currency = "USD", weo_ver = weo_ver, approximate_missing = T)
  deflators <- deflators[, .(source_org_iso3 = ISO, year = as.character(year), deflator = gdp_defl)]
  
  fts <- merge(fts, deflators, by = c("source_org_iso3", "year"), all.x = T, sort = F)
  fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[source_org_iso3 == "DAC"], by = "year", all.x = T, sort = F)$deflator
  fts[, `:=` (amountUSD_defl = amountUSD/deflator, amountUSD_defl_millions = (amountUSD/deflator)/1000000)]
  
  #Reorder columns nicely
  col_order <- union(col_order, names(fts)[order(names(fts))])
  fts <- fts[, col_order, with = F]

  return(fts)
}

```

</details>

The final bit of code relating to the FTS data creates as ‘master’ dataset of all the extracted FTS data across all years in this guide’s case for further analysis. It executes the code of the functions that were defined in the previous four code chunks. If there are years of FTS other than 2017 to 2024 data to be downloaded and cleaned in future, this would need to be changed in this code. Given FTS updates daily, it is recommended to regularly update the most recent and ongoing year (by specifying that year for 'update_years' in the 'fts_save_master' function):

<details>

<summary>05_fts_curated_master.R</summary>

```R
list.of.packages <- c("tidyverse")
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

fts_save_master <- function(years = 2017:2024, update_years = NA, path = "fts/"){
  fts_all <- fts_curated_flows(years, update_years = update_years)
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

<details>

<summary>06_fetch_projects.R</summary>

```R
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

```

</details>

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

All currency units tend to be converted to US$ to have a uniform currency across different financial amounts. There are a number of possible data sources to use for this conversion. In previous iterations of this methodology, DI chose to align the exchange rates for this analysis with that used in other parts of DI’s [Global Humanitarian Assistance Reports](https://devinit.org/resources/falling-short-humanitarian-funding-reform/) for consistent currency conversions across analyses. This might be relevant when trying to estimate the share of total international humanitarian assistance (IHA) made up by CVA ([see below](#relative-share-of-cva-as--of-iha)).

If following that approach, the exchanges used in different years to convert different currencies into US$ would be primarily sourced from the OECD DAC, and supplemented by the IMF and World Bank for currencies missing from the OECD DAC, as executed by the following code chunk:

<details>

<summary>util_exchange_rates.R</summary>

```R
list.of.packages <- c("data.table", "jsonlite", "devtools", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))
devtools::install_github("christophergandrud/imfr")
library(imfr)

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
source("util_oecd_sdmx.R")
setwd("..")

years <- 1950:2025

isos <- fread("reference_datasets/isos.csv", encoding = "UTF-8", na.strings = "")

all_ex <- data.table(expand.grid(iso3 = c(isos$iso3, "EUI"), year = years))

##OECD
if(!file.exists("reference_datasets/oecd_ex.csv")){
  oecd_ex = OECD("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A....EXC_A.......?startPeriod=1950&dimensionAtObservation=AllDimensions")
  fwrite(oecd_ex, "reference_datasets/oecd_ex.csv")
}else{
  oecd_ex = fread("reference_datasets/oecd_ex.csv")
}
# setdiff(unique(oecd_ex$`Reference area`), isos$countryname_oecd)
# setdiff(isos$countryname_oecd, unique(oecd_ex$`Reference area`))
oecd_ex <- merge(isos[, .(countryname_oecd, iso3)], oecd_ex, by.x = "countryname_oecd", by.y = "Reference area", all.y = T)

{oecd_ex[countryname_oecd == "Euro area (20 countries)", iso3 := "EUI"]
oecd_ex[countryname_oecd == "European Union (27 countries from 01/02/2020)", iso3 := "EUI"]
oecd_ex[countryname_oecd == "Hong Kong (China)", iso3 := "HKG"]
oecd_ex[countryname_oecd == "Russia", iso3 := "RUS"]
oecd_ex[countryname_oecd == "Slovak Republic", iso3 := "SVK"]
oecd_ex[countryname_oecd == "Czechia", iso3 := "CZE"]}
oecd_ex$variable = year(oecd_ex$`Time period`)

oecd_ex <- oecd_ex[!is.na(value) & value != 0]

##WORLD BANK
if(!file.exists("reference_datasets/wb_ex.csv")){
  wb_ex <- data.table(fromJSON("https://api.worldbank.org/v2/country/all/indicator/PA.NUS.ATLS?date=1950:2025&format=json&per_page=20000")[[2]])
  fwrite(wb_ex, "reference_datasets/wb_ex.csv")
}else{
  wb_ex = fread("reference_datasets/wb_ex.csv")
}
wb_ex <- wb_ex[, .(iso3 = countryiso3code, variable = date, value = value)]

wb_ex <- wb_ex[!is.na(value) & (!(paste0(iso3, variable) %in% oecd_ex[, paste0(iso3, variable)]))]

##IFS
if(!file.exists("reference_datasets/ifs.csv")){
  imf_app_name("calp-cva")
  params = imf_parameters("IFS")
  ifs_ex <- data.table(imf_dataset(database_id="IFS", indicator="ENDA_XDC_USD_RATE", freq="A"))
  fwrite(ifs_ex, "reference_datasets/ifs.csv")
}else{
  ifs_ex = fread("reference_datasets/ifs.csv")
}

setnames(ifs_ex, c("ref_area", "value", "date"), c("iso2c","ENDA_XDC_USD_RATE", "year"))
ifs_ex <- merge(isos[, .(iso3, iso2)], ifs_ex[!is.na(`ENDA_XDC_USD_RATE`)], by.x= "iso2", by.y = "iso2c", all = T)[, .(iso3, variable = year, value = `ENDA_XDC_USD_RATE`)]
ifs_ex = ifs_ex[!is.na(iso3) & !is.na(variable) & !is.na(value)]
ifs_ex <- ifs_ex[!is.na(value) & !(paste0(iso3, variable) %in% c(oecd_ex[, paste0(iso3, variable)], wb_ex[, paste0(iso3, variable)]))]

##All
oecd_ex = oecd_ex[,c("iso3", "variable", "value")]
all_wd_ex <- rbind(oecd_ex, wb_ex, ifs_ex)[, .(iso3, year = variable, value = value)]

all_ex <- merge(all_ex, all_wd_ex, all.x = T)

fwrite(all_ex, "reference_datasets/usd_exchange_rates.csv")

```

</details>

### Deflators

Currently, the global volumes of humanitarian CVA are presented in current prices, i.e., without adjusting for inflation/rising costs. The main reasons for this are that it would be a slightly arbitrary choice of which set of deflators to use for the implementing agencies’ data (the price level in donor countries or in recipient locations (if known)?) and that adjusting for inflation would require manipulating the implementers’ data so that they potentially do not recognise themselves in the trend anymore.

However, this means that the increase in global volumes of humanitarian CVA is likely inflated by increasing costs/price levels in both donor and recipient countries. It therefore does not represent an increase of x% across years (depending on the years of comparison) all else being equal.

For time-series analysis of financial data over a long time period, it can make sense to adjust financial data in each year for inflation to have better comparability over time. For example, in current prices, the total bilateral official development assistance from OECD DAC countries increased by 324% between 2002 and 2022, but when adjusting for inflation by deflating both to constant 2022 prices, this changes to an increase by only 189%.

Deflators are mostly relevant for this guide as consideration for future methodological adjustments ([see below](#suggestions-for-future-improvements)) and for calculating CVA as % of IHA, the latter previously calculated by DI in constant prices ([see below](#relative-share-of-cva-as--of-iha)). The following code chunk calculates the deflators by donor, which are required to change the IHA figures back to current prices so that they are more comparable with the CVA figures:  

<details>

<summary>03_deflators.R</summary>

```R
get_deflators <- function(base_year = 2021, currency = "USD", weo_ver = NULL, approximate_missing = T){
  suppressPackageStartupMessages(lapply(c("data.table", "httr", "jsonlite","lubridate"), require, character.only=T))
  
  if(!dir.exists("weo")){
    dir.create("weo")
  }
  
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
  weo_filename = paste0("weo/",weo_ver ,"all.xls")
  if(!file.exists(weo_filename)){
    while(T){
      url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year,"/",weo_month_text, "/WEO", weo_ver ,"all.ashx")
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
    }
    download.file(url, destfile=weo_filename)
  }
  
  message("Using IMF WEO version ", weo_ver, ".")
  
  weo <- read.delim(weo_filename,sep="\t",na.strings=c("","n/a","--"),check.names=F, fileEncoding="utf-16")
  weo = data.table(weo)
  country_codes <- unique(weo[, .(ISO, Country)])
  country_codes = country_codes[complete.cases(country_codes),]
  
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
  
  #Calculate Total DAC
  oecd_dac_iso3 <- c(
    "AUS", # Australia
    "AUT", # Austria
    "BEL", # Belgium
    "CAN", # Canada
    "CZE", # Czech Republic
    "DNK", # Denmark
    "EST", # Estonia
    "FIN", # Finland
    "FRA", # France
    "DEU", # Germany
    "GRC", # Greece
    "HUN", # Hungary
    "ISL", # Iceland
    "IRL", # Ireland
    "ITA", # Italy
    "JPN", # Japan
    "KOR", # South Korea
    "LTU", # Lithuania
    "LUX", # Luxembourg
    "NLD", # Netherlands
    "NZL", # New Zealand
    "NOR", # Norway
    "POL", # Poland
    "PRT", # Portugal
    "SVK", # Slovakia
    "SVN", # Slovenia
    "ESP", # Spain
    "SWE", # Sweden
    "CHE", # Switzerland
    "GBR", # United Kingdom
    "USA"  # United States
  )
  weo_gdp_con_dac <- weo_gdp_con[ISO %in% oecd_dac_iso3]
  weo_totaldac_defl <- weo_gdp_con_dac[, .(ISO = "DAC", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]
  
  weo_deflators <- rbind(weo_deflators, weo_totaldac_defl)
  
  deflators <- weo_deflators

  deflators[, variable := as.numeric(variable)]
  
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
    DAC_copies <- c("CUB", "PRK", "SYR")
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
    
    missing_defl_b <- suppressWarnings(merge(missing_defl_b[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_b$ISO, .SD[variable == min(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO"))
    missing_defl_f <- suppressWarnings(merge(missing_defl_f[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_f$ISO, .SD[variable == max(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO"))
    
    missing_defl <- rbind(missing_defl_b[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)], missing_defl_f[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)])
    
    missing_defl[, `:=` (source = paste0(source, "_est"))]
    
    deflators <- rbind(deflators[!(paste0(ISO, variable) %in% paste0(missing_defl$ISO, missing_defl$variable))], missing_defl)
  }
  
  #Final out
  deflators <- deflators[, .(ISO, year = variable, base_year, currency, source, ver, gdp_defl)][order(ISO, year)]
  return(deflators)
}

```

</details>

# Parsing CVA data

Following the procurement of the required source data in the previous steps of this guide, this section lays out how to isolate the data relevant to CVA from those datasets.

## Projects module CVA fields

First, the relevant CVA fields from the projects module need to be identified so that the CVA information on those projects can be merged with the FTS funding data, given the projects module at this stage only represents planning figures.

Former DI staff already went through all the unique project questions from all response plans in English, French and Spanish up to 2023 to identify those relevant to CVA. These are saved in the CSV file ‘cva\_project\_questions’. The relevant questions were also already classified into whether their answers represent a yes/no flag of whether CVA is part of the project (flagCVA), whether they provide a quantitative indicator of the planned project budget share of cash, vouchers or both (quantC/V/CVA), or whether they contain other information related to CVA, for instance on conditionality or relating to the recipients (otherCVA). This list of relevant questions would need to be maintained and reviewed every year for possible additions. A line of code was added to the script to automate this question review process each year by searching for keywords relevant to CVA in the set of questions, identifying any newly added questions with those keywords that were not previously marked as CVA.

The following script processes the project data fetched from the projects API by merging project data from multiple years, identifying projects related to CVA based on relevant questions, and standardizes the answers for further analysis:

<details>

<summary>07_process_project_data.R</summary>

```R
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

```

</details>

## Combining FTS and projects module CVA data

Now that we have isolated the **planned** project budget percentage for cash/vouchers/CVA for projects with available data, we can use the unique project IDs to merge this information with FTS funding flow data to the same projects. We thereby make the assumption that the delivered share of CVA of the received funding for each project matches that of the planned CVA project budget share.

We also import the project text for projects that received funding on FTS and combine it with the FTS description, which tends to be brief. This will provide more text data for the machine learning algorithm later on to classify the CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

The following code reads in the FTS and the project data, merges the CVA project budget percentages by project ID and combines project text fields:

<details>

<summary>08_fts_keyword_searching_cash.R lines 1-51</summary>

```R
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

```

</details>

## Identifying CVA relevance of funding

There are several possible ways to identify financial flows on FTS relevant to CVA, now that we have enriched it with projects data:

1. The ‘method’ column: this can contain either ‘Traditional aid’ as default value or, if reported to FTS, ‘Cash transfer programming (CTP)’. There are however a number of flows that evidently support CVA as per the other methods of identifying relevant FTS data, indicating that this reporting field is unfortunately not in consistent use.  
2. The ‘destinationObjects\_Cluster.name’ column: this is a free-text field that represents the field cluster. It can be in English, French or Spanish. A number of response-plans include a multi-purpose cash cluster, which would be listed in this field, though in a number of different spellings or languages (though always the same spelling and language for the same response plan in the same year). Relevant clusters identified up until 2023 are listed in the 08\_fts\_keyword\_searching\_cash.R code chunk from lines 98 up to 119 and need to be maintained every year to check for updates.  
3. The ‘project\_cva\_percentage’ column: This has been added from the projects dataset and represents the planned budget percentage of CVA for the project supported by this financial flow.  
4. The ‘all\_text’ column: this is a free-text field that often contains a description of the activity supported by the financial flow (merged from the flow description and project text). This can be scanned for CVA keywords and then classified by a machine learning algorithm for its CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

In the existing methodology, the choice was made to distinguish for each FTS financial flow whether its CVA relevance is full, partial or nonexistent. This was in recognition of a number of large financial flows, especially from the US, that as per their description supported a range of activities including CVA alongside other modalities. It would therefore be an overestimate to count the full value of those flows towards CVA and they are marked as partial. Financial flows with no identifiable CVA characteristics as per the three criteria listed above were marked as not relevant.

Otherwise, the categorisation into full/partial/none for the three categories works as follows:

1. ‘Method’: Marked as ‘Full’ if reported as ‘Cash transfer programming (CTP)’ and ‘None’ otherwise.  
2. ‘destinationObjects\_Cluster.name’: Marked as ‘Full’ if a relevant CVA field cluster is reported as the only destination cluster. Marked as ‘Partial’ if a CVA field cluster is reported as one of multiple destination clusters for the same flow. Marked as ‘None’ otherwise.  
3. ‘project\_cva\_percentage’: Marked as ‘Full’ if greater than 75%, marked as ‘Partial’ if between 0 and 75%, marked as ‘None’ if equal to zero or blank.  
4. [See below](#machine-learning-to-classify-cva-flow-descriptions) on machine-learning to classify flow and project descriptions.

The following code chunk executes this classification process with FTS flow data for steps 1 to 3:

<details>

<summary>08_fts_keyword_searching_cash.R lines 52-148</summary>

```R

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

```

</details>

### Machine-learning to classify CVA flow descriptions

After applying the three above steps to classify the CVA relevance of FTS funding flows, this still leaves the possibility of identifying funding for CVA through unstructured text. The former DI team trained a machine-learning classifier on a manually classified dataset of several hundred FTS flows for their CVA relevance. This algorithm is then applied to text for FTS flows that either contain a keyword relevant to CVA in their flow or project text (the existing keyword list is in lines 52 to 76 and can be adapted as required) or those that represent funding to projects flagged as CVA in the projects data ([see above](#projects-module-cva-fields)), but without data on the planned budget share of CVA.

The following code chunk compiles the relevant text data before we process that with the classifier:

<details>

<summary>08_fts_keyword_searching_cash.R lines 149-157</summary>

```R

# Save those that are either keyword match or marked at project level as CVA, but are None for ML
to_inference = subset(fts, (keyword_match | project_cva) & relevance == "None")
keep = c("id", "description")
to_inference = unique(to_inference[,keep,with=F])
setnames(to_inference, "description", "text")
fwrite(to_inference, "classifier_code/fts_to_inference.csv")

```

</details>

The CSV data from this code serves as input for the classifier, which is run in Python. The following script uses a pre-trained machine learning model to infer the relevance of FTS flows to CVA by processing the text descriptions of flows and predicts for their CVA relevance based on the model's classification:

<details>

<summary>classifier_code/flow_inference.py</summary>

```python
from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch
from datasets import load_dataset
from scipy.special import softmax

card = "alex-miller/cva-flow-weighted-classifier2"
tokenizer = AutoTokenizer.from_pretrained(card)
model = AutoModelForSequenceClassification.from_pretrained(card)


def inference(example):
    inputs = tokenizer(example['text'], return_tensors="pt")

    with torch.no_grad():
        logits = model(**inputs).logits

    predicted_class_id = logits.argmax().item()
    class_name = model.config.id2label[predicted_class_id]
    predicted_confidences = softmax(logits[0], axis=0)
    class_confidence = predicted_confidences[1]
    example['predicted_class'] = class_name
    example['predicted_confidence'] = class_confidence
    return example

def main():
    dataset = load_dataset("csv", data_files="fts_to_inference.csv", split="train")
    dataset = dataset.map(inference)
    dataset.to_csv('fts_to_inference_output.csv')


if __name__ == '__main__':
    main()

```

</details>

The algorithm predicts with a percentage chance for each prediction whether the relevant financial flows from FTS have full or partial relevance to CVA based on the text input. This resulting prediction data can then be merged into the FTS flow dataset through the following code chunk:

<details>

<summary>08_fts_keyword_searching_cash.R lines 159-end</summary>

```R

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

```

</details>

### Calculating the CVA-relevant funding amounts

Finally, what remains is to calculated the estimated CVA US$ amount in terms of total programming costs supported by each financial flow in the dataset. Given, as described above, FTS captures financial flows between organisations (i.e., from donors to implementers, or more rarely sub-grants from one implementer to another), instances of funding to CVA projects that are classified as ‘Full’ and thereby fully included in terms of their CVA amounts also include programming costs. For financial flows with planning information on the share of the project budget for CVA, it is possible that those shares represent transfer values only. However, it is not straightforward to ascertain this, given the CVA project questions are often ambiguously worded and do not explicitly ask for the share of CVA in terms of transfer value or programming costs. The many instances of projects indicating CVA project budget shares of 90% or higher suggest that at least some organisations interpret this question to also refer to programming costs. This methodology therefore assumes that instances of funding on FTS to CVA include transfers and overall CVA programming costs, whether counted fully or partially according to the logic laid out below.

The logical steps for calculating CVA amounts are laid out as follows:

1. Including the full current USD amount if ‘method’ is reported as ‘Cash transfer programming (CTP)’, or if there is only one destination field cluster and that is relevant to CVA (usually multi-purpose cash)  
2. For remaining flows with relevant field clusters, including a proportion of the current USD amount if there are multiple destination field clusters, one of which is relevant to CVA. The proportion is estimated by taking the fraction of one divided by the number of total field clusters reported for that financial flow.  
3. For remaining flows with CVA project budget shares, including the current USD amount multiplied by those budget shares.  
4. For remaining flows with a predicted CVA relevance from the machine learning model, including all of the current USD amount if the predicted confidence for CVA relevance is 80% or higher and if the text field includes keywords such as cash/voucher/cva.  
5. Remaining flows with a predicted CVA relevance from the machine learning model of 50% or higher that do not meet the criteria of the previous step are compiled as list of financial flows that require a manual review of their text field for whether they seem to fully or partially support CVA, or whether they represent false positives, or whether the text is insufficient to make that assessment (then also excluded).

To save time when executing this guide and methodology, the following script already joins the manual review decisions from former DI staff up to 2023 to the file. The remaining flows that have not yet coded and that remain after step 5 need to be then reviewed and classified manually. The following code chunk executes those steps:

<details>

<summary>09_calculate_cva.R lines 1-73</summary>

```R
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
  fts_flagged$amountUSD[which(fts_flagged$CVAamount == 0 & !is.na(fts_flagged$project_cva_percentage))] *
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

```

</details>

The following code chunk uses the output of the manual review to enhance the training data for the machine learning model:

<details>

<summary>09_calculate_cva.R lines 74-87</summary>

```R

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

```

</details>

Once the flows have been manually reviewed, they can be read into the R environment and merged with the fts\_cva dataset:

<details>

<summary>09_calculate_cva.R lines 88-end</summary>

```R
# Manual file is filled out prior to this step
fts_flagged_output = subset(fts_flagged, CVAamount > 0 & is.finite(CVAamount))
if(file.exists("output/cva_manually_classified.csv")){
  fts_manually_classified = fread("output/cva_manually_classified.csv")
  fts_cva = rbind(fts_flagged_output, fts_manually_classified)
}else{
  fts_cva = fts_flagged_output
}

fwrite(fts_cva, "output/fts_cva.csv")

```

</details>

This then completes the process for compiling a fully coded dataset of estimated funding amounts to humanitarian CVA based on FTS flow and project planning data.

# CVA data analysis

## Global estimated volumes of humanitarian CVA

The primary use of the above data analysis, adapted from its early iteration from the [‘Counting Cash’ paper](https://odi.org/en/publications/counting-cash-tracking-humanitarian-expenditure-on-cash-based-programming/) in 2016, is to calculate an estimate of the global value of humanitarian cash and voucher assistance delivered in any given year. There are usually two sets of figures:

1. The overall programming costs, including transfer values, for delivering CVA. The rationale behind calculating this is that funding is required for more than just the CVA transfer value to facilitate those transfers.   
2. The transfer values of the delivered CVA, disaggregated if possible by cash and vouchers.

The CVA survey requests both from implementing agencies, though only a small share of respondents provides both and most only provide data on the transfer values. FTS data, as mentioned above, is assumed to provide an indication of funding amounts to overall CVA programming. Given the need to therefore be able to convert CVA programming costs into transfer values and vice versa, every year the methodology was executed in the past, a percentage was calculated based on data from organisations that provided both in their surveys (programming costs and transfer values) of what the ratio was from the latter to the former for the entire sample. This is included as a third tab in the survey data file. For organisations that did not provide CVA programming costs in the survey data, this set of percentages is used to calculate the estimated programming costs for each provided transfer value.

The sub-grant data in the survey file includes a column ‘Take out’, which indicates whether the recipient organisation of each sub-grant has also provided survey data and therefore should be taken into consideration to avoid double-counting when aggregating that survey data. Filling in this column has in the past been a manual process of first reviewing all survey submissions (in the ‘Survey\_data’ tab) and then going through the received data on sub-grants line by line to highlight which of the sub-grant recipients also provided survey data.

The following code reads in the survey data into the R environment so that it can be analysed and combined with the fts\_cva data:

<details>

<summary>10_global_cva_analysis.R lines 1-37</summary>

```R
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

```

</details>

To be able to analyse the FTS data along the survey data, we also need to add another reference file that identifies which FTS organisations have provided CVA survey data in any of the years and another file that aligns the implementing organisation types on FTS with the categories used in the final CVA analysis output:

<details>

<summary>10_global_cva_analysis.R lines 38-52</summary>

```R
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

```

</details>

What then remains is to follow the steps below to aggregate the CVA data in both datasets while avoiding double-counting:

1. Starting with the survey data, we aggregate all of the programming costs and the transfer values by organisation type and years.  
2. From the calculated program costs, we subtract all the aggregate sub-grant values by source organisation type and year that were highlighted to be taken out to avoid double-counting. We remove double-counted funding by source organisation type to ensure that they are allocated to the organisation type that does the last mile of CVA delivery. For the calculated transfer values in step 1, we do the same but multiply the sub-grant amounts by the corresponding percentage of the transfer value/programming cost estimate for each year.  
3. For FTS data, we aggregate all the CVA amounts by cva\_org\_type (to match survey data) and year, ensuring we only include data for destination organisations in years for which those organisations did not also submit a survey. This provides the programming costs CVA estimate from FTS data additional to the CVA survey. We multiple those values by the corresponding percentage of the transfer value/programming cost estimate for each year to obtain the FTS estimates on CVA transfer values supported by this funding.  
4. We add the values calculated from survey and FTS data for both programming costs and transfer values by organisation type and year to obtain the final set of global CVA estimates.

The following chunk of codes executes these aggregation steps:

<details>

<summary>10_global_cva_analysis.R remaining code</summary>

```R
# Aggregate FTS for joining
fts_cva_agg = fts_cva[,.(PC.USD.m=sum(CVAamount) / 1e6), by=.(
  year,
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

# Aggregate
sub_grants_agg = data.table(sub_grants)[
  ,.(PC.USD.m_subgrant=sum(Amount.USD, na.rm=T)),
  by=.(clean_org, Year)
]
sub_grants_agg = subset(sub_grants_agg, !is.na(clean_org))

# Merge and subtract
cva_agg = merge(cva_agg, sub_grants_agg, by=c("clean_org", "Year"), all.x=T)
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

```
</details>

## CVA data by cluster

There are two main avenues to explore CVA data by cluster with the data compiled in this guide. 

The first involves the FTS CVA dataset generated above. Within that, we could analyse funding for CVA by cluster across a large number of contexts. The ‘destinationObjects\_GlobalCluster.name’ column standardises the field clusters into a set of global clusters and allows for easier analysis (though currently there is no designated ‘multi-purpose cash’ category within that column). However, given none of the possible ways of reporting on CVA to FTS are used comprehensively or consistently, this data will inevitably be partial. It might therefore be harder to use as a basis for advocacy with clusters to change the share of CVA within each cluster if that true share is only partially known.

The second possible way involves using the project planned budget data ([see above](#projects-module-cva-fields)). Thie has the advantage that it provides a complete account of planning figures for each HRP with available data (usually over 20 plans each year). This should therefore provide information with fairly high confidence about the planned significance of CVA by cluster in those response plans. This data would be available alongside information on which organisations or organisation types plan for smaller or greater shares of CVA within their activities, and what those activities are. It also has the advantage for being planning data that it can be used for forward-looking advocacy on activities that are yet to be implemented, unlike most other CVA data, which is mostly retrospective.

## CVA data by country

The dataset compiled above from FTS and projects data allows for a partial analysis of global CVA volumes by country. The comprehensiveness can be improved by also incorporating data from the WFP CASHboard ([see above](#wfp-cashboard-analytics)) to get a complete representation of WFP’s data. This would then require excluding WFP from the FTS data to avoid double-counting. The disadvantage for the WFP data is however that it does not include donor information.

## Relative share of CVA as % of IHA

There tends to be interest, especially from within the CALP Network, on what share of international humanitarian assistance (IHA) is delivered as CVA. Calculating this is currently flawed and only a best estimate given it involves comparing financial inputs to the humanitarian system with its outputs (with CVA a delivery modality). Given the lack of comprehensive public reporting on how much and when humanitarian activities funding from international funding deliver CVA, there is no sufficient data to connect the two. There can also likely be a mismatches (both ways) of funding being disbursed by a donor in one year and it being delivered as CVA in another, making it harder to compare data on financial inputs and outputs in the same year.

Still, given the demand for this calculation, the method used so far to do it was to take the global CVA estimate for programming costs produced in this guide ([see above](#global-estimated-volumes-of-humanitarian-cva)) and to divide it by the best possible estimate of IHA available for each year. This also requires first excluding some CVA survey data that would not be included by IHA funding data (e.g. CVA delivered domestically by RCRC national societies in donor countries).

The IHA figures used are those calculated by DI, which are partly based on a labour-intensive process to compile data on private humanitarian funding alongside an analysis of donor funding amounts based on mostly DAC data for DAC member donors and otherwise FTS data for other donors. IHA figures need to be converted to current prices to be comparable to CVA figures, which have not been deflated in this methodology ([see above](#deflators)). The historical IHA data in current prices calculated by DI is provided alongside this guide to aid calculating this percentage.

# Methodological limitations

Survey data provides very few data points to further investigate CVA data (e.g., lack of donor, country or cluster data).

FTS and project data as main alternative data sources with much more contextual data are inconsistently reported in terms of CVA data and therefore only provides a partial picture across all dimensions. 

Further, FTS represents transfers between organisations (unable to easily capture transfer values in its current state) and project data represents planning figures. When project data merged with FTS, only works for plans with the relevant questions, and only around 60% of funding to those plans reported with project IDs.

# Suggestions for future improvements

Better reporting by implementers \- actually follow the minimum agreements as agreed, especially by publishing comprehensive and timely CVA data to 

Point to the CVA collected by cash working groups and how that could be improved through better interoperability with, e.g., FTS data

Social protection payments in crisis countries \- no comparable global data source, but might be possible to incorporate in specific contexts with available data

Technically:

* Automate the de-duplication of subgrants (Alex already did most of the coding work on this)  
* Change sequence of logical steps for CVA amount calculation  
* Can think about deflating CVA given the time horizon is extending so far but not sure how to best do that \- CVA as % of IHA estimate gets around that somewhat but also a flawed calculation ([see above](#relative-share-of-cva-as--of-iha))  
* How to handle anonymous reporting on FTS \- e.g., international NGO’s (confidential)

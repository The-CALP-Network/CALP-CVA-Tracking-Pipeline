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

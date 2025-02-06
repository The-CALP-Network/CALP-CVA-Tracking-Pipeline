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

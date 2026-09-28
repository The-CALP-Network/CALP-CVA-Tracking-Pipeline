# 11_country_cva_analysis.R
#
# Inputs:
# output/fts_cva.csv
# reference_datasets/cva_survey_data.xlsx (sheets: Survey_data, 2, 3)
# reference_datasets/fts_survey_overlap.csv
# reference_datasets/cva_org_type.csv
#
#
# Run from the project root:
# Rscript code/11_country_cva_analysis.R

source("code/util/utils.R")
enforce_project_root()
load_packages("data.table", "openxlsx", "stringdist")

years <- 2020:2025

# Load FTS CVA flows
fts_cva <- fread("output/fts_cva.csv")
fts_cva <- fts_cva[as.integer(year) >= 2020L]

# Load CVA agg
cva_agg <- fread("output/cva_agg.csv")

#Load full FTS
fts <- rbindlist(lapply(years, function(yr)
  fread(paste0(
    "fts/fts_curated_", yr, ".csv"
  ))),
  use.names = T,
  fill = T)
fts <- fts[as.integer(year) >= 2020L]

# Load name mapping
fts_survey_overlap <- fread("reference_datasets/fts_survey_overlap.csv", header = T)

fts_cva <- merge(fts_cva, fts_survey_overlap, by = "destinationObjects_Organization.name", all.x = T)
fts <- merge(fts, fts_survey_overlap, by = "destinationObjects_Organization.name", all.x = T)

fts_cva[is.na(`Survey name`), `Survey name` := destinationObjects_Organization.name]
fts[is.na(`Survey name`), `Survey name` := destinationObjects_Organization.name]

country_org_cva <- fts_cva[, .(total_cva = sum(CVAamount), local_cva = sum(CVAamount[destination_ngotype %in% c("National NGO", "Local NGO", "Local/National NGO") | destination_orgtype %in% c("National Government", "National government")])), by = .(year, Organisation = `Survey name`, destination_org_iso3)]
country_org_hum <- fts[, .(total_hum = sum(amountUSD_defl)), by = .(year,  Organisation =`Survey name`, destination_org_iso3)]

country_org_cva <- country_org_cva[country_org_hum, on = .(year, Organisation, destination_org_iso3)]

cva_agg_tot <- cva_agg[Year >= 2020, .(PC = sum(PC.USD.m_undoubled, na.rm = T), TV = sum(TV.USD.m_undoubled, na.rm = T)), by = .(year = Year, Organisation)]

country_org_cva_agg <- merge(country_org_cva, cva_agg_tot, by = c("year", "Organisation"), all.x = T)

country_org_cva_agg[, `:=` (cva_prop = total_cva/sum(total_cva, na.rm = T), local_cva_prop = local_cva/sum(total_cva, na.rm = T), hum_prop = total_hum/sum(total_hum, na.rm = T)), by = .(year, Organisation)]

country_cva <- country_org_cva_agg[, .(localPC = sum(PC*local_cva_prop, na.rm = T), PC = sum(PC*cva_prop, na.rm = T), localTV = sum(TV*local_cva_prop, na.rm = T), TV = sum(TV*cva_prop, na.rm = T), Hum = sum(total_hum, na.rm = T)), by = .(year, destination_org_iso3)]

fwrite(country_cva, "output/country_cva.csv")

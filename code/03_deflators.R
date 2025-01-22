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

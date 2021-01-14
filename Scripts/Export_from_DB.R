# Script exports climatological data from the database
# Libraries ---------------------------------------------------------------

library(RODBC)
library(tidyverse)


# Exporting data ----------------------------------------------------------

export_data <- function(parameters, stacijas = NA) {
  con <- source("P:/KMN/Kodi/Clidata_connection.r")
  parameters_for <- paste0("'", parameters, "',", collapse = " ") %>% str_sub(1, nchar(.)-1)
  if (!is.na(stacijas)) {
    stacijas_for <- paste0("'", stacijas, "',", collapse = " ") %>% str_sub(1, nchar(.)-1)
    stacijas_condition <- paste0(" AND RDATA.EG_GH_ID IN (", stacijas_for, ")")
  } else {
    stacijas_condition <- ""
  }
  query <- paste0("SELECT * FROM CLIDATA.RDATA WHERE RDATA.EG_EL_ABBREVIATION IN (",
                  parameters_for, ")", stacijas_condition, ";")
  data <- sqlQuery(con[[1]], query, stringsAsFactors = F, dec = ",")
  odbcClose(con[[1]])
  return(data)
}

parameters <- c("HTDRY", "TDRY", "MTDRY")
temp_data <- export_data(parameters = parameters)
write_rds(temp_data, "Dati/Temp_dati_neapstradati.rds")

nokr_parameter <- c("HPRAB", "PRAB", "MPRAB")
nokr_data <- export_data(parameters = nokr_parameter)
write_rds(nokr_data, "Dati/nokr_dati_neapstradati.rds")

min_temp_par <- c("ATMN", "HATMN", "MATMN")
min_temp_data <- export_data(parameters = min_temp_par)
write_rds(min_temp_data, "Dati/min_temp_dati_neapstradati.rds")

max_temp_par <- c("ATMX", "HATMX", "MATMX")
max_temp_data <- export_data(parameters = max_temp_par)
write_rds(max_temp_data, "Dati/max_temp_dati_neapstradati.rds")

mean_wind <- c("HMWNS", "HWNDS", "WNS10", "MWNDS")
mean_wind_data <- export_data(parameters = mean_wind)
write_rds(mean_wind_data, "Dati/mean_wind_dati_neapstradati.rds")

max_wind_par <- c("HWSMX", "WPGSB", "MWSMX")
max_wind_data <- export_data(max_wind_par)
write_rds(max_wind_data, "Dati/max_wind_dati_neapstradati.rds")


# Export metadata ---------------------------------------------------------

con <- source("P:/KMN/Kodi/Clidata_connection.r")

query <- paste0(
  "select * from st_observation
  order by eg_gh_id")

metadata <- sqlQuery(con[[1]], query,
                     stringsAsFactors = F)

write_rds(metadata, "Dati/station_metadata.rds")
odbcClose(con[[1]])

#### Geography data ####
query <- paste0(
  "select * from geography
  where gh_id in (",
  stac_query,
  ")
  order by gh_id")

geography_data <- sqlQuery(channel[[1]], query,
                           stringsAsFactors = F)

write_rds(geography_data, "Data/geog_data.rds")
odbcClose(channel[[1]])


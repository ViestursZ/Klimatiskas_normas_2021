# Eksportē cross-referencotos temperatŗuas datus no datubāzes
# Libraries ---------------------------------------------------------------

library(RODBC)
library(tidyverse)

# Exporting data ----------------------------------------------------------

export_data <- function(parameters, stacijas = NA, regular = "Y") {
  # parameters <- "TDRY"
  # stacijas <- NA
  # regular <- "Y"
  con <- source("P:/KMN/Kodi/Clidata_connection.r")
  parameters_for <- paste0("'", parameters, "',", collapse = " ") %>% str_sub(1, nchar(.)-1)
  if (!is.na(stacijas)) {
    stacijas_for <- paste0("'", stacijas, "',", collapse = " ") %>% str_sub(1, nchar(.)-1)
    stacijas_condition <- paste0(" AND RDATA.EG_GH_ID IN (", stacijas_for, ")")
  } else {
    stacijas_condition <- ""
  }
  query <- paste0("SELECT EG_GH_ID, EG_EL_ABBREVIATION, year, month, DAY, time, VALUE FROM clidata.v_day_all_null WHERE EG_EL_ABBREVIATION IN (",
                  parameters_for, ")", stacijas_condition, " and REGULAR = '", regular, "';")
  data <- sqlQuery(con[[1]], query, stringsAsFactors = F, dec = ",")
  odbcClose(con[[1]])
  return(data)
}


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
con <- source("P:/KMN/Kodi/Clidata_connection.r")

query <- paste0(
  "select * from geography
  order by gh_id")

geography_data <- sqlQuery(con[[1]], query,
                           stringsAsFactors = F)

write_rds(geography_data, "Dati/geog_data.rds")
odbcClose(con[[1]])



# Mean T ------------------------------------------------------------------

TDRY_reg_data <- export_data(parameters = "TDRY")
write_rds(TDRY_reg_data, "Dati/TDRY_reg_export.rds")
TDRY_unreg_data <- export_data(parameters = "TDRY", regular = "N")
write_rds(TDRY_unreg_data, "Dati/TDRY_unreg_export.rds")
HTDRY_unreg_data <- export_data(parameters = "HTDRY")
write_rds(TDRY_unreg_data, "Dati/HTDRY_unreg_export.rds")



# Minimālā temperatūra ----------------------------------------------------

ATMN_reg_data <- export_data(parameters = "ATMN")
write_rds(ATMN_reg_data, "Dati/ATMN_reg_export.rds")
ATMN_unreg_data <- export_data(parameters = "ATMN", regular = "N")
write_rds(ATMN_unreg_data, "Dati/ATMN_unreg_export.rds")
HATMN_unreg_data <- export_data(parameters = "HATMN")
write_rds(HATMN_unreg_data, "Dati/ATMN_unreg_export.rds")



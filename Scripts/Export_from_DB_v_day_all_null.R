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

TDRY_reg_data <- export_data(parameters = "TDRY")
write_rds(TDRY_reg_data, "Dati/TDRY_reg_export.rds")
TDRY_unreg_data <- export_data(parameters = "TDRY", regular = "N")
write_rds(TDRY_unreg_data, "Dati/TDRY_unreg_export.rds")
HTDRY_unreg_data <- export_data(parameters = "HTDRY")
write_rds(TDRY_unreg_data, "Dati/HTDRY_unreg_export.rds")

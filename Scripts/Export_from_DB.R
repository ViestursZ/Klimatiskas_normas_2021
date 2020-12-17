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

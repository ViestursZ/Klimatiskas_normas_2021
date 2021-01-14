# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")
source("Scripts/Tidy_daily.r")
source("Scripts/Tidy_hourly.r")

# Read in data ------------------------------------------------------------
# Pagaidām tikai gaisa temperatūras dati
temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")
min_temp_data <- read_rds("Dati/min_temp_dati_neapstradati.rds")
max_temp_data <- read_rds("Dati/max_temp_dati_neapstradati.rds")


# Datu kārtošana ----------------------------------------------------------
# Kādi regular parametri ir?
unreg_pars <- c("TDRY", "ATMX", "ATMN")

# Atsevišķi katram parametram
temp_data <- select(temp_data, 1:37)

# Cleaning funkcija
precleaning <- function(data, unreg_pars) {
  # Removes unnecessary stations and flag columns
  data <- data %>%
    select(1:37) %>%
    filter(!str_detect(EG_GH_ID, "X"))
  
  # Splits daily and term data
  unreg <- data %>%
    filter(REGULAR == "N") %>%
    filter(EG_EL_ABBREVIATION %in% unreg_pars)
  reg <- data %>%
    filter(REGULAR == "Y")
  llist <- list("ur" = unreg, "r" = reg)
}

temp_data <- temp_data %>%
  precleaning(unreg_pars = unreg_pars)

# Tidy datasets
tidy_datasets <- function(data, funreg = "AVG") {
  # Tidies both datasets. Calls modified tidy daily and tidy hourly functions
  # funreg          Filters out only this parameter in unregular data. Mostly 
  #                 there to filter out errors in TIME column in CLIDATA
  data$ur <- data$ur %>%
    filter(TIME == funreg)
  
  data$ur <- data$ur %>%
    tidy_daily()
  data$r <- data$r %>%
    tidy_hourly()
  return(data)
}

temp_data <- temp_data %>%
  tidy_datasets(funreg = "AVG")

temp_data$r


# Pievieno instrūkstošās dienas
datumi <- seq.Date(min(data_ur$Datums), max(data_ur$Datums), by = "day")
datumi <- data.frame(Datums = datumi)

data_ur <- data_ur %>%
  spread(Stacija, Merijums) %>%
  left_join(datumi, .) %>%
  gather(Stacija, Merijums, -Parametrs, -Datums)

# Apstrādā regular datus
data_r <- data_r %>%
  tidy_hourly()

r_params <- unique(data_r$Parametrs)
data_r_ls <- map(r_params, ~ filter(data_r, Parametrs == .x))

names(data_r_ls) <- r_params
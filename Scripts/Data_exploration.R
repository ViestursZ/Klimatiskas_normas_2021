# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")

# Read in data ------------------------------------------------------------

temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")

# Sakārto datus -----------------------------------------------------------

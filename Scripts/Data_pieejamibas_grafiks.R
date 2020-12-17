# Izveido datu pieejamības grafikus
# Libraries ---------------------------------------------------------------

library(RODBC)
library(tidyverse)
library(magrittr)
library(lubridate)

# Ielādē funkcijas --------------------------------------------------------



# Ielādē datus ------------------------------------------------------------

# temp_data <- read_rds("Dati/temp_data.rds")

# Sakārto datus -----------------------------------------------------------

temp_data <- select(temp_data, 1:37)

temp_data_ur <- temp_data %>%
  filter(REGULAR == "N") 

temp_data_r <- temp_data %>%
  filter(REGULAR == "Y")

# Apstrādā unregular datus
temp_data_ur <- temp_data_ur %>%
  filter(EG_EL_ABBREVIATION == "TDRY")  %>%
  filter(TIME == "AVG") 

temp_data_ur %>%
  

# Apstrādā regular datus

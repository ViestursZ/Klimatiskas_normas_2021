# Izveido datu pieejamības grafikus
# Libraries ---------------------------------------------------------------

library(RODBC)
library(tidyverse)
library(magrittr)
library(lubridate)
library(RcppRoll)

# Ielādē funkcijas --------------------------------------------------------

source("Scripts/Tidy_daily.r", encoding = "UTF-8")
source("Scripts/Tidy_hourly.r", encoding = "UTF-8")

# Ielādē datus ------------------------------------------------------------

# temp_data <- read_rds("Dati/temp_data.rds")

# Sakārto datus -----------------------------------------------------------

temp_data <- select(temp_data, 1:37)
temp_data <- temp_data %>%
  filter(!str_detect(EG_GH_ID, "X"))

temp_data_ur <- temp_data %>%
  filter(REGULAR == "N") 

temp_data_r <- temp_data %>%
  filter(REGULAR == "Y")

# Apstrādā unregular datus
temp_data_ur <- temp_data_ur %>%
  filter(EG_EL_ABBREVIATION == "TDRY")  %>%
  filter(TIME == "AVG") 

temp_data_ur <- temp_data_ur %>%
  tidy_daily() 

# Pievieno instrūkstošās dienas
datumi <- seq.Date(min(temp_data_ur$Datums), max(temp_data_ur$Datums), by = "day")
datumi <- data.frame(Datums = datumi)

temp_data_ur <- temp_data_ur %>%
  spread(Stacija, Merijums) %>%
  left_join(datumi, .) %>%
  gather(Stacija, Merijums)

# Apstrādā regular datus
temp_data_r <- temp_data_r %>%
  tidy_hourly()

r_params <- unique(temp_data_r$Parametrs)
temp_data_r_ls <- map(r_params, ~ filter(temp_data_r, Parametrs == .x))

names(temp_data_r_ls) <- r_params

mer_skaits_test <- temp_data_r_ls[[1]] %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Mer_skaits = n())

mer_skaits_test %>%
  arrange(Stacija, Datums) %>%
  group_by(Stacija) %>%  
  mutate(Mer_skaits_roll = round(roll_meanr(Mer_skaits, n = 30))) %$%
  table(Mer_skaits_roll)

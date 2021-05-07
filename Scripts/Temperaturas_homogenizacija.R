# Skripts sagatavo temperatūras datus homogenizācijai
# Libraries ---------------------------------------------------------------

library(climatol)
library(tidyverse)
library(lubridate)
library(magrittr)

# Funkcijas ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/aggregation_funcs.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Ielādē sakārtotos temperatūras datus ------------------------------------

temp_dati <- read_csv("Dati/Temp_dati_clean.csv")

## NEPIESKAITĪJU ZIEMAS LAIKU
temp_d <- temp_dati %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_mean_na(Merijums, percentage_na = 0.2, consec_values = F), 1))

# Atfiltrē liekās stacijas
temp_d <- temp_d %>%
  filter(!Stacija %in% c("ADAZI", "SELIEPA", "SEVENTSP", "REZEKNE", "RIGAM165", "TEST10M"))

# Kādas ir stacijas
data_stacs <- temp_d %$% unique(Stacija)

# Jāapvieno Rīgas
temp_d <- temp_d %>%
  spread(Stacija, Merijums) %>%
  mutate(RIGASLU = ifelse(is.na(RIGASLU), RIAS99PA, RIGASLU)) %>%
  select(-RIAS99PA)

# Jāapvieno Dagdas un Rēzeknes
temp_d <- temp_d %>%
  mutate(RIREZEKN = ifelse(is.na(RIREZEKN), RIRE99MS, RIREZEKN)) %>%
  mutate(RIDAGDA = ifelse(is.na(RIDAGDA), DAGDA, RIDAGDA)) %>%
  select(-DAGDA, -RIRE99MS)

# Pievieno iztrūkstosos datumus
iztr_dateseq <- seq(min(temp_d$Datums), max(temp_d$Datums),
                    by = "day")
iztr_df <- data.frame(Datums = iztr_dateseq)

temp_d <- temp_d %>% left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) 


data_stacs <- data_stacs[!data_stacs %in% c("RIAS99PA", "RIRE99MS", "RIDAGDA")]

temp_d %>%
  write_csv("Dati/MeanT_daily.csv")

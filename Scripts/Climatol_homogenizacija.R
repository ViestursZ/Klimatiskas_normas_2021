# Climatol homogenizācija
# Libraries ---------------------------------------------------------------

library(climatol)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)

# Sakārto datus -----------------------------------------------------------

temp_daily <- read_csv("Dati/MeanT_daily.csv")

temp_daily <- temp_daily %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums))

# Unikālās stacijas
data_stacs <- temp_daily$Stacija %>% unique()

# Ielādē koordinātu datus priekš CLIMATOL
visas_stacijas <- st_read("Dati/Stacijas_sampl_LKS_25_02_2020.gpkg")

temp_stacs <- visas_stacijas %>%
  filter(GH_ID %in% data_stacs)

temp_stacs <- temp_stacs %>%
  st_transform(4326)

temp_coords <- st_coordinates(temp_stacs) %>%
  as.data.frame() %>%
  set_colnames(c("Lon", "Lat"))

climatol_temp_coords <- temp_stacs %>%
  bind_cols(temp_coords) %>%
  select(GH_ID, ELEVATION, Lon, Lat) %>%
  as.data.frame()

climatol_temp_coords <- climatol_temp_coords %>%
  select(-geom) %>%
  select(Lon, Lat, ELEVATION, GH_ID) %>%
  mutate(Lon = round(Lon, 4),
         Lat = round(Lat, 4))

climatol_temp_coords <- climatol_temp_coords %>%
  arrange(GH_ID)


# Nomet liekās stacijas ---------------------------------------------------

# Climatol nav tik draudzīgs pret datu iztrūkumiem kā ACMANT, tādēļ nepieciešams
# atfiltrēt rindu, kurā iztrūkst mērījumi
temp_daily %>%
  mutate(Gads = year(Datums)) %>%
  group_by(Stacija, Gads) %>%
  summarise(NA_skaits = sum(is.na(Merijums))) %>%
  spread(Stacija, NA_skaits) %>%
  View()

# Tādi normāli mērījumi sākas no 1927. gada. 
climatol_temp_daily <- temp_daily %>%
  filter(year(Datums) >= 1927)

# Nost metamās stacijas
navajag_clim_stacs <- climatol_temp_daily %>%
  group_by(Stacija) %>%
  summarise(Mer_skaits = n(),
            NA_skaits = sum(is.na(Merijums))) %>% 
  filter(NA_skaits >= 11444) %>%
  pull(Stacija)

# Pagaidām gan stacijas nost nemet

# Sakārto atbilstoši CLIMATOL formātam ------------------------------------




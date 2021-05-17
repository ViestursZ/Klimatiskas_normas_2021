# Climatol homogenizācija
# Libraries ---------------------------------------------------------------

library(climatol)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)

# Functions ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R",
       encoding = "UTF-8")

# Sakārto datus -----------------------------------------------------------

# temp_daily <- read_csv("Dati/MeanT_daily.csv")
temp_daily <- temp_d

korig_temp_daily <- korig_temp_d# Unikālās stacijas

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

climatol_temp_coords <- climatol_temp_coords %>% 
  mutate(Stacija = recode_stations(GH_ID, "ID-LV")) %>%
  mutate(Stacija = ifelse(Stacija == "Rīga Universitāte", "Rīga", Stacija)) %>%
  mutate(Stacija = ifelse(Stacija == "RIGAAIR", "Rīga-Lidosta", Stacija))

# Nomet liekās stacijas ---------------------------------------------------

# Climatol nav tik draudzīgs pret datu iztrūkumiem kā ACMANT, tādēļ nepieciešams
# atfiltrēt rindu, kurā iztrūkst mērījumi

# Tādi normāli priekš CLIMATOL mērījumi sākas no 1947. gada. 
# Raw dati
climatol_temp_raw_daily <- temp_daily %>%
  filter(year(Datums) >= 1947) %>%
  arrange(Stacija, Datums) %>%
  filter(year(Datums) <= 2020)

# Nost metamās stacijas - CLIMATOL nestrādā
nevajag_clim_stacs <- climatol_temp_raw_daily %>%
  group_by(Stacija) %>%
  summarise(Mer_skaits = n(),
            NA_skaits = sum(is.na(Merijums))) %>% 
  filter(NA_skaits >= 11444) %>%
  pull(Stacija)

climatol_temp_raw_daily <- climatol_temp_raw_daily %>%
  filter(!Stacija %in% nevajag_clim_stacs)

climatol_temp_raw_coords <- climatol_temp_coords %>%
  filter(!GH_ID %in% nevajag_clim_stacs)


# Koriģētie dati
climatol_temp_cor_daily <- korig_temp_daily %>%
  filter(year(Datums) >= 1947) %>%
  arrange(Stacija, Datums) %>%
  filter(year(Datums) <= 2020)

nevajag_clim_cor_stacs <- climatol_temp_cor_daily %>%
  group_by(Stacija) %>%
  summarise(Mer_skaits = n(),
            NA_skaits = sum(is.na(Merijums))) %>% 
  filter(NA_skaits >= 11444) %>%
  pull(Stacija)

climatol_temp_cor_daily <- climatol_temp_cor_daily %>%
  filter(!Stacija %in% nevajag_clim_cor_stacs)

climatol_temp_cor_coords <- climatol_temp_coords %>%
  filter(!GH_ID %in% nevajag_clim_cor_stacs)



# Pagaidām gan stacijas nost nemet
# Sakārto un ieraksta datus atbilstoši CLIMATOL formātam -----------------------
write(climatol_temp_raw_daily$Merijums, "Dati/Climatol_data/LVMeanTraw_1947-2020.dat", 
      ncolumns = 5) # Ieraksta Climatol formātā

climatol_temp_raw_coords %>%
  write.table("Dati/Climatol_data/LVMeanTraw_1947-2020.est",
              row.names = F, col.names = F) # Ieraksta coords Climatol formātā

write(climatol_temp_cor_daily$Merijums, "Dati/Climatol_data/LVMeanTcor_1947-2020.dat",
      ncolumns = 5)
climatol_temp_cor_coords %>%
  write.table("Dati/Climatol_data/LVMeanTcor_1947-2020.est", row.names = F, col.names = F)


# Climatol raw datu homogenizācija -------------------------------------------------
# setwd("./Dati/Climatol_data")
# 
# # No sākuma viens exploratory analysis run
# homogen("LVMeanTraw", 1947, 2020, expl = T)
# outrename("LVMeanTraw", 1947, 2020, "expl_analysis")
# 
# # homogen("LVMeanT", 1947, 2020)
# # outrename("LVMeanT", 1947, 2020, "test")
# # setwd("../../")
# 
# # Mēnešu sērijas, jo dienu sērijas utterly failoja
# dd2m("LVMeanTraw", 1947, 2020)
# homogen("LVMeanTraw-m", 1947, 2020)
# homogen("LVMeanTraw", 1947, 2020, dz.max = 7, metad = T)
# outrename("LVMeanTraw", 1947, 2020, "daily_monbrks")
# 
# setwd("../../")

# Climatol koriģēto datu homogenizācija -----------------------------------

setwd("./Dati/Climatol_data")

# No sākuma viens exploratory analysis run
homogen("LVMeanTcor", 1947, 2020, expl = T)
outrename("LVMeanTcor", 1947, 2020, "expl_analysis")


# Mēnešu sērijas, jo dienu sērijas utterly failoja
dd2m("LVMeanTcor", 1947, 2020)
homogen("LVMeanTcor-m", 1947, 2020)
homogen("LVMeanTcor", 1947, 2020, dz.max = 7, metad = T)
outrename("LVMeanTcor", 1947, 2020, "daily_monbrks")

setwd("../../")


# Test run over -----------------------------------------------------------



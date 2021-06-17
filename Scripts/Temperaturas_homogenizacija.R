# Skripts sagatavo temperatūras datus homogenizācijai
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)
library(readxl)
library(plotly)

# Funkcijas ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/aggregation_funcs.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Ielādē sakārtotos temperatūras datus ------------------------------------

temp_dati <- read_csv("Dati/Mean_T_dati_clean.csv")


# Ielādē ar iztrūkumiem aizvietotās stacijas ------------------------------
# locale = locale(decimal_mark = ".", date_format = "%m/%d/%Y", time_format = "%H:%M"

RIGASLU <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/RIGASLU_dati_VIESTURAM.csv",
                      delim = ",", col_types = "nnnnn")
RIGASLU <- RIGASLU %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            RIGASLU_new)

MADONA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/RIMADONA_dati_VIESTURAM.csv",
                     delim = ",", col_types = "nnnnn")
MADONA <- MADONA %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            RIMADONA_new)

DAGDA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/RIDAGDA_dati_VIESTURAM.csv",
                    delim = ",", col_types = "nnnnn")
DAGDA <- DAGDA %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            RIDAGDA_new)

# REZEKNE <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/temp_REZEKNE_aiz.csv",
#                       delim = ";", col_types = "cn")
# REZEKNE <- REZEKNE %>%
#   mutate(DATE = mdy_hm(DATE))

RUCAVA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/RUCAVA_dati_VIESTURAM.csv",
                     delim = ",", col_types = "nnnnn")
RUCAVA <- RUCAVA %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            RUCAVA_new)


# Izveido koriģēto datu kopu ----------------------------------------------

DAGDA <- DAGDA %>% 
  mutate(Stacija = "DAGDA") %>%
  rename(Datums_laiks = DATE, Merijums = RIDAGDA_new)

MADONA <- MADONA %>%
  mutate(Stacija = "RIMADONA") %>%
  rename(Datums_laiks = DATE, Merijums = RIMADONA_new)

RIGASLU <- RIGASLU %>%
  dplyr::select(Datums_laiks = DATE, Merijums = RIGASLU_new) %>%
  mutate(Stacija = "RIGASLU")

# REZEKNE <- REZEKNE %>%
#   mutate(Stacija = "RIREZEKN") %>%
#   rename(Datums_laiks = DATE, Merijums = Rezekne_atj)

RUCAVA <- RUCAVA %>%
  mutate(Stacija = "RUCAVA") %>%
  rename(Datums_laiks = DATE, Merijums = RUCAVA_new)

korig_dati <- temp_dati

DAGDA_korig <- korig_dati %>% 
  filter(Stacija %in% c("DAGDA")) %>%
  filter(Datums_laiks < ymd("1947-09-01")) %>%
  bind_rows(DAGDA) %>%
  arrange(Datums_laiks, Merijums) %>%
  mutate(Stacija = "RIDAGDA")
  
MADONA_korig <- korig_dati %>% 
  filter(Stacija %in% c("RIMADONA")) %>%
  filter(Datums_laiks < ymd("1923-10-01")) %>%
  bind_rows(MADONA) %>%
  arrange(Datums_laiks, Merijums)

RIGASLU_korig <- korig_dati %>%
  filter(Stacija %in% c("RIGASLU")) %>%
  filter(Datums_laiks < ymd("1795-01-01")) %>%
  bind_rows(RIGASLU) %>%
  arrange(Datums_laiks, Merijums)

# REZEKNE_korig <- korig_dati %>%
#   filter(Stacija %in% c("RIRE99MS")) %>%
#   filter(year(Datums_laiks) < 1991) %>%
#   bind_rows(REZEKNE) %>%
#   arrange(Datums_laiks, Merijums) %>%
#   mutate(Stacija = "RIREZEKN")

RUCAVA_korig <- korig_dati %>%
  filter(Stacija %in% c("RUCAVA")) %>%
  filter(Datums_laiks < ymd("1930-09-01")) %>%
  bind_rows(RUCAVA) %>%
  arrange(Datums_laiks, Merijums)


korig_dati <- korig_dati %>%
  filter(!Stacija %in% c("DAGDA", "RIDAGDA")) %>%
  bind_rows(DAGDA_korig) %>%
  filter(!Stacija %in% c("RIMADONA")) %>%
  bind_rows(MADONA_korig) %>%
  filter(!Stacija %in% c("RIGASLU", "RIAS99PA")) %>% 
  bind_rows(RIGASLU_korig) %>%
  # filter(!Stacija %in% c("RIRE99MS", "RIREZEKN")) %>%
  # bind_rows(REZEKNE_korig)  %>%
  filter(!Stacija == "RUCAVA") %>%
  bind_rows(RUCAVA_korig)


# Aprēķina diennakts datus ------------------------------------------------

## PIESKAITA ZIEMAS LAIKU
temp_d <- temp_dati %>%
  mutate(Datums_laiks = Datums_laiks + hours(2)) %>% # LV ziemas laiks 
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_mean_na(Merijums, percentage_na = 0.2, consec_values = F), 1))

korig_temp_d <- korig_dati %>%
  mutate(Datums_laiks = Datums_laiks + hours(2)) %>% # LV ziemas laiks 
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_mean_na(Merijums, percentage_na = 0.2, consec_values = F), 1))

# Ielādē datu pagarināšanas exceli, datu salīdzināšanai -------------------
# temp_d_old <- read_excel("//dc03/pd/KMN/Projekti/Projekts-KLIMATS/DATU_Pagarinasana/Temperatura/Diennakts_vid_temperatura_1961-2020.xlsx")
# 
# temp_old_Ainazi <- temp_d_old %>%
#   mutate(Datums = ymd(str_c(Gads, Menesis, Datums, sep = "-"))) %>%
#   select(Datums, Ainazi)
# 
# temp_new_Ainazi <- temp_d %>%
#   filter(Stacija == "RIAI99PA") %>%
#   select(Datums, Merijums)
# 
# # Salīdzina divas stacijas
# 
# temp_salidz <- inner_join(temp_old_Ainazi, temp_new_Ainazi)
# temp_salidz_graph_ainazi <- temp_salidz %>%
#   rename(Old = Ainazi, New = Merijums) %>%
#   mutate(Diff = New - Old)
# 
# temp_salidz_graph_ainazi <- temp_salidz_graph_ainazi %>%
#   ggplot(aes(Diff)) + 
#   geom_histogram(bins = 100, col= "black")+
#   ggtitle("Ainazi salidzinajums")
# 
# ggplotly(temp_salidz_graph_ainazi)

# Datu apstrāde -----------------------------------------------------------

# Atfiltrē liekās stacijas
temp_d <- temp_d %>%
  filter(!Stacija %in% c("ADAZI", "SELIEPA", "SEVENTSP", "REZEKNE", "RIGAM165", "TEST10M"))

korig_temp_d <- korig_temp_d %>%
  filter(!Stacija %in% c("ADAZI", "SELIEPA", "SEVENTSP", "REZEKNE", "RIGAM165", "TEST10M"))

# Kādas ir stacijas
data_stacs <- temp_d %$% unique(Stacija)
korig_stacs <- korig_temp_d %$% unique(Stacija)

# Spreado stacijas
temp_d <- temp_d %>%
  spread(Stacija, Merijums)
korig_temp_d <- korig_temp_d %>%
  spread(Stacija, Merijums)

# Nekoriģētajai temperatūrai jāapvieno Rīgas
temp_d <- temp_d %>%
  mutate(RIGASLU = ifelse(is.na(RIGASLU), RIAS99PA, RIGASLU)) %>%
  select(-RIAS99PA)

# Nekoriģētajai temperatūrai jāapvieno Dagdas un Rēzeknes
temp_d <- temp_d %>%
  mutate(RIREZEKN = ifelse(is.na(RIREZEKN), RIRE99MS, RIREZEKN)) %>%
  mutate(RIDAGDA = ifelse(is.na(RIDAGDA), DAGDA, RIDAGDA)) %>%
  select(-DAGDA, -RIRE99MS)

# Koriģētajai temperatūrai jāapvieno Rēzeknes
korig_temp_d <- korig_temp_d %>%
  mutate(RIREZEKN = ifelse(is.na(RIREZEKN), RIRE99MS, RIREZEKN)) %>%
  dplyr::select(-RIRE99MS)

# Pievieno iztrūkstosos datumus
iztr_dateseq <- seq(min(temp_d$Datums), max(temp_d$Datums),
                    by = "day")
iztr_df <- data.frame(Datums = iztr_dateseq)

temp_d <- temp_d %>% 
  left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) 

korig_temp_d <- korig_temp_d %>%
  left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) 

# Izņem Dagdu no koriģētajioem datiem ---------------------------------------

korig_temp_d_noDAGDA <- korig_temp_d %>%
  filter(Stacija != "RIDAGDA")

# Ieraksta jaunus csv failus ----------------------------------------------

# temp_d %>%
#   write_csv("Dati/MeanT_daily.csv")
# 
korig_temp_d %>%
  write_csv("Dati/MeanT_daily_korig.csv")

korig_temp_d_noDAGDA %>%
  write_csv("Dati/MeanT_daily_korig_noDAGDA.csv")



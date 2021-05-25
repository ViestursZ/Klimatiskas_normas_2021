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
parametrs <- "Min_T"
temp_dati <- read_csv("Dati/Min_T_dati_clean.csv")


# Ielādē ar iztrūkumiem aizvietotās stacijas ------------------------------
# locale = locale(decimal_mark = ".", date_format = "%m/%d/%Y", time_format = "%H:%M"

RIGASLU <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/min_temp/min_temp_RIGA_LR_data.csv",
                      delim = ",", col_types = "nnnnnnnnn")
RIGASLU <- RIGASLU %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            RIGASLU_new)

MADONA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/min_temp/min_MADONA_new.csv",
                     delim = ",", col_types = "nDnnnn")
MADONA <- MADONA %>%
  transmute(DATE = DATE,
            RIMADONA_new = MADONA_new)

DAGDA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/min_temp/min_temp_DAGDA_LR_data.csv",
                    delim = ",", col_types = paste0(rep("n", 10), collapse = ""))
DAGDA <- DAGDA %>%
  transmute(DATE = ymd(str_c(YEAR, MONTH, DAY, sep = "-")),
            DAGDA_new) %>% 
  arrange(DATE)

RUCAVA <- read_delim("//dc03/pd/KMN/_KLIMATS/KLIMATISKAS_NORMAS/Jaunas_normas/Dati/atjaunotas_temp_datu_rindas/min_temp/min_RUCAVA_new.csv",
                      delim = ",", col_types = "nDnnn")
RUCAVA <- RUCAVA %>%
  transmute(DATE = DATE,
            RUCAVA_new = RUCAVA_new)



# Izveido koriģēto datu kopu ----------------------------------------------

DAGDA <- DAGDA %>% 
  mutate(Stacija = "DAGDA") %>%
  rename(Datums_laiks = DATE, Merijums = DAGDA_new)

MADONA <- MADONA %>%
  mutate(Stacija = "RIMADONA") %>%
  rename(Datums_laiks = DATE, Merijums = RIMADONA_new)

RIGASLU <- RIGASLU %>%
  select(Datums_laiks = DATE, Merijums = RIGASLU_new) %>%
  mutate(Stacija = "RIGASLU")

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
  filter(Datums_laiks < ymd("1965-12-01")) %>%
  bind_rows(MADONA) %>%
  arrange(Datums_laiks, Merijums)

RIGASLU_korig <- korig_dati %>%
  filter(Stacija %in% c("RIGASLU")) %>%
  filter(Datums_laiks < ymd("1814-07-01")) %>%
  bind_rows(RIGASLU) %>%
  arrange(Datums_laiks, Merijums)


RUCAVA_korig <- korig_dati %>%
  filter(Stacija %in% c("RUCAVA")) %>%
  filter(year(Datums_laiks) < ymd("1930-09-01")) %>%
  bind_rows(RUCAVA) %>%
  arrange(Datums_laiks, Merijums)


korig_dati <- korig_dati %>%
  filter(!Stacija %in% c("DAGDA", "RIDAGDA")) %>%
  bind_rows(DAGDA_korig) %>% 
  filter(!Stacija %in% c("RIMADONA")) %>%
  bind_rows(MADONA_korig) %>%
  filter(!Stacija %in% c("RIGASLU", "RIAS99PA")) %>% # Atstāj RIAS99PA, lai pēcāk ievietotu laiku pirms 1991. g.
  bind_rows(RIGASLU_korig) %>%
  filter(!Stacija == "RUCAVA") %>%
  bind_rows(RUCAVA_korig)

# Izņem Dagdu -------------------------------------------------------------

korig_dati <- korig_dati %>%
  filter(Stacija != "RIDAGDA")

# Aprēķina diennakts datus ------------------------------------------------


## PIESKAITA ZIEMAS LAIKU, aprēķina diennakts mininumus
# temp_d <- temp_dati %>%
#   mutate(Datums_laiks = Datums_laiks + hours(2)) %>% # LV ziemas laiks 
#   mutate(Datums = date(Datums_laiks)) %>%
#   group_by(Stacija, Datums) %>%
#   summarise(Merijums = round(f_min_na(Merijums, percentage_na = 0), 1))

korig_temp_d <- korig_dati %>%
  mutate(Datums_laiks = Datums_laiks + hours(2)) %>% # LV ziemas laiks 
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_min_na(Merijums, percentage_na = 0), 1))

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

# Jāapvieno Rīgas
temp_d <- temp_d %>%
  spread(Stacija, Merijums) %>%
  mutate(RIGASLU = ifelse(is.na(RIGASLU), RIAS99PA, RIGASLU)) %>%
  select(-RIAS99PA)

korig_temp_d <- spread(korig_temp_d, Stacija, Merijums)

# Jāapvieno Dagdas un Rēzeknes
temp_d <- temp_d %>%
  mutate(RIREZEKN = ifelse(is.na(RIREZEKN), RIRE99MS, RIREZEKN)) %>%
  mutate(RIDAGDA = ifelse(is.na(RIDAGDA), DAGDA, RIDAGDA)) %>%
  select(-DAGDA, -RIRE99MS)

korig_temp_d <- korig_temp_d %>%
  mutate(RIREZEKN = ifelse(is.na(RIREZEKN), RIRE99MS, RIREZEKN))


# Pievieno iztrūkstosos datumus
iztr_dateseq <- seq(min(temp_d$Datums), max(temp_d$Datums),
                    by = "day")
iztr_df <- data.frame(Datums = iztr_dateseq)

temp_d <- temp_d %>% left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) 

korig_temp_d <- korig_temp_d %>%
  left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) 


# Noņem Rēzekni
korig_temp_d <- korig_temp_d %>%
  filter(Stacija != "RIRE99MS")

# data_stacs <- data_stacs[!data_stacs %in% c("RIAS99PA", "RIRE99MS", "DAGDA")]
# korig_stacs <- korig_stacs[!korig_stacs %in% c("RIAS99PA", "RIDAGDA")]

data_stacs <- temp_d %$% unique(Stacija)
korig_stacs <- korig_temp_d %$% unique(Stacija)

# Ieraksta jaunus csv failus ----------------------------------------------

# temp_d %>%
#   write_csv(paste0("Dati/", parametrs, "_daily.csv"))

# korig_temp_d %>%
#   write_csv(paste0("Dati/", parametrs, "_daily_korig.csv"))

korig_temp_d %>% 
  filter(year(Datums) == 2004) %>% 
  filter(Stacija == "RIGASLU")



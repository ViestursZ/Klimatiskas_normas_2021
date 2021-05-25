
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Maināmie parametri ------------------------------------------------------

parametrs <- "Min_T"
parametrs_acmant <- "MinTm"

# Funkcijas ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/aggregation_funcs.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Format in ACMANT format -------------------------------------------------
temp_d_spread <- temp_d %>% # Dati no temperaturas_homogenizacija.R
  mutate(Gads = year(Datums),
         Menesis = month(Datums),
         Diena = day(Datums)) %>%
  mutate(Merijums = replace_na(Merijums, -999.9)) %>% # ACMANT NA
  filter(Gads >= 1821) %>% # ACMANT ņem tikai 200 gadus
  select(-Datums) %>% 
  ungroup() %>% 
  spread(Diena, Merijums, fill = -999.9)
  

korig_temp_d_spread <- korig_temp_d %>%
  mutate(Gads = year(Datums),
         Menesis = month(Datums),
         Diena = day(Datums)) %>%
  mutate(Merijums = replace_na(Merijums, -999.9)) %>% # ACMANT NA
  filter(Gads >= 1821) %>% # ACMANT ņem tikai 200 gadus
  select(-Datums) %>% 
  ungroup() %>% 
  spread(Diena, Merijums, fill = -999.9)


# Otrs variants
# temp_d_spread <- read_csv("Dati/MeanT_daily.csv") %>%
#   mutate(Gads = year(Datums),
#          Menesis = month(Datums),
#          Diena = day(Datums)) %>%
#   filter(Gads >= 1821) %>% # ACMANT ņem tikai 200 gadus
#   select(-Datums)


# Spread the data between data frames
# temp_d_spr_st <- purrr::map(data_stacs, function(x) {filter(temp_d_spread, Stacija == x)})
# 
# temp_d_spr_st <- temp_d_spr_st %>%
#   purrr::map(~select(.x, -Stacija))
# 
# names(temp_d_spr_st) <- data_stacs
# 
# for (i in seq_along(temp_d_spr_st)) {
#   fname <- paste0("AvgTn", str_replace_all(format(i, width = 4), " ", "0"),
#                   "d.txt")
#   write_lines(data_stacs[i], paste0("Dati/ACMANT_format/", fname))
#   write_delim(temp_d_spr_st[[i]], paste0("Dati/ACMANT_format/", fname),
#               delim = " ", append = T, col_names = F)
# }



# Atlasa precīzās stacijas, kuras homogenizēt --------------------------
homog_stacijas <- c("RIAI99PA", "RIAL99MS", "RIBA99PA", "RIDAGDA","RIDM99MS", "RIDO99MS",
  "RIGASLU", "RIGU99MS", "RIJE99PA", "RIKO99PA", "RILP99PA", "RIMADONA",
  "RIME99MS", "RIPA99PA", "RIPR99PA", "RIREZEKN", "RIRU99PA", "RISA99PA",
  "RISE99MS", "RISI99PA", "RIST99PA", "RIVE99PA", "RIZI99PA", "RIZO99MS", 
  "RUCAVA")

korig_stacs <- homog_stacijas
data_stacs <- homog_stacijas


# Ieraksta ACMANT datus ---------------------------------------------------
# Spread the corrected data between data frames
korig_temp_d_spr_st <- purrr::map(korig_stacs, function(x) {filter(korig_temp_d_spread, Stacija == x)})

korig_temp_d_spr_st <- korig_temp_d_spr_st %>%
  purrr::map(~select(.x, -Stacija))

names(korig_temp_d_spr_st) <- korig_stacs

for (i in seq_along(korig_temp_d_spr_st)) {
  fname <- paste0(parametrs_acmant, str_replace_all(format(i, width = 4), " ", "0"),
                  "d.txt")
  write_lines(korig_stacs[i], paste0("Dati/ACMANT_format/", fname))
  write_delim(korig_temp_d_spr_st[[i]], paste0("Dati/ACMANT_format/", fname),
              delim = " ", append = T, col_names = F)
}


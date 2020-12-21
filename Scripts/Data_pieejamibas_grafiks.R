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

temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")

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
  gather(Stacija, Merijums, -Parametrs, -Datums)

# Apstrādā regular datus
temp_data_r <- temp_data_r %>%
  tidy_hourly()

r_params <- unique(temp_data_r$Parametrs)
temp_data_r_ls <- map(r_params, ~ filter(temp_data_r, Parametrs == .x))

names(temp_data_r_ls) <- r_params

# Iegūst mērījumu skaitu pa dienām un stacijām
mer_skaits_test <- temp_data_r_ls[[1]] %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Mer_skaits = n())

# mer_skaits_test %>%
#   arrange(Stacija, Datums) %>%
#   group_by(Stacija) %$%  
#   # mutate(Mer_skaits_roll = round(roll_meanr(Mer_skaits, n = 30))) %$%
#   table(Mer_skaits)

# Iegūst pirmos datumus, kad mērījumi TDRY parametram katrā stacijā nav NA
temp_stacijas <- unique(mer_skaits_test$Stacija)

ur_cut_dates <- map(temp_stacijas, function(x) {
  dat <- mer_skaits_test %>%
    filter(Stacija == x) %>%
    filter(!is.na(Mer_skaits)) %>%
    arrange(Datums) %>%
    slice(1) %>%
    pull(Datums)
  return(dat)
}) %>%
  unlist() %>%
  as_date()

ur_cut_dates <- tibble(Stacija = temp_stacijas, cut_date = ur_cut_dates)

# Cutto regular datus pirmajā daļā
mer_skaits_test <- map2(temp_stacijas, ur_cut_dates, function(x, y) {
  mer_skaits_test %>%
    filter(Stacija == x & Datums >= y)
})

# Cutto ur datus, līdz ur_cut_dates, kā arī croppo NA sākumā
ur_list <- map2(temp_stacijas, ur_cut_dates, function(stac, cutd) {
  temp_data_ur %>%
    filter(Stacija == stac) %>%
    filter(!is.na(Merijums)) %>%
    filter(Datums >= min(Datums)) %>% 
    filter(Datums < cutd)
})
  

ur_list
mer_skaits_test

extract_start_date <- function(df, stacija, datcol = "Datums") {
  stac_df <- filter(df, Stacija == stacija)
  datums <- stac_df %>%
    filter(Stacija == stacija) %>%
    filter(!is.na(Merijums)) %>%
    filter(!!sym(datcol) == min(!!sym(datcol))) %>%
    pull(!!sym(datcol))
  return(datums)
}

# Extracto pārejo parametru sākuma datumus
HTDRY_start <- map(temp_stacijas, extract_start_date, df = temp_data_r_ls[[2]], datcol = "Datums_laiks")
MTDRY_start <- map(temp_stacijas, extract_start_date, df = temp_data_r_ls[[2]], datcol = "Datums_laiks")

# Viss ir temp_stacijas secībā

# Grafika piemērs ---------------------------------------------------------


mer_skaits_test %>%
  filter(Stacija == "RIAI99PA") %>%
  ggplot() + 
  geom_line(aes(Datums, Mer_skaits))

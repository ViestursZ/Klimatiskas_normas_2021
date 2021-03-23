
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)

# Funkcijas ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/aggregation_funcs.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Ielādē sakārtotos temperatūras datus ------------------------------------

temp_dati <- read_csv("Dati/Temp_dati_clean.csv")

temp_dati <- temp_dati %>%
  mutate(Datums_laiks = ymd_hms(Datums_laiks))


# Aprēķina dienu vērtības -------------------------------------------------

temp_d <- temp_dati %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_mean_na(Merijums, percentage_na = 0.2, consec_values = F), 1))

# Atfiltrē liekās stacijas
temp_d <- temp_d %>%
  filter(!Stacija %in% c("ADAZI", "SELIEPA", "SEVENTSP", "REZEKNE", "RIGAM165"))

# Kādas ir stacijas
data_stacs <- temp_d %$% unique(Stacija)

# Jāapvieno Rīgas, jāatfiltrē lidosta
temp_d <- temp_d %>%
  spread(Stacija, Merijums) %>%
  mutate(RIGASLU = ifelse(is.na(RIGASLU), RIAS99PA, RIGASLU)) %>%
  select(-RIAS99PA)

# Pievieno iztrūkstosos datumus
iztr_dateseq <- seq(min(temp_d$Datums), max(temp_d$Datums),
                    by = "day")
iztr_df <- data.frame(Datums = iztr_dateseq)

temp_d <- temp_d %>% left_join(iztr_df, .,) %>%
  gather(Stacija, Merijums, -Datums) %>%
  mutate(Merijums = replace_na(Merijums, -999.9)) # ACMANT NA

data_stacs <- data_stacs[data_stacs != "RIAS99PA"]

# Format in ACMANT format -------------------------------------------------
temp_d_spread <- temp_d %>%
  mutate(Gads = year(Datums),
         Menesis = month(Datums),
         Diena = day(Datums)) %>%
  filter(Gads >= 1821) %>% # ACMANT ņem tikai 200 gadus
  select(-Datums) %>% 
  ungroup() %>% 
  spread(Diena, Merijums, fill = -999.9)
  
# Spread the data between data frames
temp_d_spr_st <- map(data_stacs, function(x) {filter(temp_d_spread, Stacija == x)})

temp_d_spr_st <- temp_d_spr_st %>%
  map(~select(.x, -Stacija))

names(temp_d_spr_st) <- data_stacs

for (i in seq_along(temp_d_spr_st)) {
  fname <- paste0("MeanT", str_replace_all(format(i, width = 4), " ", "0"),
                  "d.txt")
  write_lines(data_stacs[i], paste0("Dati/ACMANT_format/", fname))
  write_delim(temp_d_spr_st[[i]], paste0("Dati/ACMANT_format/", fname),
              delim = " ", append = T, col_names = F)
}

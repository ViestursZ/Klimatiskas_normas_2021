
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)

# Funkcijas ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/aggregation_funcs.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Format in ACMANT format -------------------------------------------------
temp_d_spread <- temp_d %>% # Dati no temperaturas_homogenizacija.R
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

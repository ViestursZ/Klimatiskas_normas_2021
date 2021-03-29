# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Ielādē ACMANT datus -----------------------------------------------------

# Listo failus
ACMANT_files <- list.files("Homogenizacija/Programmas/ACMANTv4.3/Output/", pattern = "v", full.names = T)
ACMANT_reliabilityf <- list.files("Homogenizacija/Programmas/ACMANTv4.3/Output/", pattern = "i")
ACMANT_breaksf <- "Homogenizacija/Programmas/ACMANTv4.3/Output/MeanT_breaks.txt"

ACMANT_data <- map(ACMANT_files, read_table, skip = 1, col_names = F, skip_empty_rows = F)
ACMANT_stations <- map_chr(ACMANT_files, read_lines, n_max = 1)

ACMANT_stations <- str_remove_all(ACMANT_stations, " ")

# Datu sakārtošana --------------------------------------------------------

ACMANT_data_t <- ACMANT_data %>%
  map2(ACMANT_stations, ~mutate(.x, Stacija = .y)) %>%
  map(~select(.x, Stacija, everything())) %>%
  bind_rows() %>%
  set_colnames(c("Stacija", "Gads", "Menesis", seq(1:31))) %>%
  pivot_longer(cols = paste(seq(1:31)), names_to = "Diena", values_to = "Merijums") %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums),
         Datums = ymd(str_c(Gads, Menesis, Diena, sep = "-"))) %>%
  filter(!is.na(Datums)) %>%
  select(-Gads, -Menesis, -Diena)


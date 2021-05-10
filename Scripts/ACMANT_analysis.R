# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Ielādē neapstrādātos temperatūras ACMANT homogenizētus datus ------------

# Listo failus
ACMANT_files <- list.files("Dati/ACMANT_homogenized_data/MeanT/Raw/", pattern = "v.txt$", full.names = T)
ACMANT_reliabilityf <- list.files("Homogenizacija/Programmas/ACMANTv4.3/Output/", pattern = "i.txt$")
ACMANT_breaksf <- "Homogenizacija/Programmas/ACMANTv4.3/Output/AvgTn_breaks.txt"

ACMANT_data <- purrr::map(ACMANT_files, read_table, skip = 1, col_names = F, skip_empty_rows = F)
ACMANT_stations <- purrr::map_chr(ACMANT_files, read_lines, n_max = 1)

ACMANT_stations <- str_remove_all(ACMANT_stations, " ")


# Ielādē apstrādātos temperatūras ACMANT homogenizētus datus --------------



# Datu sakārtošana --------------------------------------------------------

ACMANT_data_t <- ACMANT_data %>%
  purrr::map2(ACMANT_stations, ~mutate(.x, Stacija = .y)) %>%
  purrr::map(~select(.x, Stacija, everything())) %>%
  bind_rows() %>%
  set_colnames(c("Stacija", "Gads", "Menesis", seq(1:31))) %>%
  pivot_longer(cols = paste(seq(1:31)), names_to = "Diena", values_to = "Merijums") %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums),
         Datums = ymd(str_c(Gads, Menesis, Diena, sep = "-"))) %>%
  filter(!is.na(Datums)) %>%
  select(-Gads, -Menesis, -Diena)





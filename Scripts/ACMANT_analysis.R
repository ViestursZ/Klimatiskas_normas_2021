# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Ielādē neapstrādātos temperatūras ACMANT homogenizētus datus ------------

# Listo failus
ACMANT_files <- list.files("Dati/ACMANT_homogenized_data/MeanT/Raw/", pattern = "v.txt$", full.names = T)
ACMANT_reliabilityf <- list.files("Dati/ACMANT_homogenized_data/MeanT/Raw/", pattern = "i.txt$")
ACMANT_breaksf <- "Dati/ACMANT_homogenized_data/MeanT/Raw/AvgTn_breaks.txt"

ACMANT_data <- purrr::map(ACMANT_files, read_table, skip = 1, col_names = F, skip_empty_rows = F)
ACMANT_stations <- purrr::map_chr(ACMANT_files, read_lines, n_max = 1)

ACMANT_stations <- str_remove_all(ACMANT_stations, " ")


# Ielādē apstrādātos temperatūras ACMANT homogenizētus datus --------------
ACMANT_kor_files <- list.files("Dati/ACMANT_homogenized_data/MeanT/Corrected/", pattern = "v.txt$", full.names = T)
ACMANT_kor_reliabilityf <- list.files("Dati/ACMANT_homogenized_data/MeanT/Corrected/", pattern = "i.txt$")
ACMANT_breaksf <- "Dati/ACMANT_homogenized_data/MeanT/Corrected/AvgTn_breaks.txt"

ACMANT_kor_data <- purrr::map(ACMANT_kor_files, read_table, skip = 1, col_names = F, skip_empty_rows = F)
ACMANT_kor_stations <- purrr::map_chr(ACMANT_kor_files, read_lines, n_max = 1)

ACMANT_kor_stations <- str_remove_all(ACMANT_kor_stations, " ")


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

ACMANT_kor_data_t <- ACMANT_kor_data %>%
  purrr::map2(ACMANT_kor_stations, ~mutate(.x, Stacija = .y)) %>%
  purrr::map(~select(.x, Stacija, everything())) %>%
  bind_rows() %>%
  set_colnames(c("Stacija", "Gads", "Menesis", seq(1:31))) %>%
  pivot_longer(cols = paste(seq(1:31)), names_to = "Diena", values_to = "Merijums") %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums),
         Datums = ymd(str_c(Gads, Menesis, Diena, sep = "-"))) %>%
  filter(!is.na(Datums)) %>%
  select(-Gads, -Menesis, -Diena)



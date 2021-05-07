# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)
library(data.table)
library(RcppRoll)

# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")

clean_data <- function(data, regular, filter_params = "AVG") {
  # Cleans the clidata.v_day_all_null view and formats the dates and times in an R approproate format
  # data            data frame of data
  # regular         indicates if we are working with regulad data or no
  # filter_params   parameter aggragation in the database (such as MIN or AVG)
  if (regular == "N") {
    td <- data %>%
      filter(TIME == filter_params) %>%
      mutate(Datums = ymd(str_c(YEAR, MONTH, DAY, sep = "-"))) %>%
      select(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION, Datums, Merijums = VALUE) %>%
      arrange(Stacija, Datums)
  } else if (regular == "Y") {
    data %>%
      filter(!TIME %in% filter_params) %>%
      mutate(Datums_laiks = ymd_hm(str_c(YEAR, MONTH, DAY, TIME, sep = "-"))) %>%
      select(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION, Datums_laiks, Merijums = VALUE) %>%
      arrange(Stacija, Datums_laiks)
  }
}

H_starts <- function(data) {
  # extract the start date for hourly parameters. Uses extract_start_date function
  data %>%
    group_by(Stacija) %>%
    extract_start_date()
}

# Extract start date regular parametram
r_starts <- function(data) {
  # Extract the start date for regular parameters. Uses extract_start_date function
  data %>%
    group_by(Stacija) %>%
    extract_start_date()
}

cut_unregular_data <- function(ur_data, ur_data_starts) {
  # Cuts unregular data at the start points
  # ur_data           unregular data
  # ur_data_starts    unregular data starts 
  unregular_list <- list()
  for (i in 1:nrow(ur_data_starts)) {
    Stac <- ur_data_starts[i, "Stacija", drop = T]
    Datums_end <- ur_data_starts[i, "Datums_laiks", drop = T]
    Datums_end <- date(Datums_end)
    unregular_list[[i]] <- ur_data %>%
      filter(Stacija == Stac) %>%
      filter(Datums < Datums_end)
  }
  
  unregular_data_clean <- bind_rows(unregular_list)
  
  unregular_data_clean <- unregular_data_clean %>%
    mutate(Datums_laiks = as_datetime(Datums),
           Datums_laiks = `hour<-`(Datums_laiks, 12)) %>% # Pieliek datumam stundu, 
                                                          # lai saiet kopā ar pārējiem datiem
    dplyr::select(-Datums)
}

cut_term_data <- function(data, r_start, h_start) {
  # Cuts the term start and end dates
  # data             filtered correct term data
  # r_start          regular parameter start dates from r_starts function
  # h_start          hourly parameter start dates from H_starts function
  
  datalist_termini_2 <- list()
  for (i in 1:nrow(r_start)) {
    Stac <- r_start[i, "Stacija", drop = T]
    Datums_start <- r_start[i, "Datums_laiks", drop = T]
    Datums_end <- filter(h_start, Stacija == Stac) %>% pull(Datums_laiks)
    
    if (length(Datums_end) == 0) {
      df <- data %>%
        filter(Stacija == Stac)
    } else {
      df <- data %>%
        filter(Stacija == Stac) %>%
        filter(Datums_laiks < Datums_end)
    }
    
    if (length(Datums_start) == 0) {
      datalist_termini_2[[i]] <- df
    } else {
      datalist_termini_2[[i]] <- df %>%
        filter(Datums_laiks >= Datums_start)
    }
  }
  return(bind_rows(datalist_termini_2))
}

cut_hourly_data <- function(data, h_starts) {
  dlist <- list()
  for(i in 1:nrow(h_starts)) {
    Stac <- h_starts[i, "Stacija", drop = T]
    Datums_start <- h_starts[i, "Datums_laiks", drop = T]
    dlist[[i]] <- data %>%
      filter(Stacija == Stac) %>%
      filter(Datums_laiks >= Datums_start)
  }
  ddf <- dlist %>% bind_rows()
  return(ddf)
}

# Ielādē datus ------------------------------------------------------------
# No skripta Export_from_DB_v_day_all.R
# Pagaidām tikai gaisa temperatūras dati

# metadati
metadata <- read_rds("Dati/station_metadata.rds")

# Datu tīrīšana -----------------------------------------------------------

TDRY_reg_data_cl <- TDRY_reg_data %>%
  clean_data(regular = "Y", filter_params = "AVG")

TDRY_unreg_data_cl <- TDRY_unreg_data %>%
  clean_data(regular = "N", filter_params = "AVG")

HTDRY_reg_data_cl <- HTDRY_unreg_data %>%
  clean_data(regular = "Y", filter_params = "AVG")


# Ekstraktē sākuma un beigu datumus katram parametram ---------------------
# Extract start date for Hourly parameters
Rpar_starts <- TDRY_reg_data_cl %>%
  r_starts()
Hpar_starts <- HTDRY_reg_data_cl %>%
  H_starts()

# Apvieno datu kopas ------------------------------------------------------

unreg_data_clean <- cut_unregular_data(TDRY_unreg_data_cl, Rpar_starts)

term_data_clean <- cut_term_data(TDRY_reg_data_cl, r_start = Rpar_starts,
                                 h_start = Hpar_starts)

hourly_data_clean <- cut_hourly_data(HTDRY_reg_data_cl, h_starts = Hpar_starts)

# Apvieno datu kopas ar visiem pareizajiem NA
full_data_clean <- bind_rows(unreg_data_clean, term_data_clean, hourly_data_clean) %>%
  arrange(Stacija, Datums_laiks)

# Noņem liekās stacijas 
full_data_clean <- full_data_clean %>%
  filter(!str_detect(Stacija, "X"))

# Pārbauda duplikātus datos 
dupli_idx <- duplicated(full_data_clean[, c(1, 4)])

didx2 <- c(which(dupli_idx), which(dupli_idx) - 1)
didx2 <- didx2[order(didx2)]

duplidata <- full_data_clean[didx2, ]

# Kā redzams, kļūda duplikātu dēļ datos ir ļoti minimāla, tāpēc tie vienkārši ignorēti
full_data_clean2 <- full_data_clean[!dupli_idx, ]

# Write clean data
full_data_clean %>% write_csv("Dati/Temp_dati_clean.csv")

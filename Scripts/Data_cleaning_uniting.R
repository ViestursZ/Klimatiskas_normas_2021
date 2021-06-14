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
      dplyr::select(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION, Datums, Merijums = VALUE) %>%
      arrange(Stacija, Datums)
  } else if (regular == "Y") {
    data %>%
      filter(!TIME %in% filter_params) %>%
      mutate(Datums_laiks = ymd_hm(str_c(YEAR, MONTH, DAY, TIME, sep = "-"))) %>%
      dplyr::select(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION, Datums_laiks, Merijums = VALUE) %>%
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

add_unregular_hour <- function(ur_data, h = 12) {
  ur_data %>%
    mutate(Datums_laiks = as_datetime(Datums),
           Datums_laiks = `hour<-`(Datums_laiks, h)) %>% # Pieliek datumam stundu, 
                                                         # lai saiet kopā ar pārējiem datiem
    dplyr::select(-Datums)
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
      filter(date(Datums_laiks) < Datums_end)
  }
  
  unregular_data_clean <- bind_rows(unregular_list)
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

remove_fake_unreg <- function(df_unreg, df_reg) {
  # Finds fake unregular data (with legint time values in TIME column) and adds
  # them to regular data frame
  
  fake_data <- df_unreg %>%
    filter(str_detect(TIME, "\\d")) # Searches for TIME == digit
  
  Men_ir_unreg <- fake_data %>%
    group_by(EG_GH_ID, YEAR, MONTH) %>%
    summarise(Men_ir = !(sum(is.na(VALUE) / n()) == 1)) %>% # Vai mēnesī ir vismaz viens mērījums pret mērījumu skaitu?
    filter(Men_ir == T)
  
  svec <- Men_ir_unreg$EG_GH_ID
  yvec <- Men_ir_unreg$YEAR
  mvec <- Men_ir_unreg$MONTH
  
  ureg_list <- list()
  for (i in 1:length(svec)) {
    ureg_list[[i]] <- fake_data %>%
      filter(EG_GH_ID == svec[i] & YEAR == yvec[i] & MONTH == mvec[i])
  }
  
  fake_data_df <- bind_rows(ureg_list)
  
  df_unreg <- filter(df_unreg, !str_detect(TIME, "\\d"))
  df_reg <- bind_rows(df_reg, fake_data_df)
  return(list("unreg" = df_unreg, "reg" = df_reg))
}

# Ielādē datus ------------------------------------------------------------

# metadati
# metadata <- read_rds("Dati/station_metadata.rds")
# 
# TDRY_reg_data <- read_rds("Dati/TDRY_reg_export.rds")
# TDRY_unreg_data <- read_rds("Dati/TDRY_unreg_export.rds")
# HTDRY_unreg_data <- read_rds("Dati/HTDRY_unreg_export.rds")

# Datu tīrīšana -----------------------------------------------------------

# Mean T
TDRY_list <- remove_fake_unreg(TDRY_unreg_data, TDRY_reg_data)

TDRY_reg_data_1 <- TDRY_list[[2]]
TDRY_unreg_data_1 <- TDRY_list[[1]]

TDRY_reg_data_cl <- TDRY_reg_data_1 %>%
  clean_data(regular = "Y", filter_params = "AVG")
TDRY_unreg_data_cl <- TDRY_unreg_data_1 %>%
  clean_data(regular = "N", filter_params = "AVG")
HTDRY_reg_data_cl <- HTDRY_unreg_data %>%
  clean_data(regular = "Y", filter_params = "AVG")

# # Min T
# ATMN_reg_data_cl <- ATMN_reg_data %>%
#   clean_data(regular = "Y", filter_params = "MIN")
# ATMN_unreg_data_cl <- ATMN_unreg_data %>%
#   clean_data(regular = "N", filter_params = "MIN")
# HATMN_reg_data_cl <- HATMN_unreg_data %>%
#   clean_data(regular = "Y", filter_params = "MIN")
# 
# # Max T
# ATMX_reg_data_cl <- ATMX_reg_data %>%
#   clean_data(regular = "Y", filter_params = "MAX")
# ATMX_unreg_data_cl <- ATMX_unreg_data %>%
#   clean_data(regular = "N", filter_params = "MAX")
# HATMX_reg_data_cl <- HATMX_unreg_data %>%
#   clean_data(regular = "Y", filter_params = "MAX")

# Nokrišņi
# PRAB_reg_data_cl <- PRAB_reg_data %>% 
#   clean_data(regular = "Y", filter_params = "SUM")
# PRAB_unreg_data
# HPRAB_reg_data


# Šos parametrus maina atkarībā no parametra un pārējais skript aiziet ----
parametrs <- "Mean_T"
term_par <- "TDRY"

reg_data_cl <- TDRY_reg_data_cl
unreg_data_cl <- TDRY_unreg_data_cl
h_data_cl <- HTDRY_reg_data_cl

# reg_data_cl <- ATMN_reg_data_cl
# unreg_data_cl <- ATMN_unreg_data_cl
# h_data_cl <- HATMN_reg_data_cl
 
# reg_data_cl <- ATMX_reg_data_cl
# unreg_data_cl <- ATMX_unreg_data_cl
# h_data_cl <- HATMX_reg_data_cl



# Ekstraktē sākuma un beigu datumus katram parametram ---------------------
# Extract start date for Hourly parameters
Rpar_starts <- reg_data_cl %>%
  r_starts()
Hpar_starts <- h_data_cl %>%
  H_starts()


# Pievieno stundu (Datums_laiks) unregular datiem -------------------------

unreg_data_cl <- unreg_data_cl %>%
  add_unregular_hour(h = 12)

# Apvieno datu kopas ------------------------------------------------------

term_data_clean <- cut_term_data(reg_data_cl, r_start = Rpar_starts,
                                 h_start = Hpar_starts)

term_data_stacijas <- unique(term_data_clean$Stacija)

term_data_filled_list <- list()
  
# Termiņiem pievieno iztrūkstošos mēnešus no unreg_data_clean

for (i in seq_along(term_data_stacijas)) {
  # i <- 1
  menesu_term_dati <- term_data_clean %>%
    filter(Stacija == term_data_stacijas[i])
  menesu_mer <- menesu_term_dati %>% 
    mutate(Gads = year(Datums_laiks), Menesis = month(Datums_laiks)) %>%
    group_by(Gads, Menesis) %>%
    filter(!is.na(Merijums)) %>%
    summarise(Mer_sk_men = n())
  
  menesu_mer <- menesu_mer %>%
    mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-")))
  
  monseq <- seq.Date(min(menesu_mer$Datums), max(menesu_mer$Datums),
                     by = "month")
  
  monseq_df <- data.frame(Datums = monseq)
  
  menesi_trukst <- menesu_mer %>%
    right_join(monseq_df) %>% 
    mutate(Gads = year(Datums),
           Menesis = month(Datums),
           Mer_sk_men = ifelse(is.na(Mer_sk_men), 0, Mer_sk_men)) %>%
    filter(Mer_sk_men < 5) # Ja ir mazāk par 5 termiņiem mēnesī, skatās unregular datos
  
  
  st_unreg_data <- unreg_data_cl %>%
    filter(Stacija == term_data_stacijas[i]) %>%
    mutate(Gads = year(Datums_laiks),
           Menesis = month(Datums_laiks))
  
  yvec <- pull(menesi_trukst, Gads)
  mvc <- pull(menesi_trukst, Menesis)
  unreg_data_list <- list()
  
  for (j in seq_along(yvec)) {
    unreg_data_list[[j]] <- st_unreg_data %>%
      filter(Gads == yvec[j] & Menesis == mvc[j])
  }
  
  fill_unreg_data_df <- unreg_data_list %>% 
    bind_rows()
  
  if(nrow(fill_unreg_data_df > 0)) {
    fill_unreg_data_df <- dplyr::select(fill_unreg_data_df, -Gads, -Menesis)
  }
    
  term_data_filled_list[[i]] <- menesu_term_dati %>%
    bind_rows(fill_unreg_data_df) %>%
    arrange(Datums_laiks)
}

term_data_clean <- bind_rows(term_data_filled_list)
unreg_data_clean <- cut_unregular_data(unreg_data_cl, Rpar_starts)
hourly_data_clean <- cut_hourly_data(h_data_cl, h_starts = Hpar_starts)


# 24 uz 8 h termiņnovērojumu fix ------------------------------------------
# Mirklī, kad termiņnovērojumi pāriet no 24h uz 8h nepieciešams izņemt visus iztrūkumus atbilstošajā mēnesī

# Stacijas
term_data_stacijas <- unique(term_data_clean$Stacija)
metadata <- read_rds("Dati/station_metadata.rds")

for (i in seq_along(term_data_stacijas)) {
  # i <- 1
  tdat <- metadata %>%
    filter(REGULAR == "Y" & EG_EL_ABBREVIATION == term_par) %>%
    filter(EG_GH_ID == term_data_stacijas[i]) %>%
    filter(TI_INTERVAL == "01:00") %>%
    filter(BEGIN_DATE == min(BEGIN_DATE)) %>%
    pull(BEGIN_DATE)

  if (length(tdat) == 0) {
    next()
  } 
  ytdat <- year(tdat)
  mtdat <- month(tdat)
  
  taizviet_menesis <- term_data_clean %>%
    filter(Stacija == term_data_stacijas[i]) %>%
    filter(year(Datums_laiks) == ytdat) %>%
    filter(month(Datums_laiks) == mtdat) %>%
    filter(!is.na(Merijums))
  
  term_data_clean <- term_data_clean %>%
    filter(!(Stacija == term_data_stacijas[i] & year(Datums_laiks) == ytdat & month(Datums_laiks) == mtdat))
  
  term_data_clean <- bind_rows(term_data_clean, taizviet_menesis)
}

term_data_clean <- term_data_clean %>% arrange(Stacija, Datums_laiks)
term_data_clean <- term_data_clean %>%
  dplyr::select(-Gads, -Menesis)

# Apvieno datu kopas ------------------------------------------------------
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

# Ieraksta novērojumu datus vienā failā -----------------------------------
# Write clean data
full_data_clean2 %>% write_csv(paste0("Dati/", parametrs, "_dati_clean.csv"))

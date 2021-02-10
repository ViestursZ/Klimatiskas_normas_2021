# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)
library(data.table)

# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")
source("Scripts/Tidy_daily.r")
source("Scripts/Tidy_hourly.r")

# Cleaning funkcija
precleaning <- function(data, unreg_pars) {
  # Removes unnecessary stations and flag columns
  data <- data %>%
    select(1:37) %>%
    filter(!str_detect(EG_GH_ID, "X"))
  
  # Splits daily and term data
  unreg <- data %>%
    filter(REGULAR == "N") %>%
    filter(EG_EL_ABBREVIATION %in% unreg_pars)
  reg <- data %>%
    filter(REGULAR == "Y")
  llist <- list("ur" = unreg, "r" = reg)
}

# Tidy datasets
tidy_datasets <- function(data, funreg = "AVG") {
  # Tidies both datasets. Calls modified tidy daily and tkāidy hourly functions
  # funreg          Filters out only this parameter in unregular data. Mostly 
  #                 there to filter out errors in TIME column in CLIDATA
  data$ur <- data$ur %>%
    filter(TIME == funreg)
  
  data$ur <- data$ur %>%
    tidy_daily()
  data$r <- data$r %>%
    tidy_hourly()
  return(data)
}

# Extract stations function
extract_stations <- function(data) {
  stations <- c(unique(data$r$Stacija),
                unique(data$ur$Stacija)[!(data$ur %$% unique(Stacija)) %in% unique(data$r$Stacija)])
  return(stations)
}

# Filter metadata
filter_meta <- function(metad, stacs, params) {
  metad %>%
    filter(EG_GH_ID %in% stacs) %>%
    filter(EG_EL_ABBREVIATION %in% params) %>%
    arrange(EG_GH_ID, BEGIN_DATE)
}

# Extracts metadata terms from the data
extract_meta_termini <- function(meta_dati, param_starts) {
  metalist_termini <- list()
  
  for (i in 1:nrow(param_starts)) {
    Stacija <- param_starts[i, "Stacija", drop = T]
    Datums_laiks <- param_starts[i, "Datums_laiks", drop = T]
    metalist_termini[[i]] <- meta_dati %>%
      filter(REGULAR == "Y") %>%
      filter(!(str_detect(EG_EL_ABBREVIATION, "^H") | str_detect(EG_EL_ABBREVIATION,"^M"))) %>%
      filter(EG_GH_ID == Stacija & BEGIN_DATE < Datums_laiks)
  }
  return(metalist_termini)
}


# Extracts correct terms 
extract_correct_terms <- function(data, meta_terms) {
  # Filter terms according to metadata terms; Add the missing rows with no data
  # data          filtered reguler term meteorological data
  # meta_terms    metadata terms list per station, extracted by extract_meta_termini
  #               function
  datalist_termini <- list()
  for (j in seq_along(meta_terms)) {
    
    # Skip, ja nav termiņu metadatu
    if (nrow(meta_terms[[j]]) == 0) {
      next()
    }
    meta_stac <- meta_terms[[j]] %$% unique(EG_GH_ID)
    terms <- data %>%
      filter(Stacija == meta_stac)
    
    staclist <- list()
    for (i in 1:nrow(meta_terms[[j]])) {
      # Termiņu sākums
      term_sakums <- meta_terms[[j]][i, "BEGIN_DATE", drop = T]
      term_beigas <- meta_terms[[j]][i, "END_DATE", drop = T]
      
      # Time interval
      ti <- meta_terms[[j]][i, "TI_INTERVAL", drop = T] %>%
        str_remove("^0") %>%
        str_remove(":00") %>%
        as.numeric()
      
      # Time interval skaits pārbaudei
      ti_skaits <- 24 / ti
      
      df <- terms %>%
        filter(Datums_laiks >= term_sakums & Datums_laiks <= term_beigas)
      
      # Ja nu df ir tukšs
      if (nrow(df) == 0) {
        staclist[[i]] <- df
        next
      }
      
      # Stundas in time interval
      df_stundas <- df %>%
        mutate(Stunda = hour(Datums_laiks)) %$%
        unique(Stunda)
      
      # Salīdzina stundu skaitu ar interval skaitu; ja nesakrīt, izvelk biežākās stundas termiņā
      if (length(df_stundas) != ti_skaits) {
        stundas_table <- df %>%
          mutate(Stunda = hour(Datums_laiks)) %$%
          table(Stunda)
        st_proc <- stundas_table / nrow(df) * 100
        df_stundas <- as.numeric(names(stundas_table[!st_proc < 1])) # Noņem visas stundas, kuru skaits < 1%
      }
      
      datetime_seq <- seq.POSIXt(from = term_sakums, to = term_beigas, by = "hour")
      datetime_seq <- datetime_seq[hour(datetime_seq) %in% df_stundas]
      datetime_df <- data.frame(Datums_laiks = datetime_seq, Stacija = meta_stac, stringsAsFactors = F)
      
      staclist[[i]] <- full_join(datetime_df, df) %>%
        mutate(Data_stundas = list(df_stundas)) %>% # Debugging - kuras ir noteiktās stundas no datiem
        mutate(t_interval = ti, # Debugging - kāds ir TI no metadatiem?
               ti_skaits = ti_skaits) # Debugging - kāds ir TI skaits dienā?
    }
    datalist_termini[[j]] <- bind_rows(staclist)
  }
  return(datalist_termini)
}

# Extract start date for Hourly parameters
H_starts <- function(data) {
  # extract the start date for hourly parameters. Uses extract_start_date function
  data$r %>%
    filter(str_detect(Parametrs, "^H")) %>%
    group_by(Stacija) %>%
    extract_start_date()
}

# Extract start date regular parametram
Ur_starts <- function(data) {
  # Extract the start date for unregular parameters. Uses extract_start_date function
  data$r %>%
    filter(!(str_detect(Parametrs, "^H") | str_detect(Parametrs, "^M"))) %>%
    group_by(Stacija) %>%
    extract_start_date()
}

# Filters out term data from regular data
extract_term <- function(reg_data) {
  term_data <- reg_data %>%
    filter(!(str_detect(Parametrs, "^H") | str_detect(Parametrs,"^M")))
  return(term_data)
}

# Extracts hourly data
extract_hourly <- function(df) {
  df %>% filter(str_detect(Parametrs, "^H"))
}


# Cutto termiņu sākuma un beigu datumus
cut_term_start <- function(data, ur_start, h_start) {
  # Cuts the term start and end dates
  # data             filtered correct term data
  # ur_start         unregular parameter start dates from Ur_starts function
  # h_start          hourly parameter start dates from H_starts function
  
  datalist_termini_2 <- list()
  for (i in 1:nrow(ur_start)) {
    Stac <- ur_start[i, "Stacija", drop = T]
    Datums_start <- ur_start[i, "Datums_laiks", drop = T]
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

# Add missing hourly data hours
add_missing_hours <- function(data, hpar_start) {
  # Function that adds missinf observations to hourly data
  # data          hourly data set
  # hpar_start    Hourly parameter start date, from Hpar_starts function
  hourly_data <- data %>%
    spread(Stacija, Merijums)
  
  hrange <- range(data$Datums_laiks)
  dateseq <- seq.POSIXt(hrange[1], hrange[2], by = "hour")
  dateseq_df <- data.frame(Datums_laiks = dateseq)
  
  hourly_full <- left_join(dateseq_df, hourly_data)  
  hourly_full <- hourly_full %>%
    gather(Stacija, Merijums, -Datums_laiks, -Parametrs)
  
  # Cut stations by HPRAB start
  hourly_list <- list()
  for (i in 1:nrow(hpar_start)) {
    # i <- 1
    Stac <- hpar_start[i, "Stacija", drop = T]
    Datums_start <- hpar_start[i, "Datums_laiks", drop = T]
    hourly_list[[i]] <- hourly_full %>%
      filter(Stacija == Stac) %>%
      filter(Datums_laiks >= Datums_start)
  }
  
  hourly_full_clean <- hourly_list %>%
    bind_rows()
  return(hourly_full_clean)
}


cut_unregular_data <- function(ur_data, ur_data_starts) {
  # Cuts unregular data at the start points
  # ur_data           unregular data
  # ur_data_starts    unregular data starts 
  unregular_list <- list()
  for (i in 1:nrow(ur_data_starts)) {
    Stac <- ur_data_starts[i, "Stacija", drop = T]
    Datums_end <- ur_data_starts[i, "Datums_laiks", drop = T]
    unregular_list[[i]] <- ur_data %>%
      filter(Stacija == Stac) %>%
      filter(Datums < Datums_end)
  }
  
  unregular_data_clean <- bind_rows(unregular_list)
  
  unregular_data_clean <- unregular_data_clean %>%
    mutate(Datums_laiks = as_datetime(Datums),
           Datums_laiks = `hour<-`(Datums_laiks, 12)) %>%
    dplyr::select(-Datums)
}


# Read in data ------------------------------------------------------------
# Pagaidām tikai gaisa temperatūras dati
temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")
min_temp_data <- read_rds("Dati/min_temp_dati_neapstradati.rds")
max_temp_data <- read_rds("Dati/max_temp_dati_neapstradati.rds")
nokr_data <- read_rds("Dati/nokr_dati_neapstradati.rds")

# metadati
metadata <- read_rds("Dati/station_metadata.rds")

# Datu kārtošana ----------------------------------------------------------
# Kādi regular parametri ir?
unreg_pars <- c("TDRY", "ATMX", "ATMN", "PRAB")

# Atsevišķi katram parametram
# temp_data <- select(temp_data, 1:37)
# nokr_data <- select(nokr_data, 1:37)

# Clean the data
# temp_data <- temp_data %>%
#   precleaning(unreg_pars = unreg_pars)
nokr_data <- nokr_data %>%
  precleaning(unreg_pars = unreg_pars)

# temp_data <- temp_data %>%
#   tidy_datasets(funreg = "AVG")
nokr_data <- nokr_data %>%
  tidy_datasets(funreg = "SUM")


# Extract vajadzīgās stacijas
# temp_stacijas <- extract_stations(temp_data)
nokr_stacijas <- extract_stations(nokr_data)

# Metadata cleaning -------------------------------------------------------
# Remove REGULAR = N, bet stundu dati
# Atfiltrē arī tikai termiņu datus
metadata <- metadata %>%
  filter(!(REGULAR == "N" & str_detect(EG_EL_ABBREVIATION, "^H"))) %>%
  dplyr::select(EG_GH_ID, EG_EL_ABBREVIATION, REGULAR, BEGIN_DATE, END_DATE, TS_ID, TI_INTERVAL) %>%
  mutate(BEGIN_DATE = force_tz(BEGIN_DATE, "UTC"),
         END_DATE = force_tz(END_DATE, "UTC"))

# Atfiltrē relevant metadatus
# temp_meta <- metadata %>% filter_meta(temp_stacijas, c("TDRY", "HTDRY", "MTDRY"))
nokr_meta <- metadata %>% filter_meta(nokr_stacijas, c("PRAB", "HPRAB", "MPRAB")) # Extract start date H parametram


# Parametru sākumi --------------------------------------------------------
# Extract hourly parameter start
# Hpar_starts <- H_starts(temp_data)
Hpar_starts_nokr <- H_starts(nokr_data)

# Extract start date for undergular parameter
# Urpar_starts <- Ur_starts(temp_data)
Urpar_starts_nokrisni <- Ur_starts(nokr_data)

# Extract relevant metadata terms and relevant term data
metalist_termini_nokr <- extract_meta_termini(meta_dati = nokr_meta, param_starts = Hpar_starts_nokr)
# metalist_termini_temp <- extract_meta_termini(meta_dati = temp_meta, param_starts = Hpar_starts)


# Filters out only term data
nokr_termini_data <- extract_term(nokr_data$r)
# termini_data <- extract_term(temp_data$r)


##### TEST #####
nokr_termini_data_ex <- extract_correct_terms(nokr_termini_data, metalist_termini_nokr)
nokr_termini_data_ex <- bind_rows(nokr_termini_data_ex)

# termini_data <- bind_rows(datalist_termini)
nokr_termini_data_clean <- cut_term_start(nokr_termini_data, Urpar_starts_nokrisni, Hpar_starts_nokr)


# Pievieno HTDRY datus un unregular datus -----------------------------------
# Remove 10 minute parameters
hourly_data <- temp_data$r %>%
  extract_hourly()
hourly_nokrisni <- nokr_data$r %>%
  extract_hourly()

# Add missing hours
nokr_hourly_full_clean <- add_missing_hours(hourly_nokrisni, Hpar_starts_nokr)

# Nogriež unregular data --------------------------------------------------
unregular_data <- temp_data$ur
unregular_nokr_data <- nokr_data$ur

# Cuts end of unregular data
unregular_nokr_data_clean <- cut_unregular_data(unregular_nokr_data, Urpar_starts_nokrisni)


# Apvieno visas datu kopas vienā ------------------------------------------

nokr_termini_data_clean
nokr_hourly_full_clean
unregular_nokr_data_clean

# Apvieno datu kopas ar visiem pareizajiem NA
full_data_clean <- bind_rows(termini_data_clean, hourly_full_clean, unregular_data_clean) %>%
  arrange(Stacija, Datums_laiks)

full_nokr_clean <- bind_rows(nokr_hourly_full_clean, nokr_termini_data_clean,
                             unregular_nokr_data_clean)

# Pārbauda pēdējo daļu datos
dupli_idx <- duplicated(full_data_clean[, 1:2])
full_data_clean <- full_data_clean[!dupli_idx, ]

dupli_nokr_idx <- duplicated(full_nokr_clean[, 1:2])
full_nokr_clean2 <- full_nokr_clean[!dupli_nokr_idx, ]

# Export data -------------------------------------------------------------

temp_data_clean <- full_data_clean
temp_data_clean %>%
  write_excel_csv("Dati/Temp_dati_clean.csv")

full_nokr_clean2 %>%
  write_excel_csv("Dati/nokr_dati_clean.csv")

# Plots -------------------------------------------------------------------
#
# library(plotly)
# 
# Ainazi_plot <- temp_data_clean %>%
#   filter(Stacija == "RIAI99PA") %>%
#   ggplot(data = .) +
#   geom_point(aes(Datums_laiks, Merijums), size = 0.5)
# 
# Ainazi_html <- ggplotly(Ainazi_plot)
# 
# htmlwidgets::saveWidget(Ainazi_html, "Ainazi_test.html")


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)


# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")
source("Scripts/Tidy_daily.r")
source("Scripts/Tidy_hourly.r")

# Read in data ------------------------------------------------------------
# Pagaidām tikai gaisa temperatūras dati
temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")
min_temp_data <- read_rds("Dati/min_temp_dati_neapstradati.rds")
max_temp_data <- read_rds("Dati/max_temp_dati_neapstradati.rds")

# metadati
metadata <- read_rds("Dati/station_metadata.rds")

# Datu kārtošana ----------------------------------------------------------
# Kādi regular parametri ir?
unreg_pars <- c("TDRY", "ATMX", "ATMN")

# Atsevišķi katram parametram
temp_data <- select(temp_data, 1:37)

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

temp_data <- temp_data %>%
  precleaning(unreg_pars = unreg_pars)

# Tidy datasets
tidy_datasets <- function(data, funreg = "AVG") {
  # Tidies both datasets. Calls modified tidy daily and tidy hourly functions
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

temp_data <- temp_data %>%
  tidy_datasets(funreg = "AVG")

# Extract vajadzīgās stacijas
extract_stations <- function(data) {
  stations <- c(unique(data$r$Stacija),
                unique(data$ur$Stacija)[!(data$ur %$% unique(Stacija)) %in% unique(data$r$Stacija)])
  return(stations)
}

temp_stacijas <- extract_stations(temp_data)

# Metadata cleaning -------------------------------------------------------
# Remove REGULAR = N, bet stundu dati
# Atfiltrē arī tikai termiņu datus
metadata <- metadata %>%
  filter(!(REGULAR == "N" & str_detect(EG_EL_ABBREVIATION, "^H"))) %>%
  dplyr::select(EG_GH_ID, EG_EL_ABBREVIATION, REGULAR, BEGIN_DATE, END_DATE, TS_ID, TI_INTERVAL) %>%
  mutate(BEGIN_DATE = force_tz(BEGIN_DATE, "UTC"),
         END_DATE = force_tz(END_DATE, "UTC"))

filter_meta <- function(metad, stacs, params) {
  metad %>%
    filter(EG_GH_ID %in% stacs) %>%
    filter(EG_EL_ABBREVIATION %in% params) %>%
    arrange(EG_GH_ID, BEGIN_DATE)
}

# Fun call
temp_meta <- metadata %>% filter_meta(temp_stacijas, c("TDRY", "HTDRY", "MTDRY"))


# Extract start date H parametram
H_starts <- function(data) {
  # extract the start date for hourly parameters. Uses extract_start_date function
  data$r %>%
  filter(str_detect(Parametrs, "^H")) %>%
  group_by(Stacija) %>%
  extract_start_date()
}

# Fun call
Hpar_starts <- H_starts(temp_data)

# Extract start date regular parametram
Ur_starts <- function(data) {
  # Extract the start date for unregular parameters. Uses extract_start_date function
  data$r %>%
    filter(!(str_detect(Parametrs, "^H") | str_detect(Parametrs, "^M"))) %>%
    group_by(Stacija) %>%
    extract_start_date()
}

# Fun call
Urpar_starts <- Ur_starts(temp_data)


# Extract relevant metadata terms and relevant term data
metalist_termini <- list()

for (i in 1:nrow(Hpar_starts)) {
  Stacija <- Hpar_starts[i, "Stacija", drop = T]
  Datums_laiks <- Hpar_starts[i, "Datums_laiks", drop = T]
  metalist_termini[[i]] <- temp_meta_r %>%
    filter(!str_detect(EG_EL_ABBREVIATION, "^H") | str_detect(EG_EL_ABBREVIATION,"^M")) %>%
    filter(EG_GH_ID == Stacija & BEGIN_DATE < Datums_laiks)
}

termini_data <- temp_data$r %>%
  filter(!(str_detect(Parametrs, "^H") | str_detect(Parametrs,"^M")))

# Filter terms according to metadata terms; Add the missing rows with no data
datalist_termini <- list()
for (j in seq_along(metalist_termini)) {
  
  # Skip, ja nav termiņu metadatu
  if (nrow(metalist_termini[[j]]) == 0) {
    next()
  }
  meta_stac <- metalist_termini[[j]] %$% unique(EG_GH_ID)
  terms <- termini_data %>%
    filter(Stacija == meta_stac)
  
  staclist <- list()
  for (i in 1:nrow(metalist_termini[[j]])) {
    # Termiņu sākums
    term_sakums <- metalist_termini[[j]][i, "BEGIN_DATE", drop = T]
    term_beigas <- metalist_termini[[j]][i, "END_DATE", drop = T]
    
    # Time interval
    ti <- metalist_termini[[j]][i, "TI_INTERVAL", drop = T] %>%
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
    
    staclist[[i]] <- left_join(datetime_df, df)
  }
  datalist_termini[[j]] <- bind_rows(staclist)
}

termini_data <- bind_rows(datalist_termini)

# Nogriež datus termiņū sākuma un beigu punktos
datalist_termini_2 <- list()
for (i in 1:nrow(Urpar_starts)) {
  # i <- 1
  Stac <- Urpar_starts[i, "Stacija", drop = T]
  Datums_start <- Urpar_starts[i, "Datums_laiks", drop = T]
  Datums_end <- filter(Hpar_starts, Stacija == Stac) %>% pull(Datums_laiks)
  
  if (length(Datums_end) == 0) {
    df <- termini_data %>%
      filter(Stacija == Stac)
  } else {
    df <- termini_data %>%
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

termini_data_clean <- datalist_termini_2 %>% bind_rows()


# Pievieno HTDRY datus un unregular datus -----------------------------------




  
  
# Calculate daily data





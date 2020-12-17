# Apstrādā/sakaārto stundu datus tieši no datubāzes
library(tidyverse)
library(lubridate)
library(tidyr)

tidy_hourly <- function(df, to_LV_time = F) {
  # df        - data frame ar tieši no datubāzes tabulas formā sakārtotiem datiem
  # to_LV_time- TRUE vai FALSE, kas apzīmē vai ir nepieciešamība pāriet uz 
  #             Latvijas laiku
  df  <- df %>%
    gather(key = DIENA, value = Merijums, -c(1:5)) %>%
    mutate(Datums_laiks = str_c(YEAR, "-", MONTH, "-", 
                                str_replace(DIENA, "[:alpha:]+", ""), " ", TIME),
           Datums_laiks = ymd_hm(Datums_laiks)) %>%
    rename(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION) %>%
    select(Datums_laiks, Stacija, Parametrs, Merijums)
  
  if (to_LV_time == T) {
    df <- df %>%
      mutate(Datums_laiks = with_tz(Datums_laiks, "Europe/Riga")) %>%
      filter(!is.na(Datums_laiks)) %>%
      arrange(Datums_laiks)
  } else {
    df <- df %>%
      filter(!is.na(Datums_laiks)) %>%
      arrange(Datums_laiks)
  }
  return(df)
}

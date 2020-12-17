# Apstrādā/sakārto dienu datus tieši no datubāzes
library(tidyverse)
library(lubridate)
library(tidyr)

tidy_daily <- function(df) {
  # df        - data frame ar CLIDATA tabulas formā sakārtotiem datiem
  df  <- df %>%
    gather(key = DIENA, value = Merijums, -c(1:6)) %>%
    mutate(Datums = str_c(YEAR, "-", MONTH, "-", 
                          str_replace(DIENA, "[:alpha:]+", "")),
           Datums = ymd(Datums)) %>% 
    rename(Stacija = EG_GH_ID, Parametrs = EG_EL_ABBREVIATION) %>%
    select(Datums, Stacija, Parametrs, Merijums) %>%
    filter(!is.na(Datums)) %>%
    arrange(Datums)
  return(df)
}

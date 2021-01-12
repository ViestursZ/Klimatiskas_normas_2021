library(lubridate)
library(RcppRoll)

extract_start_date <- function(df, stacija, datcol = "Datums", na_count = 3) {
  stac_df <- filter(df, Stacija == stacija)
  
  datums <- stac_df %>%
    filter(Stacija == stacija) %>%
    arrange(!!sym(datcol)) %>%
    mutate(Merijums_roll = roll_suml(Merijums, n = na_count)) %>%
    filter(!is.na(Merijums_roll)) %>%
    filter(!!sym(datcol) == min(!!sym(datcol))) %>%
    pull(!!sym(datcol))
  
  datums <- datums - days(na_count - 1)
  return(datums)
}

extract_end_date <- function(df, stacija, datcol = "Datums", na_count = 3) {
  stac_df <- filter(df, Stacija == stacija)
  
  datums <- stac_df %>%
    filter(Stacija == stacija) %>%
    arrange(!!sym(datcol)) %>%
    mutate(Merijums_roll = roll_suml(Merijums, n = na_count)) %>%
    filter(!is.na(Merijums_roll)) %>%
    filter(!!sym(datcol) == max(!!sym(datcol))) %>%
    pull(!!sym(datcol))
  
  return(datums)
}

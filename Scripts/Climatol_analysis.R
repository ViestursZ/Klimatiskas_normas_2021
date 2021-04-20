# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)


# Funkcijas ---------------------------------------------------------------
extract_last <- function(vec, len) {
  vec <- vec[(length(vec) - (len - 1)):(length(vec))]
  return(vec)
}

compare_series <- function(real_series, comp_series) {
  abs_err <- vector(mode = "numeric")
  for (ser_idx in seq_along(comp_series)) {
    abs_err[ser_idx] <- mean(abs(real_series[[1]] - comp_series[[ser_idx]]),
                             na.rm = T)
  }
  return(abs_err)
}



# Homogenization files ----------------------------------------------------

# # Paraugs multi-analīzei nākotnē
# climatol_daily <- c("LV-h_medited_breakpoints2_dzmax11_1949-2018.rda",
#               "LV-h_m_breakpoints2_dzmax11_1949-2018.rda",
#               "LV-h_d_snht1_100_snht2_75_1949-2018.rda",
#               "LV-h_d_snht1_100_snht2_100_1949-2018.rda")
# 
# homdata <- purrr::map(homfiles, function(x) {
#   load(x)
#   vals <- list("ini" = ini, "nd" = nd, "ndec" = ndec, "ne" = ne, "nei" = nei, 
#                "nm" = nm, "std" = std, "x" = x)
#   data <- list("dah" = dah, "dat" = dat, "est.c" = est.c)
#   newlist <- list("vals" = vals, "data" = data)
#   return(newlist)
# })


# Ielādē climatol datus
climatol_daily <- load("Dati/Climatol_data/LVMeanT-daily_monbrks_1947-2020.rda")
vals <- list("ini" = ini, "nd" = nd, "ndec" = ndec, "ne" = ne, "nei" = nei, 
             "nm" = nm, "std" = std, "x" = x)
data <- list("dah" = dah, "dat" = dat, "est.c" = est.c)
homdata <- list("vals" = vals, "data" = data)


# Sakārto datu kopas
climatol_temp_daily_s <- climatol_temp_daily %>%
  spread(Stacija, Merijums)

datnames <- colnames(climatol_temp_daily_s)[colnames(climatol_temp_daily_s) != "Datums"]

# Loop ar datu apstrādi
# Initialize variables
datumi <- homdata$vals$x
dath <- homdata$data$dah
datuh <- homdata$data$dat

dahnames <- homdata$data$est.c$Code

colnames(dath) <- dahnames


dath <- dath %>%
  as_tibble() %>%
  mutate(Datums = datumi) %>%
  dplyr::select(Datums, everything())

series <- list()
abs_errors <- vector()


for (i in seq_along(datnames)) {
  series_name <- datnames[i]
  real_ser <- climatol_temp_daily_s %>%
    dplyr::select(matches(series_name)) %>%
    purrr::map(extract_last, 100)
  
  hom_series <- dath %>%
    dplyr::select(matches(series_name))
  
  comp_ser <- hom_series %>%
    purrr::map(extract_last, 100)
  
  errors <- compare_series(real_ser, comp_ser)
  real_idx <- which.min(errors)
  
  hom_series <- hom_series %>%
    dplyr::select(real_idx) %>%
    set_colnames(series_name)
  
  series[[i]] <- hom_series
  abs_errors[i] <- errors[real_idx]
}

homogenizetie_dati <- bind_cols(series) %>%
  mutate(Datums = datumi) %>%
  dplyr::select(Datums, everything())
  

# Veja pieejamības grafiki
# Libraries ---------------------------------------------------------------

library(RODBC)
library(tidyverse)
library(magrittr)
library(lubridate)
library(RcppRoll)

# Ielādē funkcijas --------------------------------------------------------

source("Scripts/Tidy_daily.r", encoding = "UTF-8")
source("Scripts/Tidy_hourly.r", encoding = "UTF-8")

# Ielādē datus ------------------------------------------------------------

data <- read_rds("Dati/mean_wind_dati_neapstradati.rds")

# Sakārto datus -----------------------------------------------------------
# Kādi regular parametri ir?
reg_pars <- c("HMWNS", "WPGSB")

data <- select(data, 1:37)
data <- data %>%
  filter(!str_detect(EG_GH_ID, "X"))

data_ur <- data %>%
  filter(REGULAR == "N") 

data_r <- data %>%
  filter(REGULAR == "Y")

# Apstrādā unregular datus
data_ur <- data_ur %>%
  filter(EG_EL_ABBREVIATION %in% reg_pars)

data_ur <- data_ur %>%
  filter(TIME == "AVG")

# Random testi
# data_ur %$% unique(TIME)
# 
# data_ur %>% 
#   filter(EG_GH_ID == "AIZPUTE") %>%
#   filter(YEAR %in% c(1963:1984)) %>% View()
#   filter(TIME == "00:00") %>% View()


data_ur <- data_ur %>%
  tidy_daily()

# Pievieno instrūkstošās dienas
datumi <- seq.Date(min(data_ur$Datums), max(data_ur$Datums), by = "day")
datumi <- data.frame(Datums = datumi)

data_ur <- data_ur %>%
  spread(Stacija, Merijums) %>%
  left_join(datumi, .) %>%
  gather(Stacija, Merijums, -Parametrs, -Datums)

# Apstrādā regular datus
data_r <- data_r %>%
  tidy_hourly()

r_params <- unique(data_r$Parametrs)
data_r_ls <- map(r_params, ~ filter(data_r, Parametrs == .x))

names(data_r_ls) <- r_params

# Iegūst mērījumu skaitu pa dienām un stacijām
mer_skaits_test <- data_r_ls[[1]] %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Mer_skaits = n())

# mer_skaits_test %>%
#   arrange(Stacija, Datums) %>%
#   group_by(Stacija) %$%
#   # mutate(Mer_skaits_roll = round(roll_meanr(Mer_skaits, n = 30))) %$%
#   table(Mer_skaits)

# Iegūst pirmos datumus, kad mērījumi Regular parametram katrā stacijā nav NA
stacijas <- unique(mer_skaits_test$Stacija)

ur_cut_dates <- map(stacijas, function(x) {
  dat <- mer_skaits_test %>%
    filter(Stacija == x) %>%
    filter(!is.na(Mer_skaits)) %>%
    arrange(Datums) %>%
    slice(1) %>%
    pull(Datums)
  return(dat)
}) %>%
  unlist() %>%
  as_date()

# ur_cut_dates <- tibble(Stacija = temp_stacijas, cut_date = ur_cut_dates)

# Cutto regular datus pirmajā daļā
mer_skaits_test <- map2(stacijas, ur_cut_dates, function(x, y) {
  mer_skaits_test %>%
    filter(Stacija == x & Datums >= y)
})

# Cutto ur datus, līdz ur_cut_dates, kā arī croppo NA sākumā
ur_list <- map2(stacijas, ur_cut_dates, function(stac, cutd) {
  data_ur %>%
    filter(Stacija == stac) %>%
    filter(!is.na(Merijums)) %>%
    filter(Datums >= min(Datums)) %>% 
    filter(Datums < cutd)
})


extract_start_date <- function(df, stacija, datcol = "Datums") {
  stac_df <- filter(df, Stacija == stacija)
  datums <- stac_df %>%
    filter(Stacija == stacija) %>%
    filter(!is.na(Merijums)) %>%
    filter(!!sym(datcol) == min(!!sym(datcol))) %>%
    pull(!!sym(datcol))
  return(datums)
}

# Extracto pārejo parametru sākuma datumus
par2_start <- map(stacijas, extract_start_date, df = data_r_ls[[2]], datcol = "Datums_laiks")
par3_start <- map(stacijas, extract_start_date, df = data_r_ls[[3]], datcol = "Datums_laiks")
par2_start <- map(par2_start, function(x) {ifelse(length(x) == 0, NA, x)})
par3_start <- map(par3_start, function(x) {ifelse(length(x) == 0, NA, x)})

par2_start <- par2_start %>% unlist() %>% as_datetime() %>% as_date()
par3_start <- par3_start %>% unlist() %>% as_datetime() %>% as_date()

# Grafika piemērs ---------------------------------------------------------
# Temperatūras grafiks

for (i in seq_along(stacijas)) {
  # i <- 4
  graph_df <- ur_list[[i]] %>%
    mutate(Mer_skaits = 1) %>%
    select(Datums, Stacija, Mer_skaits) %>%
    bind_rows(mer_skaits_test[[i]])
  
  # Pievieno iztrūkumus
  graph_datumi <- tibble(Datums = seq.Date(min(graph_df$Datums), max(graph_df$Datums), by = "day"))
  ran <- range(graph_datumi$Datums)
  ran_idx <- ran[2] - ran[1]
  
  # Joino iztrūkumus
  graph_df <- graph_df %>%
    left_join(graph_datumi, .)
  
  ranmax <- ran[2] + 0.12 * ran_idx
  ranmin <- ran[1] - 0.11 * ran_idx
  
  ran_diff <- 0.05 * as.numeric(ran_idx)
  
  par3x <- ur_cut_dates[i] - 0.1 * ran_idx
  if (par3x < ranmin + ran_diff) par3x <- ranmin + ran_diff
  if (par3x > ranmax - ran_diff) par3x <- ranmax - ran_diff
  
  # Plot
  iztr_plot <- ggplot(graph_df) +
    geom_line(aes(Datums, Mer_skaits), na.rm = F, size = 2, color = "black") + 
    ggtitle(paste0(stacijas[i], " vidējās gaisa temperatūras novērojumi")) +
    geom_vline(xintercept = ur_cut_dates[i], linetype = "longdash", col = "dark green", size = 1.2) +
    annotate(geom = "label", label = paste0("Termiņ-\nnovērojumu\nsākums\n", ur_cut_dates[i]),
             x = par3x, y = 15.5, col = "dark green", size = 4)
  
  if (!is.na(par2_start[i])) {
    par1x <- par2_start[i] - 0.1 * ran_idx
    if (par1x < ranmin + ran_diff) par1x <- ranmin + ran_diff
    if (par1x > ranmax - ran_diff) par1x <- ranmax - ran_diff
    
    iztr_plot <- iztr_plot +
      geom_vline(xintercept = par2_start[i], linetype = "longdash", col = "dark blue", size = 1.2) + 
      annotate(geom = "label", label = paste0(r_params[2], "\nsākums\n", par2_start[i]),
               x = par1x, y = 26.5, col = "dark blue", size = 4)
  }
  
  if (!is.na(par3_start[i])) {
    par2x <- par3_start[i] + 0.1 * ran_idx
    if (par2x < ranmin + ran_diff) par2x <- ranmin + ran_diff
    if (par2x > ranmax - ran_diff) par2x <- ranmax - ran_diff
    
    iztr_plot <- iztr_plot +
      geom_vline(xintercept = par3_start[i], linetype = "longdash", col = "dark red", size = 1.2) +
      annotate(geom = "label", label = paste0(r_params[3], "\nsākums\n", par3_start[i]),
                          x = par2x, y = 21, col = "dark red", size = 4)
  }
  
  iztr_plot + 
    scale_x_date(limits = c(ranmin, ranmax)) +
    scale_y_continuous(limits = c(0, 28), name = "Mērījumu skaits") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggsave(paste0("Grafiki/Parametru_analize/Temperatura/", stacijas[i], ".png"))
  
  # ggplot(graph_df) +
  #   geom_line(aes(Datums, Mer_skaits), na.rm = F, size = 2, color = "black") + 
  #   ggtitle(paste0(stacijas[i], " nokrišņu novērojumi")) +
  #   geom_vline(xintercept = par2_start[i], linetype = "longdash", col = "dark blue", size = 1.2) + 
  #   geom_vline(xintercept = ur_cut_dates[i], linetype = "longdash", col = "dark green", size = 1.2) +
  #   geom_vline(xintercept = par3_start[i], linetype = "longdash", col = "dark red", size = 1.2) +
  #   annotate(geom = "text", label = paste0(r_params[2], "\nsākums\n", par2_start[i]),
  #            x = par1x, y = 26.5, col = "dark blue", size = 4) +
  #   annotate(geom = "text", label = paste0(r_params[3], "\nsākums\n", par3_start[i]),
  #            x = par2x, y = 21, col = "dark red", size = 4) +
  #   annotate(geom = "text", label = paste0("Termiņ-\nnovērojumu\nsākums\n", ur_cut_dates[i]),
  #            x = par3x, y = 15.5, col = "dark green", size = 4) +
  #   scale_x_date(limits = c(ranmin, ranmax)) +
  #   scale_y_continuous(limits = c(0, 28), name = "Mērījumu skaits") +
  #   theme_bw() +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   ggsave(paste0("Grafiki/Parametru_analize/Nokrisni/", stacijas[i], ".png"))
  
}

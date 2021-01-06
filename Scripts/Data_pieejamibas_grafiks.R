# Izveido datu pieejamības grafikus
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

temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")

# Sakārto datus -----------------------------------------------------------

temp_data <- select(temp_data, 1:37)
temp_data <- temp_data %>%
  filter(!str_detect(EG_GH_ID, "X"))

temp_data_ur <- temp_data %>%
  filter(REGULAR == "N") 

temp_data_r <- temp_data %>%
  filter(REGULAR == "Y")

# Apstrādā unregular datus
temp_data_ur <- temp_data_ur %>%
  filter(EG_EL_ABBREVIATION == "TDRY")  %>%
  filter(TIME == "AVG") 

temp_data_ur <- temp_data_ur %>%
  tidy_daily() 

# Pievieno instrūkstošās dienas
datumi <- seq.Date(min(temp_data_ur$Datums), max(temp_data_ur$Datums), by = "day")
datumi <- data.frame(Datums = datumi)

temp_data_ur <- temp_data_ur %>%
  spread(Stacija, Merijums) %>%
  left_join(datumi, .) %>%
  gather(Stacija, Merijums, -Parametrs, -Datums)

# Apstrādā regular datus
temp_data_r <- temp_data_r %>%
  tidy_hourly()

r_params <- unique(temp_data_r$Parametrs)
temp_data_r_ls <- map(r_params, ~ filter(temp_data_r, Parametrs == .x))

names(temp_data_r_ls) <- r_params

# Iegūst mērījumu skaitu pa dienām un stacijām
mer_skaits_test <- temp_data_r_ls[[1]] %>%
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Mer_skaits = n())

# mer_skaits_test %>%
#   arrange(Stacija, Datums) %>%
#   group_by(Stacija) %$%  
#   # mutate(Mer_skaits_roll = round(roll_meanr(Mer_skaits, n = 30))) %$%
#   table(Mer_skaits)

# Iegūst pirmos datumus, kad mērījumi TDRY parametram katrā stacijā nav NA
temp_stacijas <- unique(mer_skaits_test$Stacija)

ur_cut_dates <- map(temp_stacijas, function(x) {
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
mer_skaits_test <- map2(temp_stacijas, ur_cut_dates, function(x, y) {
  mer_skaits_test %>%
    filter(Stacija == x & Datums >= y)
})

# Cutto ur datus, līdz ur_cut_dates, kā arī croppo NA sākumā
ur_list <- map2(temp_stacijas, ur_cut_dates, function(stac, cutd) {
  temp_data_ur %>%
    filter(Stacija == stac) %>%
    filter(!is.na(Merijums)) %>%
    filter(Datums >= min(Datums)) %>% 
    filter(Datums < cutd)
})
  

ur_list
mer_skaits_test

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
HTDRY_start <- map(temp_stacijas, extract_start_date, df = temp_data_r_ls[[2]], datcol = "Datums_laiks")
MTDRY_start <- map(temp_stacijas, extract_start_date, df = temp_data_r_ls[[3]], datcol = "Datums_laiks")

HTDRY_start <- HTDRY_start %>% unlist() %>% as_datetime() %>% as_date()
MTDRY_start <- MTDRY_start %>% unlist() %>% as_datetime() %>% as_date()

# Apvieno ur_list un Mer_skaits_test

# Grafika piemērs ---------------------------------------------------------

temp_stacijas[4]
mer_skaits_test[[4]]
ur_cut_dates[4]
HTDRY_start[4]
MTDRY_start[4]

for (i in seq_along(temp_stacijas)) {
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
  
  par1x <- HTDRY_start[i] - 0.1 * ran_idx
  if (par1x < ranmin + ran_diff) par1x <- ranmin + ran_diff
  if (par1x > ranmax - ran_diff) par1x <- ranmax - ran_diff
  
  par2x <- MTDRY_start[i] + 0.1 * ran_idx
  if (par2x < ranmin + ran_diff) par2x <- ranmin + ran_diff
  if (par2x > ranmax - ran_diff) par2x <- ranmax - ran_diff
  
  par3x <- ur_cut_dates[i] - 0.1 * ran_idx
  if (par3x < ranmin + ran_diff) par3x <- ranmin + ran_diff
  if (par3x > ranmax - ran_diff) par3x <- ranmax - ran_diff
  
  ggplot(graph_df) +
    geom_line(aes(Datums, Mer_skaits), na.rm = F, size = 2, color = "black") + 
    ggtitle(paste0(temp_stacijas[i], " gaisa temperatūras novērojumi")) +
    geom_vline(xintercept = HTDRY_start[i], linetype = "longdash", col = "dark blue", size = 1.2) + 
    geom_vline(xintercept = ur_cut_dates[i], linetype = "longdash", col = "dark green", size = 1.2) +
    geom_vline(xintercept = MTDRY_start[i], linetype = "longdash", col = "dark red", size = 1.2) +
    annotate(geom = "text", label = paste0("HTDRY\nsākums\n", HTDRY_start[i]),
             x = par1x, y = 26.5, col = "dark blue", size = 4) +
    annotate(geom = "text", label = paste0("MTDRY\nsākums\n", MTDRY_start[i]),
             x = par2x, y = 21, col = "dark red", size = 4) +
    annotate(geom = "text", label = paste0("Termiņ-\nnovērojumu\nsākums\n", ur_cut_dates[i]),
             x = par3x, y = 15.5, col = "dark green", size = 4) +
    scale_x_date(limits = c(ranmin, ranmax)) +
    scale_y_continuous(limits = c(0, 28), name = "Mērījumu skaits") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggsave(paste0("Grafiki/Parametru_analize/Temperatura/", temp_stacijas[i], ".png"))
}

ggplot(graph_df) +
  geom_line(aes(Datums, Mer_skaits), na.rm = F, size = 2, color = "black") + 
  ggtitle(paste0(temp_stacijas[i], " gaisa temperatūras novērojumi")) +
  geom_vline(xintercept = HTDRY_start[i], linetype = "longdash", col = "dark blue", size = 1.2) + 
  geom_vline(xintercept = ur_cut_dates[i], linetype = "longdash", col = "dark green", size = 1.2) +
  geom_vline(xintercept = MTDRY_start[i], linetype = "longdash", col = "dark red", size = 1.2) +
  annotate(geom = "text", label = paste0("HTDRY\nsākums\n", HTDRY_start[i]),
           xmin = ran[1] - 0.11 * ran_idx, x = HTDRY_start[i] - 0.1 * ran_idx, 
           xmax = HTDRY_start[i], y = 26.5, col = "dark blue") +
  annotate(geom = "text", label = paste0("MTDRY\nsākums\n", MTDRY_start[i]),
           xmin = MTDRY_start[i], x = MTDRY_start[i] +  0.1 * ran_idx, 
           xmax = ran[1] + 0.11 * ran_idx, y = 26.5, col = "dark red") +
  annotate(geom = "text", label = paste0("Termiņ-\nnovērojumu\nsākums\n", ur_cut_dates[i]),
           xmin = ran[1] - 0.11 * ran_idx, x = ur_cut_dates[i] -  0.1 * ran_idx,
           xmax = ur_cut_dates[i], y = 15.5, col = "dark green", ) +
  scale_x_date(limits = c(ran[1] - 0.11 * ran_idx, ran[2] + 0.12 * ran_idx)) +
  scale_y_continuous(limits = c(0, 28), name = "Mērījumu skaits") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0("Grafiki/Parametru_analize/Temperatura/", temp_stacijas[i], ".png"))

             
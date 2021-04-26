
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(RcppRoll)
library(plotly)
library(htmlwidgets)
library(lubridate)

# Functions ---------------------------------------------------------------

source("Scripts/extract_par_start_end_function.r")
source("Scripts/Tidy_hourly.r")
source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R", encoding = "UTF-8")

# Extract start date for Hourly parameters
H_starts <- function(data) {
  # extract the start date for hourly parameters. Uses extract_start_date function
  data %>%
    group_by(Stacija) %>%
    extract_start_date()
}

# Read in the data --------------------------------------------------------

temp_data <- read_rds("Dati/Temp_dati_neapstradati.rds")
max_temp_data <- read_rds("Dati/max_temp_dati_neapstradati.rds")
min_temp_data <- read_rds("Dati/min_temp_dati_neapstradati.rds")
nokr_data <- read_rds("Dati/nokr_dati_neapstradati.rds")
mean_wind_data <- read_rds("Dati/mean_wind_dati_neapstradati.rds")
max_wind_data <- read_rds("Dati/max_wind_dati_neapstradati.rds")

# Mazliet satīra datus ----------------------------------------------------

temp_data_long <- temp_data %>%
  filter(EG_EL_ABBREVIATION == "HTDRY") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly()

max_temp_data_long <- max_temp_data %>%
  filter(EG_EL_ABBREVIATION == "HATMX") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly() %>%
  filter(!str_detect(Stacija, "X"))

min_temp_data_long <- min_temp_data %>%
  filter(EG_EL_ABBREVIATION == "HATMN") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly() %>%
  filter(!str_detect(Stacija, "X"))

nokr_data_long <- nokr_data %>%
  filter(EG_EL_ABBREVIATION == "HPRAB") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly() %>%
  filter(!str_detect(Stacija, "X"))

mean_wind_data_long <- mean_wind_data %>%
  filter(EG_EL_ABBREVIATION == "HWNDS") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly() %>%
  filter(!str_detect(Stacija, "X"))

max_wind_data_long <- max_wind_data %>%
  filter(EG_EL_ABBREVIATION == "HWSMX") %>%
  filter(REGULAR == "Y") %>%
  select(1:37) %>%
  tidy_hourly() %>%
  filter(!str_detect(Stacija, "X"))

# Grafiki -----------------------------------------------------------------

temp_starts <- temp_data_long %>% 
  H_starts()

temp_starts_graph <- temp_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HTDRY sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(temp_starts_graph)
saveWidget(t, "HTDRY_sakums.html", selfcontained = T)

# Max_temp
max_temp_starts <- max_temp_data_long %>% 
  H_starts()

max_temp_starts_graph <- max_temp_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HATMX sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(max_temp_starts_graph)
saveWidget(t, "HATMX_sakums.html", selfcontained = T)

# Min_temp
min_temp_starts <- min_temp_data_long %>% 
  H_starts()

min_temp_starts_graph <- min_temp_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HATMN sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(min_temp_starts_graph)
saveWidget(t, "HATMN_sakums.html", selfcontained = T)

# nokr
nokr_starts <- nokr_data_long %>% 
  H_starts()

# nokr_starts %>%
#   mutate(Datums  = date(Datums_laiks)) %>%
#   select(Stacija, Datums) %>%
#   mutate(Stacija_full = recode_stations(Stacija, "ID-LV")) %>%
#   write_excel_csv2("Stundu_nokrisni_starts.csv")


nokr_starts_graph <- nokr_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HPRAB sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(nokr_starts_graph)
saveWidget(t, "HPRAB_sakums.html", selfcontained = T)


# mean_wind
mean_wind_starts <- mean_wind_data_long %>% 
  H_starts()

mean_wind_starts_graph <- mean_wind_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HWNDS sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(mean_wind_starts_graph)
saveWidget(t, "HWNDS_sakums.html", selfcontained = T)

# max_wind
max_wind_starts <- max_wind_data_long %>% 
  H_starts()

max_wind_starts_graph <- max_wind_starts %>%
  ggplot() +
  geom_point(aes(Stacija, Datums_laiks), size = 1.5) +
  coord_flip() +
  ggtitle("HWSMX sākuma datums") + 
  labs(x = "Datums") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

t <- ggplotly(max_wind_starts_graph)
saveWidget(t, "HWSMX_sakums.html", selfcontained = T)

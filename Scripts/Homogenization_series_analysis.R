# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)

# Ielādē time-series ------------------------------------------------------

temp_daily <- read_csv("Dati/MeanT_daily.csv")

# Parastās normas
temp_daily <- temp_daily %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums))

# Pārsauc kolonnas, lai normāli rēķinātos normas
temp_daily <- temp_daily %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value"))


ACMANT_data_trn <- ACMANT_data_t %>% # No format_ACMANT.R skripta
  set_colnames(c("EG_GH_ID", "Value", "DATE"))

climatol_homdata_trn <- climatol_homdata %>%  # No Climatol_analysis.R
  pivot_longer(-Datums, names_to = "EG_GH_ID", values_to = "Value") %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value"))

# Plotly graphs -----------------------------------------------------------

temp_daily <- temp_daily %>%
  rename(UNH = Value)

ACMANT_daily <- ACMANT_data_trn %>%
  rename(ACMANT = Value)

Clim_daily <- climatol_homdata_trn %>%
  rename(CLIMATOL = Value)

tdata_join <- left_join(temp_daily, ACMANT_daily) %>%
  left_join(Clim_daily)

tdata_join <- tdata_join %>%
  filter(year(DATE) >= 1947 & year(DATE) <= 2020)

stat_monthly <- tdata_join %>%
  mutate(Gads = year(DATE),
         Menesis = month(DATE),
         Diena = day(DATE)) %>%
  group_by(EG_GH_ID, Gads, Menesis) %>%
  summarise(UNH = f_mean_na(UNH, percentage_na = 0.2, consec = T, consec_values = 3),
            ACMANT = mean(ACMANT, na.rm = F),
            CLIMATOL = mean(CLIMATOL, na.rm = F)) %>%
  mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-")))

stacs <- unique(stat_monthly$EG_GH_ID)
for (stac in stacs) {
  mong <- stat_monthly %>% 
    ungroup() %>%
    filter(EG_GH_ID == stac) %>%
    pivot_longer(c(UNH, ACMANT, CLIMATOL), names_to = "Parametrs", values_to = "Vertiba") %>%
    ggplot(aes(Datums, Vertiba, col = Parametrs)) +
    ggtitle(stac) +
    geom_line()
    # ggsave(paste0("Grafiki/Homog_normu_salidzinajums/Laikrindas/Monthly", stac, "_MeanTmon.png"))

  mongp <- ggplotly(mong)
  htmlwidgets::saveWidget(mongp, paste0(stac, "_MeanTmon.html"), selfcontained = T)
}

stat_yearly <- tdata_join %>%
  mutate(Gads = year(DATE),
         Menesis = month(DATE),
         Diena = day(DATE)) %>%
  group_by(EG_GH_ID, Gads) %>%
  summarise(UNH = f_mean_na(UNH, percentage_na = 0.2, consec = T, consec_values = 3),
            ACMANT = mean(ACMANT, na.rm = F),
            CLIMATOL = mean(CLIMATOL, na.rm = F)) %>%
  mutate(Datums = ymd(str_c(Gads, "01", "01", sep = "-")))

stacs <- unique(stat_monthly$EG_GH_ID)
for (stac in stacs) {
  yg <- stat_yearly %>% 
    ungroup() %>%
    filter(EG_GH_ID == stac) %>%
    pivot_longer(c(UNH, ACMANT, CLIMATOL), names_to = "Parametrs", values_to = "Vertiba") %>%
    ggplot(aes(Datums, Vertiba, col = Parametrs)) +
    ggtitle(stac) +
    geom_line()
    # ggsave(paste0("Grafiki/Homog_normu_salidzinajums/Laikrindas/Yearly/", stac, "_MeanTyear.png"))
  
  ygp <- ggplotly(yg)
  htmlwidgets::saveWidget(ygp, paste0(stac, "_MeanTyear.html"), selfcontained = T)
}


# Normu karšu zīmēšana ----------------------------------------------------



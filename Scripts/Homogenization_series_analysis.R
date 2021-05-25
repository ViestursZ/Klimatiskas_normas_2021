# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)


# Plotting variables ------------------------------------------------------

Graph_loc <- "./Grafiki/Homog_normu_salidzinajums/Mean_T/Laikrindas"

# Ielādē gaisa temperatūras koeficientus ----------------------------------


# Ielādē datus ------------------------------------------------------------
# Dati apstrādāti Normals_calculation.R skriptā
# Koriģētie dati
korig_temp_daily <- read_csv("Dati/MeanT_daily_korig.csv", col_types = c("Dcn"))

korig_temp_daily <- korig_temp_daily %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value")) %>%
  filter(year(DATE) >= 1991 & year(DATE) <= 2020)

# Koriģētie dati no normām
ACMANT_kor_data_trn 
climatol_cor_homdata_trn 



# Ielādē staciju metadatus ------------------------------------------------
# Aktuālās stacijas
homog_stacijas <- unique(korig_temp_daily$EG_GH_ID)

metadata <- read_rds("Dati/station_metadata.rds")

metadata <- metadata %>%
  filter(EG_EL_ABBREVIATION %in% c("HTDRY", "TDRY")) %>%
  # dplyr::select(EG_GH_ID, EG_EL_ABBREVIATION, REGULAR, BEGIN_DATE, END_DATE, TS_ID, TI_INTERVAL) %>%
  mutate(BEGIN_DATE = force_tz(BEGIN_DATE, "UTC"),
         END_DATE = force_tz(END_DATE, "UTC"))

metadata_filter <- metadata %>%
  filter(year(BEGIN_DATE) >= 1991) %>%
  filter(EG_GH_ID %in% homog_stacijas)

# Noņemu duplikātus
metadata_filter <- metadata_filter[!duplicated(metadata_filter[, c(1, 4)]), ]

# Noņem, kur sākuma un beigu datumi ir tā pati diena
metadata_filter <- metadata_filter %>%
  filter(!date(BEGIN_DATE) == date(END_DATE))



# Hpar_starts # No Data_cleaning_and_uniting.R

# Pāvietojumu dati
geog_data <- read_rds("Dati/geog_data.rds")

geog_data <- geog_data %>%
  filter(GH_ID %in% homog_stacijas)

# Atfiltrē visus end dates mazākus par year 3999. Tie apzīmē, ka stacija ir
# pēcāk tikusi pārvietota
geog_data <- geog_data %>% 
  select(GH_ID, BEGIN_DATE, END_DATE, GEOGR1, GEOGR2, REMARK) %>%
  arrange(GH_ID, BEGIN_DATE)

geog_data <- geog_data %>%
  group_by(GH_ID) %>%
  mutate(Selectable = ifelse(year(lag(END_DATE, 1)) < 3999, 1, 0)) %>%
  filter(Selectable == 1)

# Pievieno klāt manuāli vēl zināmos pārvietošanas eventus (no vēja homogenizācijas)

stacs <- c("RIBA99PA", "RIDM99MS", "RIJE99PA", "RIDM99MS", "RIAS99PA")
dates <- c(ymd("2016-02-24"), ymd("2016-12-08"), ymd("2016-08-22"), 
           ymd("1990-01-01"), ymd("1997-01-01"))

add_geog_dates <- data.frame(GH_ID = stacs, BEGIN_DATE = dates)

geog_data <- geog_data %>% 
  ungroup () %>%
  mutate(BEGIN_DATE = as_date(BEGIN_DATE),
         END_DATE = as_date(END_DATE)) %>%
  bind_rows(add_geog_dates) %>%
  select(-END_DATE, GEOGR1, GEOGR2) %>%
  mutate(REMARK = "Station movement") %>%
  select(-Selectable) %>%
  rename(EG_GH_ID = GH_ID) %>%
  mutate(BEGIN_DATE = as_datetime(BEGIN_DATE),
         REGULAR = "Y-N",
         EG_EL_ABBREVIATION = "TDRY-HTDRY")

metadata_filter <- metadata_filter %>%
  bind_rows(geog_data) %>%
  arrange(EG_GH_ID, BEGIN_DATE)

  
# Novāc visus TDRY, kuri ir lielāki par Hpar sākumu
mlist <- list()
for (i in seq_along(unique(Hpar_starts$Stacija))) {
  stac <- unique(Hpar_starts$Stacija)[i]
  fdat <- filter(Hpar_starts, Stacija == stac) %>%
    pull(Datums_laiks)
  mlist[[i]] <- metadata_filter %>% 
    filter(EG_GH_ID == stac) %>%
    filter((BEGIN_DATE < fdat & EG_EL_ABBREVIATION == "TDRY") | EG_EL_ABBREVIATION == "HTDRY")
}

metadata_filter <- metadata_filter %>%
  filter(!EG_GH_ID %in% unique(Hpar_starts$Stacija)) %>%
  bind_rows(mlist)
  
metadata_filter <- metadata_filter %>%
  mutate(EG_GH_ID = ifelse(EG_GH_ID == "RIAS99PA", "RIGASLU", EG_GH_ID))


# Plotly graphs -----------------------------------------------------------
korig_temp_daily <- korig_temp_daily %>%
  rename(UNH = Value)

ACMANT_daily <- ACMANT_data_trn %>%
  rename(ACMANT = Value)

Clim_daily <- climatol_cor_homdata_trn %>%
  rename(CLIMATOL = Value)

tdata_join <- left_join(korig_temp_daily, ACMANT_daily) %>%
  left_join(Clim_daily)

tdata_join <- tdata_join %>%
  filter(year(DATE) >= 1991 & year(DATE) <= 2020)


# Monthly laikrindas
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
  metalines <- metadata_filter %>%
    filter(EG_GH_ID == stac)
  
  if (nrow(metalines > 0)) {
    metalines <- mutate(metalines,
                        Gads = year(BEGIN_DATE), Menesis = month(BEGIN_DATE),
                        Datums = ymd(str_c(Gads, Menesis, "01", sep = "-")))
  }
    
  mong <- stat_monthly %>% 
    ungroup() %>%
    filter(EG_GH_ID == stac) %>%
    pivot_longer(c(UNH, ACMANT, CLIMATOL), names_to = "Parametrs", values_to = "Vertiba") %>%
    ggplot(aes(Datums, Vertiba, col = Parametrs)) +
    ggtitle(stac) +
    geom_line()
    # ggsave(paste0(Graph_loc, "/Monthly/", stac, "_MeanTmon.png"))
  
  if (nrow(metalines > 0)) {
    mong <- mong + 
      geom_vline(data = metalines, aes(xintercept = Datums), linetype = "longdash", col = "dark green", size = 1)
      # ggsave(paste0(Graph_loc, "/Monthly/", stac, "_MeanTmon.png"))
  }
  mongp <- ggplotly(mong)
  if (nrow(metalines > 0)) {
    for (i in 1:nrow(metalines)) {
      mongp <- mongp %>%
        add_segments(x = as.numeric(metalines[i, "Datums", drop = T]), 
                     xend = as.numeric(metalines[i, "Datums", drop = T]),
                     y = -20,
                     yend = 30)
    }  
  }
    htmlwidgets::saveWidget(mongp, paste0(stac, "_MeanTmon.html"), selfcontained = T)
  mong + ggsave(paste0(Graph_loc, "/Monthly/", stac, "_MeanTmon.png"))
}

html_files <- list.files(pattern = ".html")
nordir <- paste0(Graph_loc, "/Monthly")
old_html_files <- list.files(nordir, pattern = "html", full.names = T)
file.remove(old_html_files)
file.copy(html_files, nordir)
file.remove(html_files)



# Yearly laikrindas
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
  # stac <- "AIZPUTE"
  metalines <- metadata_filter %>%
    filter(EG_GH_ID == stac)
  
  if (nrow(metalines > 0)) {
    metalines <- mutate(metalines,
                        Gads = year(BEGIN_DATE),
                        Datums = ymd(str_c(Gads, "01", "01", sep = "-")))
  }
  
  yg <- stat_yearly %>% 
    ungroup() %>%
    filter(EG_GH_ID == stac) %>%
    pivot_longer(c(UNH, ACMANT, CLIMATOL), names_to = "Parametrs", values_to = "Vertiba") %>%
    ggplot(aes(Datums, Vertiba, col = Parametrs)) +
    ggtitle(stac) +
    geom_line() 
    # ggsave(paste0(Graph_loc, "/Yearly/", stac, "_MeanTyear.png"))
  
  if (nrow(metalines > 0)) {
    yg <- yg + 
      geom_vline(data = metalines, aes(xintercept = Datums), linetype = "longdash", col = "dark green", size = 1)
      # ggsave(paste0(Graph_loc, "/Yearly/", stac, "_MeanTyear.png"))
  }
  
  ygp <- ggplotly(yg)
  if (nrow(metalines > 0)) {
    for (i in 1:nrow(metalines)) {
      ygp <- ygp %>%
        add_segments(x = as.numeric(metalines[i, "Datums", drop = T]), 
                     xend = as.numeric(metalines[i, "Datums", drop = T]),
                     y = -20,
                     yend = 30)
    }  
  }
  
  htmlwidgets::saveWidget(ygp, paste0(stac, "_MeanTyear.html"), selfcontained = T)
  yg + ggsave(paste0(Graph_loc, "/Yearly/", stac, "_MeanTyear.png"))
}

html_files <- list.files(pattern = ".html")
nordir <- paste0(Graph_loc, "/Yearly")
old_html_files <- list.files(nordir, pattern = "html", full.names = T)
file.remove(old_html_files)
file.copy(html_files, nordir)
file.remove(html_files)

# Linijas_pa_gadiem -------------------------------------------------------

# ACMANT_kor_data_t %>%
#   mutate(Gads = year(Datums),
#          Gada_diena = yday(Datums)) %>%
#   group_by(Gads) %>%
#   filter(Gads >= 1991) %>%
#   filter(Stacija == "RIDM99MS") %>%
#   ggplot() + 
#   geom_smooth(aes(Gada_diena, Merijums, group = Gads, col = Gads), se = F) + 
#   scale_color_distiller(palette = "Blues", direction = 1) +
#   ggtitle("ACMANT")
# 
# 
#   # 
# 
# korig_temp_daily %>%
#   mutate(Gads = year(DATE),
#          Gada_diena = yday(DATE)) %>%
#   group_by(Gads) %>%
#   filter(Gads >= 1991) %>%
#   filter(EG_GH_ID == "RIDM99MS") %>%
#   ggplot() + 
#   geom_smooth(aes(Gada_diena, Value, group = Gads, col = Gads), se = F) + 
#   scale_color_distiller(palette = "Blues", direction = 1) +
#   ggtitle("raw")

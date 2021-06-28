library(tidyverse)

# Read in the homogenized data --------------------------------------------

homog_dagda <- read_csv2("Dati/CLIMATOL_homog_Mean_T_daily_monbrks_arDAGDA.csv")
homog_snht20 <- read_csv2("Dati/CLIMATOL_homog_Mean_T_daily_monbrks_noDAGDA_snht20.csv")
homog_default <- read_csv2("Dati/CLIMATOL_homog_Mean_T.csv")


# Read in min-max data ----------------------------------------------------
MinT_korig <- read_csv("Dati/Min_T_daily_korig.csv", col_types = "Dcn")
MaxT_korig <- read_csv("Dati/Max_T_daily_korig.csv", col_types = "Dcn")


# Read in clean data ------------------------------------------------------

temp_dati <- read_csv("Dati/Mean_T_dati_clean.csv")
temp_d <- temp_dati %>%
  mutate(Datums_laiks = Datums_laiks + hours(2)) %>% # LV ziemas laiks 
  mutate(Datums = date(Datums_laiks)) %>%
  group_by(Stacija, Datums) %>%
  summarise(Merijums = round(f_mean_na(Merijums, percentage_na = 0.2, consec_values = F), 1))

# Apvieno pa stacijām -----------------------------------------------------
# Apvieno stacijas no dažādām homogenizācijas metodēm
homog_full <- homog_default %>%
  dplyr::select(-RIGASLU, -RIPR99PA) %>%
  bind_cols(dplyr::select(homog_snht20, RIGASLU, RIPR99PA)) %>% 
  bind_cols(dplyr::select(homog_dagda, RIDAGDA)) %>%
  pivot_longer(names_to = "Stacija", values_to = "Mean_T", -Datums)


# Pārbauda Min-Max vērtības, kad trūkst dažāda veida stacijas
homog_full

MinT_korig <- MinT_korig %>%
  rename(Min_T = Merijums)

MaxT_korig <- MaxT_korig %>%
  rename(Max_T = Merijums)

Temp_dati_all <- inner_join(homog_full, MinT_korig) %>%
  inner_join(MaxT_korig)

# Atfiltrē, kur MinT ir mazāka par vidējo. Pārbauda gadījumus, aiz --------
RIGASLU_reg_dates <- temp_d %>%
  filter(Stacija %in% c("RIGASLU", "RIAS99PA")) %>%
  spread(Stacija, Merijums) %>%
  filter(is.na(RIGASLU) & !is.na(RIAS99PA)) %>% 
  pull(Datums)
  
RIGASLU_ir_dates <- temp_d %>%
  filter(Stacija %in% c("RIGASLU")) %>%
  spread(Stacija, Merijums) %>%
  filter(!is.na(RIGASLU)) %>%
  pull(Datums)

# Ja nav Max_T, tad Mean_T == NA
Temp_dati_all <- Temp_dati_all %>% 
  mutate(Mean_T = ifelse(Mean_T < Min_T & is.na(Max_T), NA, Mean_T)) %>% # Ja Max_T == NA, tad vnk izņem vērtību
  mutate(Mean_T = ifelse(Mean_T < Min_T & (!Stacija %in% c("RIGASLU")),
                         round(mean(c(Min_T, Max_T)), 1), Mean_T)) %>% # Ja nav Rīga, tad vienkārši aizvieto no 2 vērtībām
  mutate(Mean_T = ifelse(Mean_T < Min_T & Datums %in% RIGASLU_ir_dates,  
                         round(mean(c(Min_T, Max_T)), 1), Mean_T)) # Ja ir Rīga, bet datumi neiztrūkst no RIGASLU, tad aprēķina no min un max



# Atfiltrē, kur MeanT > MaxT ----------------------------------------------
Temp_dati_all %>%
  filter(Mean_T > Max_T) %>%
  group_by(Stacija) %>%
  summarise(Skaits  = n())
  # filter(Stacija == "RIGASLU")

# Veic tos pašus čekus, kādus minimālajai temperatūrai
Temp_dati_all <- Temp_dati_all %>% 
  mutate(Mean_T = ifelse(Mean_T > Max_T & is.na(Min_T), NA, Mean_T)) %>% # Ja Mean_T == NA, tad vnk izņem vērtību
  mutate(Mean_T = ifelse(Mean_T > Max_T & (!Stacija %in% c("RIGASLU")),
                         round(mean(c(Min_T, Max_T)), 1), Mean_T)) %>%  # Ja nav Rīga, tad vienkārši aizvieto no 2 vērtībām
  mutate(Mean_T = ifelse(Mean_T > Max_T & Datums %in% RIGASLU_ir_dates,  
                         round(mean(c(Min_T, Max_T)), 1), Mean_T)) %>% # Ja ir Rīga, bet datumi neiztrūkst no RIGASLU, tad aprēķina no min un max
  mutate(Mean_T = ifelse(Mean_T > Max_T, NA, Mean_T)) # Atlikušos iztrūkumus aizvieto ar NA
  
homog_all_correct <- Temp_dati_all %>%
  dplyr::select(Datums, Stacija, Mean_T) %>%
  arrange(Stacija, Datums)

homog_all_correct %>%
  write_excel_csv2("Dati/Mean_T_homog_FINAL_CLIMATOL_apvienots.csv")

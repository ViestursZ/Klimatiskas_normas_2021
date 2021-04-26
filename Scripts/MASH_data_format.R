# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(sf)
library(lubridate)

# Functions ---------------------------------------------------------------

source("D:/Viesturs_Zandersons/Scripts/Noderigas_R_funkcijas/Recode_stations.R",
       encoding = "UTF-8")

# Sakārto datus -----------------------------------------------------------

temp_daily <- read_csv("Dati/MeanT_daily.csv")

temp_daily <- temp_daily %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums))

# Atfiltrē gadus ar most vērtībām 
temp_daily <- temp_daily %>%
  arrange(Stacija, Datums) %>%
  filter(year(Datums) >= 1947 & year(Datums) <= 2020)

# MASH ņem pretī stacijas, kurās ir mazāk kā 30% datu iztrūkums
nevajag_stacs <- temp_daily %>% 
  group_by(Stacija) %>%
  summarise(NA_prec = sum(is.na(Merijums) / n()) * 100) %>% View()
  filter(NA_prec >= 19) %>% 
  pull(Stacija)
  

temp_daily <- temp_daily %>%
  filter(!Stacija %in% nevajag_stacs)

temp_daily <- temp_daily %>%
  arrange(Stacija, Datums)

# Unikālās stacijas
data_stacs <- temp_daily$Stacija %>% unique()

# Ielādē koordinātu datus priekš CLIMATOL
visas_stacijas <- st_read("Dati/Stacijas_sampl_LKS_25_02_2020.gpkg")

temp_stacs <- visas_stacijas %>%
  filter(GH_ID %in% data_stacs)

temp_stacs <- temp_stacs %>%
  st_transform(4326)

temp_coords <- st_coordinates(temp_stacs) %>%
  as.data.frame() %>%
  set_colnames(c("Lon", "Lat"))

mash_temp_coords <- temp_stacs %>%
  bind_cols(temp_coords) %>%
  select(GH_ID, ELEVATION, Lon, Lat) %>%
  as.data.frame()

mash_temp_coords <- mash_temp_coords %>%
  select(-geom) %>%
  select(Lon, Lat, ELEVATION, GH_ID) %>%
  mutate(Lon = round(Lon, 8),
         Lat = round(Lat, 8))

mash_temp_coords <- mash_temp_coords %>%
  arrange(GH_ID)

mash_temp_coords <- mash_temp_coords %>% 
  mutate(Stacija = recode_stations(GH_ID, "ID-LV")) %>%
  mutate(Stacija = ifelse(Stacija == "Rīga Universitāte", "Rīga", Stacija)) %>%
  mutate(Stacija = ifelse(Stacija == "RIGAAIR", "Rīga-Lidosta", Stacija))

mash_temp_coords <- mash_temp_coords %>%
  filter(!Stacija %in% nevajag_stacs)

# Koordinātu tabulas noformēšana ------------------------------------------

mash_coords_print <- mash_temp_coords %>%
  mutate(Index = row_number()) %>%
  select(Index, Lon, Lat, Stacija) %>%
  set_colnames(c("index", "lambda(x)", "fi(y)", ""))
  
mash_coords_print[, c(2:3)] %<>%
  format.data.frame(nsmall = 8, width = 14)


mash_coords_print[, c(4)] <- map2(rep("   ", nrow(mash_coords_print)), mash_coords_print[, c(4)], paste) %>% as.character() 


# Staciju_coords_print %<>% ungroup()
mash_coords_print[, 1] <- mash_coords_print[, 1] %>% format(width = 4)

#Ievieto kolonnu nosaukumus pirmajā rindā
mash_coords_print <- rbind(names(mash_coords_print), mash_coords_print)
mash_coords_print[1,2] <- "   lambda(x)"
mash_coords_print[1,3] <- "       fi(y)"

# Ieraksta koordinātu tabulu
write.table(mash_coords_print, file = "Dati/Mash_format/filastat.par", sep = "",
            row.names = F, col.names = F, append = F, quote = F)

# Datu noformēšana --------------------------------------------------------
temp_daily_mash <- temp_daily

temp_daily_mash <- temp_daily_mash %>%
  filter(Stacija != "TEST10M") %>%
  mutate(Stacija = recode_stations(Stacija, "ID-LV")) %>%
  mutate(Stacija = ifelse(Stacija == "RIGAAIR", "Rīga-Lidosta", Stacija)) %>%
  mutate(Stacija = ifelse(Stacija == "Rīga Universitāte", "Rīga", Stacija))

temp_daily_mash$Stacija %>% unique()

temp_daily_mash <- temp_daily_mash %>%
  pivot_wider(names_from = Stacija, values_from = Merijums) %>%
  mutate(Gads = year(Datums),
         Menesis = month(Datums), 
         Datums = day(Datums)) %>%
  select(Gads, Menesis, Datums, everything())


# Aizvieto NA ar 9999.99
temp_daily_mash[is.na(temp_daily_mash)] <- 9999.99

# Format numbers
temp_daily_mash[, -c(1:3)] %<>%
  round(2) %>%
  format.data.frame(trim = TRUE, digits = 2, nsmall = 2, width = 8, scientific = FALSE)

temp_daily_mash[, c(2:3)] %<>%
  round(0) %>%
  format.data.frame(nsmall = 0, width = 2, scientific = FALSE)

temp_daily_mash <- temp_daily_mash %>%
  mutate(Gads = format(Gads, nsmall = 0, width = 0, scientific = FALSE))

colnms <- names(temp_daily_mash)
rec_vec <- c("Ainaži" = "ain",  "Aizpute" = "aiz", "Alūksne" = "alu", 
             "Bauska" = "bau", "Dagda" = "dag", "Daugavgrīva" = "dgv", 
             "Daugavpils" = "dgp", "Dobele" = "dob", "Gulbene"  = "gul",
             "Jelgava" = "jel", "Kalnciems" = "kcm", "Kolka" = "kol",  
             "Kuldīga" = "kul", "Lielpēči" = "lpc", "Liepāja" = "lpj", 
             "Madona" = "mdn", "Mērsrags" = "mrs", "Pāvilosta" = "pvl",
             "Piedruja" = "pdr", "Priekuļi" = "prk", "Rēzekne" = "rez",
             "Rīga" = "rgu", "Rīga-Lidosta" = "rgl", "Rucava" = "ruc", 
             "Rūjiena" = "ruj", "Saldus" = "sal", "Sigulda" = "sig",
             "Sīļi" = "sil", "Skrīveri" = "skr", "Skulte" = "sku",
             "Stende" = "ste", "Ventspils" = "ven", "Vičaki" = "vic", 
             "Zīlāni" = "zil", "Zosēni" = "zos")

names(temp_daily_mash) <- colnms %>% recode(!!!rec_vec)

# Pārsauc vārdus atbilstoši MASH dataformātam un staciju abreviatūrām(Viestura izdomātas):

temp_daily_mash <- temp_daily_mash %>%
  mutate_all(as.character)

# Ievieto staciju vārdus pirmajā data frame rindā, lai failu ierakstot tie būtu pareizi noformēti
temp_daily_mash1 <- rbind(c("    ", "  ", "  ", paste("     ", names(temp_daily_mash)[-c(1:3)], sep = "")), temp_daily_mash)


# FAILA IZVEIDOŠANA
# Ieraksta noformēto Dv1 failu kā tab delimited *.txt failu
write.table(temp_daily_mash1, file = "Dati/Mash_format/Daily.dat", sep = "", row.names = F, col.names = F, append = F, 
            quote = F)







# Libraries ---------------------------------------------------------------

library(sf)
library(sp)
library(raster)
library(gstat)
library(tidyverse)
library(ggrepel)
library(lubridate)


# Plotting variables ------------------------------------------------------

Graph_loc <- "./Grafiki/Homog_normu_salidzinajums/Mean_T"

# Funkcijas --------------------------------------------------------------------

# Universal kriging function
uk_interpol <- function(observations, pred_grid, pred_par = pred_param,
                        model = inter_model, range = inter_range, 
                        vario_type = inter_vtype, resolut = izskirtspeja) {
  
  if (suppressWarnings(any(is.na(model)))) {
    mod <- with(observations, t ~ x + y + I(x^2) + I(y^2) + x * y + cont + h)
  } else {
    mod <- model
  }
  
  if (is.na(inter_range)) {
    if (pred_par == "temperatura") {
      ran <- 39500
    } else {
      ran <- 41500
    }
  } else {
    ran <- range
  }
  print(ran)
  
  if (is.na(vario_type)) {
    vtype <- "Exp"
  } else {
    vtype <- vario_type
  }
  
  vario <- variogram(object = mod, data = observations)  # sample variogramm
  vmodel <- vgm(var(observations$t), vtype, range = ran, nugget = 0) # theoretical varogramm
  v.fit <- fit.variogram(vario, vmodel, fit.ranges = F, fit.sills = c(F, T)) #fitting
  if (attr(v.fit, "singular")) err <- 1 else err <- 0 # a logical attribute that indicates whether the
  # non-linear fit converged (FALSE), or ended in a singularity
  att <- attr(v.fit, "SSErr")
  uk  <- krige(mod, observations, pred_grid, v.fit) #interpolation
  # seting limits for values
  uk$var1.pred[uk$var1.pred >= max(observations$t)] <- max(observations$t) # Changes interpolated values, so they are never more than max value
  uk$var1.pred[uk$var1.pred <= min(observations$t)] <- min(observations$t) # Changes interpolated values, so they are never less than min value
  
  # Variogram plot
  uk_plot <- plot(vario, v.fit)
  
  # Error check stacijās pie dažādiem grid
  grstp <- if (resolut == "1x1") 500 else 5000  
  
  N <- vector()
  k <- dim(observations)[1]
  for (i in 1:k) {N[i] <- max(which(pred_grid$x - grstp <= observations$x[i] & pred_grid$y - grstp <= observations$y[i]))}
  N <- N[!is.na(N)]
  rmse <- sqrt(mean((observations$t - uk$var1.pred[N])^2, na.rm = TRUE)) #RMSE
  merr <- max(abs(observations$t - uk$var1.pred[N]), na.rm = TRUE) # Max error
  meer <- mean(observations$t - uk$var1.pred[N], na.rm = TRUE) # Mean error
  mab <- mean(abs(observations$t - uk$var1.pred[N]), na.rm = TRUE) # Mean absolute bias
  # Return
  krig_result <- list("uk" = uk, 
                      "variogram_plot" = uk_plot, 
                      "error" = rbind(rmse,merr,meer,mab))
  return(krig_result)
}


# Ielādē normu datus ------------------------------------------------------

# No Average_normals_calculation.R skripta
# uh_day_normal 
# uh_dec_normal 
# uh_month_normal 

uh_day_korig_normal
uh_dec_korig_normal
uh_month_korig_normal
uh_year_korig_normal

old_day
old_dec
old_mon

# ac_day_normal
# ac_dec_normal
# ac_month_normal

ac_day_korig_normal
ac_dec_korig_normal
ac_month_korig_normal
ac_year_korig_normal

# clim_day_normal
# clim_dec_normal
# clim_month_normal

clim_day_korig_normal
clim_dec_korig_normal
clim_month_korig_normal
clim_year_korig_normal 


# Apvieno dekāžu normas un izveido kartes ---------------------------------
# uh_dec_norm_l <- uh_dec_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "UNH")

uh_dec_korig_norm_l <- uh_dec_korig_normal %>%
  pivot_longer(-DATE, names_to = "Stacija", values_to = "UNH_korig")

old_norm_l <- old_dec %>%
  pivot_longer(-DATE, names_to = "Stacija", values_to = "Old_norma")

# ac_dec_norm_l <- ac_dec_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "ACMANT")

ac_dec_korig_norm_l <- ac_dec_korig_normal %>%
  pivot_longer(-DATE, names_to = "Stacija", values_to = "ACMANT_korig")

# clim_dec_norm_l <- clim_dec_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "CLIMATOL")

clim_dec_korig_norm_l <- clim_dec_korig_normal %>%
  pivot_longer(-DATE, names_to = "Stacija", values_to = "CLIMATOL_korig")

# dec_norm_l <- inner_join(uh_dec_norm_l, uh_dec_korig_normal) %>%
#   inner_join(old_norm_l) %>%
#   inner_join(ac_dec_norm_l) %>%
#   inner_join(ac_dec_korig_norm_l) %>%
#   inner_join(clim_dec_norm_l) %>%
#   inner_join(clim_dec_korig_norm_l) 

dec_norm_l <- full_join(uh_dec_korig_norm_l, old_norm_l) %>%
  full_join(ac_dec_korig_norm_l) %>%
  full_join(clim_dec_korig_norm_l) 


dec_norm_l_starpibas <- dec_norm_l %>%
  transmute(DATE = DATE,
            Stacija = Stacija,
            # UNH = UNH - Old_norma,
            UNH_cor = UNH_korig - Old_norma,
            # ACMANT = ACMANT - Old_norma,
            ACMANT_cor = ACMANT_korig - Old_norma,
            # CLIMATOL = CLIMATOL - Old_norma,
            CLIMATOL_cor = CLIMATOL_korig - Old_norma)


dec_norm_l <- dec_norm_l %>%
  pivot_longer(-c(DATE, Stacija), names_to = "Metode", values_to = "Val")

dec_norm_l_starpibas <- dec_norm_l_starpibas %>%
  pivot_longer(-c(DATE, Stacija), names_to = "Metode", values_to = "Val")

dec_norm_l <- dec_norm_l %>%
  filter(Stacija %in% c("RIAI99PA", "RIAL99MS", "RIBA99PA", "RIDAGDA","RIDM99MS", "RIDO99MS",
                        "RIGASLU", "RIGU99MS", "RIJE99PA", "RIKO99PA", "RILP99PA", "RIMADONA",
                        "RIME99MS", "RIPA99PA", "RIPR99PA", "RIREZEKN", "RIRU99PA", "RISA99PA",
                        "RISE99MS", "RISI99PA", "RIST99PA", "RIVE99PA", "RIZI99PA", "RIZO99MS", 
                        "RUCAVA"))

dec_norm_l_starpibas <- dec_norm_l_starpibas %>%
  filter(Stacija %in% c("RIAI99PA", "RIAL99MS", "RIBA99PA", "RIDAGDA","RIDM99MS", "RIDO99MS",
                       "RIGASLU", "RIGU99MS", "RIJE99PA", "RIKO99PA", "RILP99PA", "RIMADONA",
                       "RIME99MS", "RIPA99PA", "RIPR99PA", "RIREZEKN", "RIRU99PA", "RISA99PA",
                       "RISE99MS", "RISI99PA", "RIST99PA", "RIVE99PA", "RIZI99PA", "RIZO99MS", 
                       "RUCAVA"))


# Normu salīdzinājumu grafiki ---------------------------------------------
# Dekāžu normu salīdzinājumi

dec_norm_stacs <- unique(dec_norm_l_starpibas$Stacija)
for (i in seq_along(dec_norm_stacs)) {
  dec_norm_l_starpibas %>%
    filter(Stacija == dec_norm_stacs[i]) %>%
    ggplot() +
    geom_point(aes(DATE, Val, col = Metode), size = 3) +
    geom_line(aes(DATE, Val, col = Metode, group = Metode)) + 
    ggtitle(dec_norm_stacs[i]) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5)) +
    # facet_wrap(~Stacija) +
    ggsave(paste0(Graph_loc, "/Dekades_starpibas/", dec_norm_stacs[i], "_dek_starpibas.png"))
  # dev.off()
}


  
# Ielādē telpiskos parametrus ---------------------------------------------
izskirtspeja <- "1x1"
pred_param =  "temperatura"

# Ielādē grid file, nomaina parametrus
if (izskirtspeja == "10x10") {
  if (pred_param == "nokrisni") {
    gridfile <- "Scripts/Interpolation_files/xy_h_Gams_xy_10x10.csv"
    
    # Ielādē grid
    grid <- read.csv(gridfile) 
    colnames(grid) <- c("X", "h", "Stations", "x", "y", "cont")
    coordinates(grid) <- c("x", "y")
    
  } else if (pred_param == "temperatura") {
    gridfile <- "Scripts/Interpolation_files/xy_h_sea_Gorcz-xyh_10x10.csv"
    
    # Ielādē grid
    grid <- read.csv(gridfile) 
    colnames(grid) <- c("X", "h", "to_sea", "Stations", "x", "y", "cont")
    coordinates(grid) <- c("x", "y")
  } else {
    print("Need valid parameter")
  }
} else if (izskirtspeja == "1x1") {
  if (pred_param == "nokrisni") {
    gridfile <- "Scripts/Interpolation_files/1x1_LV_grid_Gams_xy_2018.csv"
  } else if (pred_param == "temperatura") {
    gridfile <- "Scripts/Interpolation_files/1x1_LV_grid_Gorcz_xyh_2018.csv"
  } else {
    print("Need valid parameter")
  }
  # Ielādē grid
  grid <- read.csv(gridfile)
  colnames(grid)<-c("X","x","y","Lakes","Rivers","Water", "Sea_area","Population",
                    "Stations", "To_bay","Sea_amount","h1","h","Forest","to_sea",
                    "to_opensea", "h10", "Bogs","cont")
  coordinates(grid) <- c("x", "y")
}

stacijas_st <- st_read("Dati/Stacijas_sampl_LKS_25_02_2020.gpkg")


# Uzzīmē dekādes
dekades <- unique(dec_norm_l$DATE)

for (j in seq_along(dekades)) {
  dec_norm_st <- dec_norm_l %>%
    filter(DATE == dekades[j]) %>%
    left_join(stacijas_st, by = c("Stacija" = "GH_ID")) %>% 
    dplyr::select(DATE, Stacija, Metode, h = ELEVATION, cont = Temperatura_kontinentalitate, 
           x = X, y = Y, t = Val, geom)
  
  # Viena metode at a time
  metodes <- unique(dec_norm_st$Metode)
  met_list <- list()
  
  for (i in seq_along(metodes)) {
    dec_norm_st_2 <- dec_norm_st %>%
      filter(Metode == metodes[i]) %>%
      filter(!is.na(t))
    
    dec_norm_st_2 <- st_as_sf(dec_norm_st_2, crs = 3059)
    dec_norm_sp <- as_Spatial(dec_norm_st_2)
    
    proj4string(grid) <- proj4string(dec_norm_sp)
    
    # Pārējie interpolācijas parametri
    # Ja NA, tad default vērtības
    inter_range <- NA
    inter_model <- NA
    inter_vtype <- NA
    
    krig <- uk_interpol(dec_norm_sp, grid, model = t ~ 1)
    
    norm_raster <- raster(`gridded<-`(krig$uk, T)) %>%
      rasterToPoints() %>%
      as.data.frame() %>% 
      `names<-`(c("X", "Y", metodes[i]))
    
    met_list[[i]] <- norm_raster
  }
  
  inter_data <- met_list[[1]] %>%
    inner_join(met_list[[2]]) %>%
    inner_join(met_list[[3]]) %>%
    inner_join(met_list[[4]])
    # inner_join(met_list[[5]]) %>%
    # inner_join(met_list[[6]]) %>%
    # inner_join(met_list[[7]])
  
  inter_data <- inter_data %>%
    pivot_longer(-c(X, Y), names_to = "Metode", values_to = "Val")
  
  # Dekāžu normu plot
  
  ggplot() +
    geom_raster(data = inter_data, aes(X, Y, fill = Val)) +
    geom_sf(data = dec_norm_st, aes(geometry = geom)) +
    geom_text_repel(data = dec_norm_st,
                    aes(label = round(t, 1), geometry = geom),
                    stat = "sf_coordinates") +
    coord_sf(datum = "+init=epsg:3059") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    ggtitle(paste0("Vidējā gaisa temperatūra, dekāde ", dekades[j])) + 
    # geom_text_repel() +
    facet_wrap(~Metode) + 
    ggsave(paste0(Graph_loc, "/Dekades_kartes/Dekade_", dekades[j], ".png"),
                  device = png(width = 1100, height = 700))
    dev.off()
}


# Uzzīmē dekāžu starpības ar veco normu
dec_norm_l_starpibas

dekades <- unique(dec_norm_l$DATE)

for (j in seq_along(dekades)) {
  dec_norm_st <- dec_norm_l_starpibas %>%
    filter(DATE == dekades[j]) %>%
    left_join(stacijas_st, by = c("Stacija" = "GH_ID")) %>% 
    dplyr::select(DATE, Stacija, Metode, h = ELEVATION, cont = Temperatura_kontinentalitate, 
           x = X, y = Y, t = Val, geom)
  
  # Viena metode at a time
  metodes <- unique(dec_norm_st$Metode)
  met_list <- list()
  
  for (i in seq_along(metodes)) {
    dec_norm_st_2 <- dec_norm_st %>%
      filter(Metode == metodes[i]) %>%
      filter(!is.na(t))
    
    dec_norm_st_2 <- st_as_sf(dec_norm_st_2, crs = 3059)
    dec_norm_sp <- as_Spatial(dec_norm_st_2)
    
    proj4string(grid) <- proj4string(dec_norm_sp)
    
    # Pārējie interpolācijas parametri
    # Ja NA, tad default vērtības
    inter_range <- NA
    inter_model <- NA
    inter_vtype <- NA
    
    krig <- uk_interpol(dec_norm_sp, grid, model = t ~ 1)
    
    norm_raster <- raster(`gridded<-`(krig$uk, T)) %>%
      rasterToPoints() %>%
      as.data.frame() %>% 
      `names<-`(c("X", "Y", metodes[i]))
    
    met_list[[i]] <- norm_raster
  }
  
  inter_data <- met_list[[1]] %>%
    inner_join(met_list[[2]]) %>%
    inner_join(met_list[[3]])
    # inner_join(met_list[[4]]) %>%
    # inner_join(met_list[[5]]) %>%
    # inner_join(met_list[[6]])
  
  inter_data <- inter_data %>%
    pivot_longer(-c(X, Y), names_to = "Metode", values_to = "Val")
  
  # Dekāžu normu plot
  limits <- max(abs(inter_data$Val)) * c(-1, 1) #Limiti +  ir sarkani, - zili
  
  ggplot() +
    geom_raster(data = inter_data, aes(X, Y, fill = Val)) +
    geom_sf(data = dec_norm_st, aes(geometry = geom)) +
    geom_text_repel(data = dec_norm_st,
                    aes(label = round(t, 1), geometry = geom),
                    stat = "sf_coordinates") +
    coord_sf(datum = "+init=epsg:3059") +
    scale_fill_distiller(palette = "RdBu", direction = -1, limit = limits) +
    ggtitle(paste0("Jaunās gaisa temperatūras normas salīdzinājums ar veco normu, dekāde ", dekades[j])) + 
    # geom_text_repel() +
    facet_wrap(~Metode) + 
    ggsave(paste0(Graph_loc, "/Dekades_kartes/Dekade_", dekades[j], ".png"),
           device = png(width = 1100, height = 700))
  dev.off()
}


# Apvieno menesu normas ---------------------------------------------------
# uh_men_norm_l <- uh_month_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "UNH")
# 
# uh_men_korig_normal <- uh_month_korig_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "UNH_korig")
# 
# old_norm_month_l <- old_mon %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "Old_norma")
# 
# ac_men_norm_l <- ac_month_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "ACMANT")
# 
# ac_men_korig_norm_l <- ac_month_korig_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "ACMANT_korig")
# 
# clim_men_norm_l <- clim_month_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "CLIMATOL")
# 
# clim_men_korig_norm_l <- clim_month_korig_normal %>%
#   pivot_longer(-DATE, names_to = "Stacija", values_to = "CLIMATOL_korig")
# 
# men_norm_l <- inner_join(uh_men_norm_l, uh_men_korig_normal) %>%
#   inner_join(old_norm_month_l) %>%
#   inner_join(ac_men_norm_l) %>%
#   inner_join(ac_men_korig_norm_l) %>%
#   inner_join(clim_men_norm_l) %>%
#   inner_join(clim_men_korig_norm_l) 
# 
# men_norm_l_starpibas <- men_norm_l %>%
#   transmute(DATE = DATE,
#             Stacija = Stacija,
#             UNH = UNH - Old_norma,
#             UNH_cor = UNH_korig - Old_norma,
#             ACMANT = ACMANT - Old_norma,
#             ACMANT_cor = ACMANT_korig - Old_norma,
#             CLIMATOL = CLIMATOL - Old_norma,
#             CLIMATOL_cor = CLIMATOL_korig - Old_norma)
# 
# 
# men_norm_l <- men_norm_l %>%
#   pivot_longer(-c(DATE, Stacija), names_to = "Metode", values_to = "Val")
# 
# men_norm_l_starpibas <- men_norm_l_starpibas %>%
#   pivot_longer(-c(DATE, Stacija), names_to = "Metode", values_to = "Val")
# 
# 
# # Mēnešu normas -----------------------------------------------------------
# menesi <- unique(men_norm_l$DATE)
# 
# for (j in seq_along(menesi)) {
#   men_norm_st <- men_norm_l %>%
#     filter(DATE == menesi[j]) %>%
#     left_join(stacijas_st, by = c("Stacija" = "GH_ID")) %>% 
#     select(DATE, Stacija, Metode, h = ELEVATION, cont = Temperatura_kontinentalitate, 
#            x = X, y = Y, t = Val, geom)
#   
#   # Viena metode at a time
#   metodes <- unique(men_norm_st$Metode)
#   met_list <- list()
#   
#   for (i in seq_along(metodes)) {
#     men_norm_st_2 <- men_norm_st %>%
#       filter(Metode == metodes[i]) %>%
#       filter(!is.na(t))
#     
#     men_norm_st_2 <- st_as_sf(men_norm_st_2, crs = 3059)
#     men_norm_sp <- as_Spatial(men_norm_st_2)
#     
#     proj4string(grid) <- proj4string(men_norm_sp)
#     
#     # Pārējie interpolācijas parametri
#     # Ja NA, tad default vērtības
#     inter_range <- NA
#     inter_model <- NA
#     inter_vtype <- NA
#     
#     krig <- uk_interpol(men_norm_sp, grid, model = t ~ 1)
#     
#     norm_raster <- raster(`gridded<-`(krig$uk, T)) %>%
#       rasterToPoints() %>%
#       as.data.frame() %>% 
#       `names<-`(c("X", "Y", metodes[i]))
#     
#     met_list[[i]] <- norm_raster
#   }
#   
#   inter_data <- met_list[[1]] %>%
#     inner_join(met_list[[2]]) %>%
#     inner_join(met_list[[3]]) %>%
#     inner_join(met_list[[4]]) %>%
#     inner_join(met_list[[5]]) %>%
#     inner_join(met_list[[6]]) %>%
#     inner_join(met_list[[7]])
#   
#   inter_data <- inter_data %>%
#     pivot_longer(-c(X, Y), names_to = "Metode", values_to = "Val")
#   
#   # Mēnešu normu plot
#   
#   ggplot() +
#     geom_raster(data = inter_data, aes(X, Y, fill = Val)) +
#     geom_sf(data = men_norm_st, aes(geometry = geom)) +
#     geom_text_repel(data = men_norm_st,
#                     aes(label = round(t, 1), geometry = geom),
#                     stat = "sf_coordinates") +
#     coord_sf(datum = "+init=epsg:3059") +
#     scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#     ggtitle(paste0("Vidējā gaisa temperatūra, mēnesis ", menesi[j])) + 
#     # geom_text_repel() +
#     facet_wrap(~Metode) + 
#     ggsave(paste0("./Grafiki/Homog_normu_salidzinajums/Menesi_kartes/Menesis_", menesi[j], ".png"),
#            device = png(width = 1100, height = 700))
#   dev.off()
# }
# 
# 
# 
# # Mēnešu normu starpības
# menesi <- unique(men_norm_l_starpibas$DATE)
# 
# for (j in seq_along(menesi)) {
#   men_norm_st <- men_norm_l_starpibas %>%
#     filter(DATE == menesi[j]) %>%
#     left_join(stacijas_st, by = c("Stacija" = "GH_ID")) %>% 
#     select(DATE, Stacija, Metode, h = ELEVATION, cont = Temperatura_kontinentalitate, 
#            x = X, y = Y, t = Val, geom)
#   
#   # Viena metode at a time
#   metodes <- unique(men_norm_st$Metode)
#   met_list <- list()
#   
#   for (i in seq_along(metodes)) {
#     men_norm_st_2 <- men_norm_st %>%
#       filter(Metode == metodes[i]) %>%
#       filter(!is.na(t))
#     
#     men_norm_st_2 <- st_as_sf(men_norm_st_2, crs = 3059)
#     men_norm_sp <- as_Spatial(men_norm_st_2)
#     
#     proj4string(grid) <- proj4string(men_norm_sp)
#     
#     # Pārējie interpolācijas parametri
#     # Ja NA, tad default vērtības
#     inter_range <- NA
#     inter_model <- NA
#     inter_vtype <- NA
#     
#     krig <- uk_interpol(men_norm_sp, grid, model = t ~ 1)
#     
#     norm_raster <- raster(`gridded<-`(krig$uk, T)) %>%
#       rasterToPoints() %>%
#       as.data.frame() %>% 
#       `names<-`(c("X", "Y", metodes[i]))
#     
#     met_list[[i]] <- norm_raster
#   }
#   
#   inter_data <- met_list[[1]] %>%
#     inner_join(met_list[[2]]) %>%
#     inner_join(met_list[[3]]) %>%
#     inner_join(met_list[[4]]) %>%
#     inner_join(met_list[[5]]) %>%
#     inner_join(met_list[[6]]) 
#   
#   inter_data <- inter_data %>%
#     pivot_longer(-c(X, Y), names_to = "Metode", values_to = "Val")
#   
#   # Mēnešu normu plot
#   
#   limits <- max(abs(inter_data$Val)) * c(-1, 1) #Limiti +  ir sarkani, - zili
#   
#   ggplot() +
#     geom_raster(data = inter_data, aes(X, Y, fill = Val)) +
#     geom_sf(data = men_norm_st, aes(geometry = geom)) +
#     geom_text_repel(data = men_norm_st,
#                     aes(label = round(t, 1), geometry = geom),
#                     stat = "sf_coordinates") +
#     coord_sf(datum = "+init=epsg:3059") +
#     scale_fill_distiller(palette = "RdBu", direction = -1, limit = limits) +
#     ggtitle(paste0("Jaunās gaisa temperatūras normas salīdzinājums ar veco normu, mēnesis ", menesi[j])) + 
#     # geom_text_repel() +
#     facet_wrap(~Metode) + 
#     ggsave(paste0("./Grafiki/Homog_normu_salidzinajums/Menesi_starpibas_kartes/Menesis_starp_", menesi[j], ".png"),
#            device = png(width = 1100, height = 700))
#   dev.off()
# }

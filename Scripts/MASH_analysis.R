# MASH datu sakārtošana un analīze

# Libraries ---------------------------------------------------------------
library(tidyverse)



# Datu kārtošana ----------------------------------------------------------

homog <- read_table("./MASH_LV_homog/Mash_LV_vejs_8th_try/MASHDaily/MASHDaily/DailyHom.dat")

LV_jaunas_stac <- readOGR("./GIS/LV_jaunas_stacijas_LKS.gpkg", encoding = "UTF-8", use_iconv = T)

#### Datu apstrāde ####
homog %<>%
  gather(key = "Stacija", value = "Vid_vejs", -c(1)) %>%
  transmute(Datums = ymd(paste0(str_sub(X1, 1, 4), "-", str_sub(X1, 5,6), "-", 
                                str_sub(X1, 7, 8))),
            Stacija = parse_factor(Stacija, levels = NULL),
            Vid_vejs = Vid_vejs)


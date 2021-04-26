
# Libraries ---------------------------------------------------------------
library(ggplot2) 
library(dplyr) 
library(scales) 
library(plotly)

Sys.setlocale("LC_ALL", "latvian_Latvia.1257")

#### Diennakts plot ####
norm1 <- uh_day_normal
norm1 <- norm1 %>% mutate(DATE=substr(as.POSIXct(norm1$DATE, format='%m-%d'),6,10))
norm11 <- norm1 %>% gather(EG_GH_ID, VALUE, -c(DATE))

#norma2 <- read.csv("temp_day_norm.csv")
norma2 <- ac_day_normal
norma2 <- norma2 %>% mutate(DATE=substr(as.POSIXct(norma2$DATE, format='%m-%d'),6,10))
norma22 <- norma2 %>% gather(EG_GH_ID, VALUE, -c(DATE))

norma3 <- clim_day_normal
norma3 <- norma3 %>% mutate(DATE=substr(as.POSIXct(norma3$DATE, format='%m-%d'),6,10))
norma33 <- norma3 %>% gather(EG_GH_ID, VALUE, -c(DATE))

normas_merge <- merge(norm11, norma22, by = c("EG_GH_ID","DATE")) %>%
  merge(norma33, by = c("EG_GH_ID","DATE")) %>%
  arrange(EG_GH_ID, DATE)
  
names(normas_merge)[c(3:5)] <- c("norma1", "norma2", "norma3")

normas_merge$DATE <- as.POSIXct(paste0(substr(normas_merge$DATE,4,5),"-",substr(normas_merge$DATE,1,2)), format = "%d-%m")
normas_merge <- normas_merge %>%  
  mutate(DATE2 = DATE + days(31)) 

stacijas <- unique(normas_merge$EG_GH_ID)

for(i in c(1:length(stacijas))){
  stac <- stacijas[i]
  ggday <- normas_merge[normas_merge$EG_GH_ID==stac,]
  
  gplot <- ggday %>% ggplot(aes(x = DATE2)) +
    geom_hline(yintercept = c(0,5,10,15,20), color = "gray", size=0.5) +
    geom_line(aes(DATE2, y = norma1, group = 1, color = "UH norma"), size = 1) +
    geom_line(aes(DATE2, y = norma2, group = 1, color = "AC norma"), size = 1) + 
    geom_line(aes(DATE2, y = norma3, group = 1, color = "Cl norma"), size = 1) +
    theme_classic() +
    scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                     labels = date_format("%1.%m.")) +
    xlab("Datums") +
    ylab("Vidējā gaisa temperatūra, °C") +
    labs(colour="") +
    ggtitle(paste0(stac, " diennakts temperatūras normas")) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("UH norma" = "blue3", "AC norma" = "brown3", "Cl norma" = "green3"))
  # ggsave(paste0("Grafiki/Homog_normu_salidzinajums/Dienas/", stac,"_dien_normas.png"))
  
  gpplot <- ggplotly(gplot)
  htmlwidgets::saveWidget(gpplot, file = paste0(stac,"_dien_normas.html"), selfcontained = T)
}

#### Dekāde plot ####
norm_dec1 <- uh_dec_normal
norm_dec1 <- norm_dec1 %>% mutate(DATE=substr(as.POSIXct(norm_dec1$DATE, format='%m-%d'),6,10))
norm_dec11 <- norm_dec1 %>% gather(EG_GH_ID, VALUE, -c(DATE))

norm_dec2 <- ac_dec_normal
norm_dec2 <- norm_dec2 %>% mutate(DATE=substr(as.POSIXct(norm_dec2$DATE, format='%m-%d'),6,10))
norm_dec22 <- norm_dec2 %>% gather(EG_GH_ID, VALUE, -c(DATE))

norm_dec3 <- clim_dec_normal
norm_dec3 <- norm_dec3 %>% mutate(DATE=substr(as.POSIXct(norm_dec3$DATE, format='%m-%d'),6,10))
norm_dec33 <- norm_dec3 %>% gather(EG_GH_ID, VALUE, -c(DATE))

dec_norm <- merge(norm_dec11, norm_dec22, by=c("EG_GH_ID","DATE")) %>% 
  merge(norm_dec33, by = c("EG_GH_ID","DATE")) %>%
  arrange(EG_GH_ID, DATE)
  
names(dec_norm)[c(3:5)] <- c("norm_dec1", "norm_dec2", "norm_dec3")
dec_norm$DATE <- as.POSIXct(paste0(substr(dec_norm$DATE,4,5),"-",substr(dec_norm$DATE,1,2)), format = "%d-%m")
#dec_norm <- dec_norm %>% mutate(DATE2 = DATE + days(31))

for(i in c(1:length(stacijas))){
  stac <- stacijas[i]
  
  ggdec <- dec_norm[dec_norm$EG_GH_ID==stac,]
  ggdec <- rbind(ggdec[rep(1, each = 10),], ggdec[rep(2, each = 10),], ggdec[rep(3, each = 11),],
                         ggdec[rep(4, each = 10),], ggdec[rep(5, each = 10),], ggdec[rep(6, each = 8),],
                         ggdec[rep(7, each = 10),], ggdec[rep(8, each = 10),], ggdec[rep(9, each = 11),],
                         ggdec[rep(10, each = 10),], ggdec[rep(11, each = 10),], ggdec[rep(12, each = 10),],
                         ggdec[rep(13, each = 10),], ggdec[rep(14, each = 10),], ggdec[rep(15, each = 11),],
                         ggdec[rep(16, each = 10),], ggdec[rep(17, each = 10),], ggdec[rep(18, each = 10),],
                         ggdec[rep(19, each = 10),], ggdec[rep(20, each = 10),], ggdec[rep(21, each = 11),],
                         ggdec[rep(22, each = 10),], ggdec[rep(23, each = 10),], ggdec[rep(24, each = 11),],
                         ggdec[rep(25, each = 10),], ggdec[rep(26, each = 10),], ggdec[rep(27, each = 10),],
                         ggdec[rep(28, each = 10),], ggdec[rep(29, each = 10),], ggdec[rep(30, each = 11),],
                         ggdec[rep(31, each = 10),], ggdec[rep(32, each = 10),], ggdec[rep(33, each = 10),],
                         ggdec[rep(34, each = 10),], ggdec[rep(35, each = 10),], ggdec[rep(36, each = 11),]) %>% 
    as.data.frame()

  ggdec <- cbind(DATE2 = normas_merge$DATE2, ggdec) # Datumi te ir ņemti no dienu zīmēšanas.
  
 gplot <- ggdec %>% ggplot(aes(x = DATE2)) +
    geom_hline(yintercept = c(0,5,10,15,20), color = "gray", size=0.5) +
    geom_line(aes(DATE2, y = norm_dec1, group = 1, color = "UH norma"), size = 1) +
    geom_line(aes(DATE2, y = norm_dec2, group = 1, col = "AC norma"), size = 1) + 
    geom_line(aes(DATE2, y = norm_dec3, group = 1, color = "Cl norma"), size = 1) +
    theme_classic() +
    scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                     labels = date_format("%1.%m.")) +
    xlab("Datums") +
    ylab("Vidējā gaisa temperatūra, °C") +
    labs(colour="")+
    ggtitle(paste0(stac, " dekades temperatūras normas")) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("UH norma" = "blue3", "AC norma" = "brown3", "Cl norma" = "green3"))  
    # ggsave(paste0("Grafiki/Homog_normu_salidzinajums/Dekades/", stac,"_dek_normas.png"))
 
 gpplot <- ggplotly(gplot)
 htmlwidgets::saveWidget(gpplot, file = paste0(stac,"_dec_normas.html"), selfcontained = T)
}


#### Mēneša plot ####

norm_mon1 <- uh_month_normal
norm_mon1$DATE <- substr(paste0("0",norm_mon1$DATE), nchar(paste0("0",norm_mon1$DATE))-1 , nchar(paste0("0",norm_mon1$DATE)))
norm_mon11 <- norm_mon1 %>% gather(EG_GH_ID, VALUE, -c(DATE))

norm_mon2 <- ac_month_normal
norm_mon2$DATE <- substr(paste0("0",norm_mon2$DATE), nchar(paste0("0",norm_mon2$DATE))-1 , nchar(paste0("0",norm_mon2$DATE)))
norm_mon22 <- norm_mon2 %>% gather(EG_GH_ID, VALUE, -c(DATE))

norm_mon3 <- clim_month_normal
norm_mon3$DATE <- substr(paste0("0",norm_mon3$DATE), nchar(paste0("0",norm_mon3$DATE))-1 , nchar(paste0("0",norm_mon3$DATE)))
norm_mon33 <- norm_mon3 %>% gather(EG_GH_ID, VALUE, -c(DATE))


mon_norm <- merge(norm_mon11, norm_mon22, by=c("EG_GH_ID","DATE")) %>% 
  merge(norm_mon33, by=c("EG_GH_ID","DATE")) %>%
  arrange(EG_GH_ID, DATE)
  
names(mon_norm)[c(3:5)] <- c("norm_mon1", "norm_mon2", "norm_mon3")
mon_norm$DATE <- as.POSIXct(paste0("01-",mon_norm$DATE), format = "%d-%m")

for(i in c(1:length(stacijas))){
  stac <- stacijas[i]
  
  ggmon <- mon_norm[mon_norm$EG_GH_ID==stac,]
  ggmon <- rbind(ggmon[rep(1, each = 31),], ggmon[rep(2, each = 28),],
                         ggmon[rep(3, each = 31),], ggmon[rep(4, each = 30),],
                         ggmon[rep(5, each = 31),], ggmon[rep(6, each = 30),],
                         ggmon[rep(7, each = 31),], ggmon[rep(8, each = 31),],
                         ggmon[rep(9, each = 30),], ggmon[rep(10, each = 31),],
                         ggmon[rep(11, each = 30),], ggmon[rep(12, each = 31),]) %>% as.data.frame()
  
  ggmon <- cbind(DATE2 = normas_merge$DATE2, ggmon) # Datumi te ir ņemti no dienu zīmēšanas.
  
  gplot <- ggmon %>% ggplot(aes(x = DATE2)) +
    geom_hline(yintercept = c(0,5,10,15,20), color = "gray", size = 0.5) +
    geom_line(aes(DATE2,y = norm_mon1, group = 1, color = "UH norma"), size = 1) +
    geom_line(aes(DATE2,y = norm_mon2, group = 1, col = "AC norma"), size = 1) + 
    geom_line(aes(DATE2, y = norm_mon3, group = 1, color = "Cl norma"), size = 1) +
    theme_classic() +
    scale_x_datetime(breaks = date_breaks(width = "1 month"), 
                     labels = date_format("%1.%m.")) +
    xlab("Datums") +
    ylab("Vidējā gaisa temperatūra, °C") +
    labs(colour="")+
    ggtitle(paste0(stac, " mēneša temperatūras normas")) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("UH norma" = "blue3", "AC norma" = "brown3", "Cl norma" = "green3"))
    # ggsave(paste0("Grafiki/Homog_normu_salidzinajums/Dekades/", stac,"_dek_normas.png"))
    
  gpplot <- ggplotly(gplot)
  htmlwidgets::saveWidget(gpplot, file = paste0(stac,"_mon_normas.html"), selfcontained = T)
}



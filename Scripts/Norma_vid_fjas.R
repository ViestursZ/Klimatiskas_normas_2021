library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(magrittr)

# Agregation functions ----------------------------------------------------

f_mean_na <- function(x, percentage_na = 0.2, consec = F, consec_values = 5) {
  # set consec to true if consecutive missing values are needed to be taken into 
  # account
  # If consec is true, set the number to how many values should be taken into 
  # account
  if(consec == F) {
    if(sum(is.na(x))/length(x) > percentage_na) {NA} else {mean(x, na.rm = T)}
  } else if (consec == T) {
    i <- x
    i[is.na(i)] <- -9999
    i <- rle(i)
    i <- any(i$lengths[which(i$values == -9999)] >= consec_values)
    if ((sum(is.na(x))/length(x) > percentage_na) | i == TRUE) {
      NA
    } else {
      mean(x, na.rm = T)
    }
  }
} 

f_sum_na <- function(x, percentage_na = 0, consec = F) {
  # set consec to true if consecutive missing days are neede to be taken into 
  # account
  if(consec == F) {
    if(sum(is.na(x))/length(x) > percentage_na) {NA} else {sum(x, na.rm = T)}
  } else if (consec == T) {
    i <- x
    i[is.na(i)] <- -9999
    i <- rle(i)
    i <- any(i$lengths[which(i$values==-9999)] >= 5)
    if ((sum(is.na(x))/length(x) > percentage_na) | i == TRUE) {
      NA
    } else {
      sum(x, na.rm = T)
    }
  }
} 

f_min_na <- function(x, percentage_na = 0) {
  if(sum(is.na(x))/length(x) > percentage_na) {NA} else {min(x, na.rm = T)}
} 

f_max_na <- function(x, percentage_na = 0) {
  if(sum(is.na(x))/length(x) > percentage_na) {NA} else {max(x, na.rm = T)}
}

#### Vidējo vērtību normu aprēķins dienai, nedēļai, dekādei, mēnesim un gadam ####

aver_dienas <- function(x, rnd = 1) {
  # rnd      round value - cik cipari jāliek aiz komata, lai noroundotu
  
  x$DATE <- format(as.POSIXct(x$DATE, format = '%Y %m %d%H:%M'),format = '%Y-%m-%d') # No datuma noņemt nost laiku
  x$Value <- x$Value %>% as.numeric()
  
  dien <- x %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina diennakts vidējo
    dplyr::summarise(Value = round(f_mean_na(round(Value, rnd), 
                                             percentage_na = 0.2, consec = T, 
                                             consec_values = 3), rnd))
  return(dien)
}

max_min_dienas <- function(x, rnd = 1, fja) {
  # rnd      round value - cik cipari jāliek aiz komata, lai noroundotu
  # fja      funkcija. Max = 999. Min = -999
  
  x$DATE <- format(as.POSIXct(x$DATE, format = '%Y %m %d%H:%M'),format = '%Y-%m-%d') # No datuma noņemt nost laiku
  x$Value <- x$Value %>% as.numeric()
  
  if(fja==999){
    dien <- x %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina diennakts vidējo
      dplyr::summarise(Value = round(f_max_na(round(Value, rnd)), rnd))
    return(dien)
  } else if(fja==-999) {
    dien <- x %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina diennakts vidējo
      dplyr::summarise(Value = round(f_min_na(round(Value, rnd)), rnd))
    return(dien)
  } else {
    "Aplami ievadīta fja. Jāvada vai nu max vai min"
  }
  
}


# Vidējās vērtības
aver_value <- function(x, time, rnd = 1){
  # x        diennakts dati
  # time     jāliek, dekāde = 2, menesis = 3, gads = 4
  # rnd      cik cipari jāliek aiz komata, lai noroundotu
  
  if(time > 4 | time == 1){
    "Aplami ievadits time. Dekadei time = 2, menesim time = 3, gadam time = 4. 1 ir invalīds"
  } else if(time == 2){ # Aprēķina dekādes vidējo
    dien <- x
    dien <- dien %>% 
      mutate(dekade = ifelse(day(DATE) < 11, 1, ifelse(day(DATE) > 20, 3, 2))) # Pievieno dekāžu kol.
    dien$DATE <- paste0(format(as.POSIXct(dien$DATE, format = '%Y-%m-%d'), format = '%Y-%m'), "-", dien$dekade) # Pārtaisa DATE 
    
    dek <- dien %>% group_by(EG_GH_ID, DATE) %>% 
      dplyr::summarise(Value = round(f_mean_na(round(Value, rnd), percentage_na = 0.2), 
                                     rnd))
    
  } else {
    
    dien <- x
    dien$DATE <- format(as.POSIXct(dien$DATE, format='%Y-%m-%d'), format='%Y-%m') # Pārtaisa DATE 
    
    men <-  dien %>% 
      group_by(EG_GH_ID, DATE) %>%  # Aprēķina menesa vidējo
      mutate(Value = round(Value, rnd)) %>%
      dplyr::summarise(Value = ifelse((sum(is.na(Value)) > 10)| # iztrūkst vaikā kā 10   dienas kopā
                                        (any((rle(Value)$length[which(is.na(rle(Value)))]) > 5)) , # iztrūkst vairāk kā 5 secīgas dienas
                                      NA, mean(Value,na.rm=T))) %>%
      mutate(Value = round(Value, rnd))
    
    if (time == 3) {
      men
    } else {
      men$DATE <- substr(men$DATE, 1, 4) # Pārtaisa DATE 
      
      year <-  men %>% group_by(EG_GH_ID, DATE) %>% # Aprēķina gada vidējo
        dplyr::summarise(Value = ifelse((sum(is.na(Value)) >= 1), # iztrūkst vismaz 1 menesa vertiba
                                        NA , mean(Value,na.rm=T))) %>%
        mutate(Value = round(Value, rnd))
    }
  }
}



# Diennakts (tiek pievienots slīdošais solis) un nedēļas normas aprēķins. 

day_norma <- function(x, rnd = 1){
  
  # Izdzēš visus 29. februārus
  x <- x[!(month(as.POSIXct(x$DATE, format = '%m-%d'))==02 & day(as.POSIXct(x$DATE, format = '%m-%d'))==29),]
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  x <- rbind((x %>% group_by(EG_GH_ID) %>% slice(c(n()-2), n()-1, n())), x, # Sākumā pievieno pēdējās 3 vērtības
             (x %>% group_by(EG_GH_ID) %>% slice(1,2,3))) %>% as.data.frame()      # Beigās pievieno pirmās 3 vērtības
  x <- x %>% arrange(EG_GH_ID) %>% group_by(EG_GH_ID) %>% 
    mutate(ilggad_val2 = round(zoo::rollmean(ilggad_val,k = 7, fill = NA), rnd))     # Aprēķina slīdošo vidējo
  
  x <- x[complete.cases(x), ] # Izdzēš iepriekš pievienotās pirmās un pēd. rindas
  x <- x[,-3] # Izdzēš ne-smooth kolonnu
  names(x)[3] <-"ilggad_val"
  x 
  
}

ned_norma <- function(x){
  
  # Izdzēš visus 29. februārus
  x <- x[!(month(as.POSIXct(x$DATE, format = '%m-%d'))==02&day(as.POSIXct(x$DATE, format = '%m-%d'))==29),]
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  x <- x %>% mutate(DATE_2 = (as.POSIXct(DATE, format = '%m-%d') - days(3)))   # Pārveido datumus tā, lai slīdošais vidējais rēķinas pareizi
  
  x <- rbind(x, (x %>% group_by(EG_GH_ID) %>% slice(1,2,3,4,5,6))) %>% as.data.frame()      # Beigās pievieno pirmās 6 vērtības
  x <- x %>% arrange(EG_GH_ID) %>% group_by(EG_GH_ID) %>% 
    mutate(ilggad_val2 = zoo::rollmean(ilggad_val,k = 7, fill = NA))     # Aprēķina slīdošo vidējo
  
  x <- x[complete.cases(x), ] # Izdzēš iepriekš pievienotās pirmās un pēd. rindas
  x <- x[,-c(2,3)] # Izdzēš dienu vērtības un sākotnējo datumu kolonnu
  names(x)[c(2,3)] <-c("DATE", "ilggad_val")
  x$DATE <- format(as.POSIXct(x$DATE,format='%Y-%m-%d'),format='%m-%d') #Pārveido datumu kolonnu, noņemot gadu 
  x
}



#### Ilggadīgās vid. un summārās vērtības ####

ilggad_value <- function(x, kopa_vert30 = 24, time, type, rnd = 1){ #time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4
  #type jāliek mean = 1, sum = 2
  
  if(time>4){
    "Aplami ievadits time. Dienai time = 1, dekadei time = 2, menesim time = 3, gadam time = 4."
  }  else if((time == 1)|(time == 2)){ # Pārveido datumus vajadzīgajā formā 
    x$DATE <- format(as.POSIXct(x$DATE,format='%Y-%m-%d'),format='%m-%d')
  } else if(time == 3){
    x$DATE <- substr(x$DATE,6,7)
  } else {
    x$DATE <- "Year_norm" 
  }
  
  x <- x %>% group_by(EG_GH_ID,DATE) %>%  # Aprēķina dienas/dekādes/menesa/gada ilggadīgo vidējo. 
    dplyr::summarise(ilggad_val = ifelse((n() >= kopa_vert30), 
                                         round(mean(Value, na.rm=T), rnd), NA))
}


#### Rezultātus pārveido kā tibble ####

norm_tibble <- function(x){
  x <- x %>% group_by(EG_GH_ID, DATE) %>% # Uztaisa glīti pārskatāmu tibble 
    dplyr::mutate(rn = row_number()) %>%
    ungroup() %>% 
    spread(EG_GH_ID, ilggad_val)
  x <- x[,-2]
}

library('dplyr')
library('tidyverse')
library('ggplot2')
library('lubridate')
library('zoo')


#### Sagatavo datus tālākām f-jām ####
data_prep <- function(x, gads_sakums = 1991, gads_beigas = 2020){
  x <- x[,c(1:37)] # Atlasa tikai tās DB kolonnas, kuras nepieciešamas
  x <- x %>% gather(DAY, VALUE, -c(EG_EL_ABBREVIATION, EG_GH_ID, MONTH, TIME, YEAR)) %>% 
    spread(EG_EL_ABBREVIATION, VALUE) 
  names(x)[ncol(x)] <- "Value"  # Pārdēvē kolonnu, lai vienmēr ir Value
  x <- x[!(x$DAY=="REGULAR"|x$DAY=="SUM"),] # Noņem nost vērtības, 
  # kuras DB ir kā REGULAR vai SUM
  
  x <- x %>%  mutate(DAY = DAY %>%  #Dienas vērtībām noņem no priekšas VAL
                       as.character %>%
                       str_sub(4, 5)) %>%
    mutate(DATE = paste(YEAR, MONTH, DAY, TIME) %>% #Pievieno datuma kolonnu, 
             as.POSIXct(format="%Y %m %d%H:%M") %>% # pārveido laiku uz LV laiku
             force_tz(tzone="Etc/GMT-0") %>%
             with_tz(tzone = "Europe/Riga"))
  
  x <- x[(year(x$DATE)>=gads_sakums&year(x$DATE)<=gads_beigas),] # Atlasa vajadzīvos normu gadus
  
  x[,6] <- x[,6] %>% as.numeric() #pārtaisa parametra vērtības par skaitļiem
  x <- x[!is.na(x$DATE),] #Noņem visus NA 
  x <- x[,-c(2:5)] # Noņem liekās kolonnas
}

#### Vidējo vērtību normu aprēķins dienai, nedēļai, dekādei, mēnesim un gadam ####

# Vidējās vērtības
aver_value <- function(x, time){ # time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4
  
  x$DATE <- format(as.POSIXct(x$DATE,format='%Y %m %d%H:%M'),format='%Y-%m-%d') # No datuma noņemt nost laiku
  x$Value <- x$Value %>% as.numeric()
  
  dien <- x %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina diennakts vidējo
    dplyr::summarise(Value = ifelse((sum(is.na(Value)) > 4)| # iztrūkst vaikā kā 4 stundas kopā
                                      (any((rle(Value)$length[which(is.na(rle(Value)))]) > 2)) , # iztrūkst vairāk kā 2 secīgas stundas
                                    NA , mean(Value,na.rm=T)))
  if(time>4){
    "Aplami ievadits time. Dienai time = 1, dekadei time = 2, menesim time = 3, gadam time = 4."
  }  else if(time == 1){
    dien
  } else if(time == 2){ # Aprēķina dekādes vidējo
    
    dien <- dien %>% mutate(dekade = ifelse(day(DATE)<11,1, ifelse(day(DATE)>20,3,2))) # Pievieno dekāžu kol.
    dien$DATE <- paste0(format(as.POSIXct(dien$DATE,format='%Y-%m-%d'),format='%Y-%m'),"-" ,dien$dekade) # Pārtaisa DATE 
    
    dek <- dien %>% group_by(EG_GH_ID, DATE) %>% 
      dplyr::summarise(Value = 
                         ifelse((sum(is.na(Value))/length(Value) > 0.2), # iztrūkst vairāk kā 20% datu
                                NA , mean(Value,na.rm=T)))
    
  } else {
    
    dien$DATE <- format(as.POSIXct(dien$DATE,format='%Y-%m-%d'),format='%Y-%m') # Pārtaisa DATE 
    
    men <-  dien %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina menesa vidējo
      dplyr::summarise(Value = ifelse((sum(is.na(Value)) > 10)| # iztrūkst vaikā kā 10   dienas kopā
                                        (any((rle(Value)$length[which(is.na(rle(Value)))]) > 5)) , # iztrūkst vairāk kā 5 secīgas dienas
                                      NA , mean(Value,na.rm=T)))
    if (time == 3) {
      men
    } else {
      men$DATE <- substr(men$DATE,1,4) # Pārtaisa DATE 
      
      year <-  men %>% group_by(EG_GH_ID, DATE) %>%  # Aprēķina gada vidējo
        dplyr::summarise(Value = ifelse((sum(is.na(Value)) >= 1), # iztrūkst vismaz 1 menesa vertiba
                                        NA , mean(Value,na.rm=T)))
    }
  } 
  
}

# Diennakts (tiek pievienots slīdošais solis) un nedēļas normas aprēķins. 

day_norma <- function(x){
  
  # Izdzēš visus 29. februārus
  x <- x[!(month(as.POSIXct(x$DATE, format = '%m-%d'))==02&day(as.POSIXct(x$DATE, format = '%m-%d'))==29),]
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  x <- rbind((x %>% group_by(EG_GH_ID) %>% slice(c(n()-2), n()-1, n())), x, # Sākumā pievieno pēdējās 3 vērtības
             (x %>% group_by(EG_GH_ID) %>% slice(1,2,3))) %>% as.data.frame()      # Beigās pievieno pirmās 3 vērtības
  x <- x %>% arrange(EG_GH_ID) %>% group_by(EG_GH_ID) %>% 
    mutate(ilggad_val2 = zoo::rollmean(ilggad_val,k = 7, fill = NA))     # Aprēķina slīdošo vidējo
  
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

ilggad_value <- function(x, kopa_vert30 = 24, time, type){ #time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4
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
                                         mean(Value, na.rm=T), NA))
}


#### Rezultātus pārveido kā tibble ####

norm_tibble <- function(x){
  x <- x %>% group_by(EG_GH_ID, DATE) %>% # Uztaisa glīti pārskatāmu tibble 
    dplyr::mutate(rn = row_number()) %>%
    ungroup() %>% 
    spread(EG_GH_ID, ilggad_val)
  x <- x[,-2]
}

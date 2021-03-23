
# No DB līdz gala tibble

#### Pieprasījums no DB ####
con <- source("P:/KMN/Kodi/Clidata_connection.r")
query <- "select *
from CLIDATA.RDATA RDATA
where EG_EL_ABBREVIATION in ('TDRY', 'HTDRY')
and EG_GH_ID in ('RIAI99PA', 'RIAL99MS', 'RIBA99PA', 'RIDM99MS', 'RIDO99MS', 'RIGU99MS', 
'RIJE99PA', 'RIKO99PA', 'RILP99PA', 
'RIME99MS', 'RIPA99PA', 'RIPR99PA', 'RIRU99PA', 'RIRE99MS',
'RISA99PA', 'RISE99MS', 'RISI99PA', 'RIST99PA', 'RIVE99PA', 
'RIZI99PA','RIZO99MS')
and YEAR > 1989
;"

data <- sqlQuery(con[[1]], query,
                 stringsAsFactors = F, dec = ",")[,c(1:37)]

data_sakums <- data_prep(data) # Sagatavo datus

# turpmākajām f-jām nepieciešams, lai dati satur kolonnas - EG_GH_ID, Value un DATE 
# DATE formāts as.POSIXct %Y %m %d%H:%M
data_sakums <- data_sakums[,-2]

vid_normas_apr <- function(x, time){  # time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4, nedēļa = 5
  if(time>5){
    "Aplami ievadits time. Dienai time = 1, dekadei time = 2, menesim time = 3, gadam time = 4, nedēļai time = 5."
  } else if(time == 1){
    diennakts <- aver_value(x, time = 1)
    ilggad_dien <- ilggad_value(diennakts, time = 1, type = 1)
    dien_norm <- day_norma(ilggad_dien)
    dien_norm_tib <- norm_tibble(dien_norm)
  } else if(time == 2){
    dekade <- aver_value(x, time = 2)
    ilggad_dek <- ilggad_value(dekade, time = 2, type = 1)
    ilggad_dek_tib <- norm_tibble(ilggad_dek)
  } else if(time == 3){
    menesis <- aver_value(x, time = 3)
    ilggad_mon <- ilggad_value(menesis, time = 3, type = 1)
    ilggad_mon_tib <- norm_tibble(ilggad_mon)
  } else if(time == 4){
    gads <- aver_value(x, time = 4)
    ilggad_year <- ilggad_value(gads, time = 4, type = 1)
    ilggad_year_tib <- norm_tibble(ilggad_year)
  } else {
    diennakts <- aver_value(x, time = 1)
    ilggad_dien <- ilggad_value(diennakts, time = 1, type = 1)
    ned_norm <- ned_norma(ilggad_dien)
    ned_norm_tib <- norm_tibble(ned_norm)
  }
}

day_normal <- vid_normas_apr(data_sakums, time = 1)
dec_normal <- vid_normas_apr(data_sakums, time = 2)
month_normal <- vid_normas_apr(data_sakums, time = 3)
year_normal <- vid_normas_apr(data_sakums, time = 4)



# write.csv(day_normal, "NORMA_DAY_TDRY.csv")
# write.csv(dec_normal, "NORMA_DEC_TDRY.csv")
# write.csv(month_normal, "NORMA_MON_TDRY.csv")
# write.csv(year_normal, "NORMA_YEAR_TDRY.csv")





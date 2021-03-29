
# Funkcijas --------------------------------------------------------------------

# turpmākajām f-jām nepieciešams, lai dati satur kolonnas - EG_GH_ID, Value un DATE 
source("D:/Viesturs_Zandersons/Klimatiskas_normas/Scripts/Norma_vid_fjas.R", encoding = "UTF-8")

vid_normas_apr <- function(x, time){  # time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4, nedēļa = 5
  if(time > 5){
    "Aplami ievadits time. Dienai time = 1, dekadei time = 2, menesim time = 3, gadam time = 4, nedēļai time = 5."
  } else if(time == 1){
    # diennakts <- aver_value(x, time = 1) # Diennakts dati jau ir aprēķināti
    ilggad_dien <- ilggad_value(x, time = 1, type = 1)
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
    # diennakts <- aver_value(x, time = 1)
    diennakts <- x
    ilggad_dien <- ilggad_value(diennakts, time = 1, type = 1)
    ned_norm <- ned_norma(ilggad_dien)
    ned_norm_tib <- norm_tibble(ned_norm)
  }
}


# Ielādē datus -------------------------------------------------------------

temp_daily <- read_csv("Dati/MeanT_daily.csv")

# Parastās normas
temp_daily <- temp_daily %>%
  mutate(Merijums = ifelse(Merijums == -999.9, NA, Merijums))

# Pārsauc kolonnas, lai normāli rēķinātos normas
temp_daily <- temp_daily %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value"))


uh_day_normal <- vid_normas_apr(temp_daily, time = 1)
uh_dec_normal <- vid_normas_apr(temp_daily, time = 2)
uh_month_normal <- vid_normas_apr(temp_daily, time = 3)
uh_year_normal <- vid_normas_apr(temp_daily, time = 4)



# write.csv(day_normal, "NORMA_DAY_TDRY.csv")
# write.csv(dec_normal, "NORMA_DEC_TDRY.csv")
# write.csv(month_normal, "NORMA_MON_TDRY.csv")
# write.csv(year_normal, "NORMA_YEAR_TDRY.csv")





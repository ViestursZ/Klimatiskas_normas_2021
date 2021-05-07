# Funkcijas --------------------------------------------------------------------

# turpmākajām f-jām nepieciešams, lai dati satur kolonnas - EG_GH_ID, Value un DATE 
source("D:/Viesturs_Zandersons/Klimatiskas_normas/Scripts/Norma_vid_fjas.R", encoding = "UTF-8")

normas_apr <- function(x, time, diennakts = T, fja){  # time jāliek diena = 1, dekāde = 2, menesis = 3, gads = 4, nedēļa = 5
  # fja      funkcija. Max = 999. Min = -999. Vid = 0
  # diennakts     Vai nepieciešams aprēķināt diennakts vērtības? T/F
  
  if (diennakts == T) {
    if(fja==0){
      diennakts <- aver_dienas(x)
    } else if(fja==999) {
      diennakts <- max_min_dienas(x, fja = 999)
    } else if(fja==-999) {
      diennakts <- max_min_dienas(x, fja = -999)
    } else {
      "Aplami ievadīta funkcija. Funkcijai jābūt max = 999   min = -999    vid = 0"
    }
  } else {
    diennakts <- x
  }
  

  if(time>5){
    "Aplami ievadits time. Dienai time = 1, dekadei time = 2, menesim time = 3, gadam time = 4, nedēļai time = 5."
  } else if(time == 1){
    ilggad_dien <- ilggad_value(diennakts, time = 1, type = 1)
    dien_norm <- day_norma(ilggad_dien)
    dien_norm_tib <- norm_tibble(dien_norm)
  } else if(time == 2){
    dekade <- aver_value(diennakts, time = 2)
    ilggad_dek <- ilggad_value(dekade, time = 2, type = 1)
    ilggad_dek_tib <- norm_tibble(ilggad_dek)
  } else if(time == 3){
    menesis <- aver_value(diennakts, time = 3)
    ilggad_mon <- ilggad_value(menesis, time = 3, type = 1)
    ilggad_mon_tib <- norm_tibble(ilggad_mon)
  } else if(time == 4){
    gads <- aver_value(diennakts, time = 4)
    ilggad_year <- ilggad_value(gads, time = 4, type = 1)
    ilggad_year_tib <- norm_tibble(ilggad_year)
  } else {
    ilggad_dien <- ilggad_value(diennakts, time = 1, type = 1)
    ned_norm <- ned_norma(ilggad_dien)
    ned_norm_tib <- norm_tibble(ned_norm)
  }
}


# Ielādē datus -------------------------------------------------------------

temp_daily <- read_csv("Dati/MeanT_daily.csv", col_types = c("D?n"))

# Pārsauc kolonnas, lai normāli rēķinātos normas
temp_daily <- temp_daily %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value")) %>%
  filter(year(DATE) >= 1991 & year(DATE) <= 2020)

uh_day_normal <- normas_apr(temp_daily, time = 1, fja = 0)
uh_dec_normal <- normas_apr(temp_daily, time = 2, fja = 0)
uh_month_normal <- normas_apr(temp_daily, time = 3, fja = 0)
uh_year_normal <- normas_apr(temp_daily, time = 4, fja = 0)


# ACMANT normas
ACMANT_data_trn <- ACMANT_data_t %>% # No format_ACMANT.R skripta
  set_colnames(c("EG_GH_ID", "Value", "DATE"))

ac_day_normal <- vid_normas_apr(ACMANT_data_trn, time = 1)
ac_dec_normal <- vid_normas_apr(ACMANT_data_trn, time = 2)
ac_month_normal <- vid_normas_apr(ACMANT_data_trn, time = 3)
ac_year_normal <- vid_normas_apr(ACMANT_data_trn, time = 4)


# Climatol normas 
climatol_homdata_trn <- climatol_homdata %>%  # No Climatol_analysis.R
  pivot_longer(-Datums, names_to = "EG_GH_ID", values_to = "Value") %>%
  set_colnames(c("DATE", "EG_GH_ID", "Value"))

clim_day_normal <- vid_normas_apr(climatol_homdata_trn, time = 1)
clim_dec_normal <- vid_normas_apr(climatol_homdata_trn, time = 2)
clim_month_normal <- vid_normas_apr(climatol_homdata_trn, time = 3)
clim_year_normal <- vid_normas_apr(climatol_homdata_trn, time = 4)


# write.csv(day_normal, "NORMA_DAY_TDRY.csv")
# write.csv(dec_normal, "NORMA_DEC_TDRY.csv")
# write.csv(month_normal, "NORMA_MON_TDRY.csv")
# write.csv(year_normal, "NORMA_YEAR_TDRY.csv")


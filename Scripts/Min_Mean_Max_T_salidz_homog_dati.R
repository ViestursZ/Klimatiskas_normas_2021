library(tidyverse)
library(plotly)
library(lubridate)
library(RcppRoll)
# Ielādē datus ------------------------------------------------------------

MeanT_homog <- read_csv2("Dati/CLIMATOL_homog_Mean_T_daily_monbrks_noDAGDA_snht20.csv")
# MeanT_homog <- read_csv2("Dati/CLIMATOL_homog_Mean_T.csv")
MeanT_korig <- read_csv("Dati/MeanT_daily_korig_noDAGDA.csv", col_types = "Dcn")

MinT_korig <- read_csv("Dati/Min_T_daily_korig_noDAGDA.csv", col_types = "Dcn")

MaxT_korig <- read_csv("Dati/Max_T_daily_korig_noDAGDA.csv", col_types = "Dcn")

MeanT_homog <- MeanT_homog %>% 
  gather(Stacija, Mean_homog, -Datums)


Clean_data <- read_csv("Dati/Mean_T_dati_clean.csv")

# Test graficiņi --------------------------------------------------------------

MeanT_homog %>% 
  filter(Stacija == "RIGASLU") %>% 
  filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>% 
  filter(month(Datums) %in% c(07, 08)) %>% 
  mutate(yday = yday(Datums)) %>% 
  group_by(yday) %>% 
  summarise(Mean_homog1 = round(mean(round(Mean_homog, 1)), 1)) %>% 
  mutate(rollmeanhom = round(roll_mean(Mean_homog1, n = 7, fill = NA), 1)) %>%
  ggplot() + geom_point(aes(x = yday, y = rollmeanhom)) 


MeanT_homog %>% 
  filter(Stacija == "RIGASLU") %>% 
  filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>% 
  filter(month(Datums) %in% c(07, 08)) %>% 
  mutate(yday = yday(Datums)) %>% 
  group_by(yday) %>% 
  summarise(Mean_homog1 = mean(Mean_homog)) %>% 
  mutate(rollmeanhom = round(roll_mean(Mean_homog1, n = 7, fill = NA), 1)) %>%
  ggplot() + geom_point(aes(x = yday, y = rollmeanhom)) 


MeanT_korig %>% filter(Stacija == "RIGASLU") %>% filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>% 
  mutate(Gads = year(Datums), Menesis = month(Datums)) %>%
  group_by(Gads, Menesis) %>%
  summarise()
  ggplot() + geom_line(aes(Datums, Merijums)) + scale_y_continuous(limits = c(-25, 25))
  
  
MeanT_homog %>%
  filter(Stacija %in% c("RISE99MS", "RIJE99PA", "RIBA99PA","RIDO99MS", "RIGASLU")) %>%
  spread(Stacija, Mean_homog) %>%
  mutate(Vid = (RIJE99PA + RIBA99PA + RIDO99MS + RISE99MS) / 4,
         Riga_minus_vid = RIGASLU - Vid) %>%
  mutate(Gads = year(Datums),
         Menesis = month(Datums)) %>%
  group_by(Gads, Menesis) %>%
  summarise(Riga_vid = mean(Riga_minus_vid)) %>%
  mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-"))) %>%
  ggplot() + geom_line(aes(Datums, Riga_vid))
  
MeanT_homog %>% filter(Stacija == "RIPR99PA") %>% filter(year(Datums) >= 2010 & year(Datums) <= 2020) %% ggplot() + geom_line(aes(Datums, Mean_homog)) + scale_y_continuous(limits = c(-25, 25)) + ggtitle("Homogenizetie")
MeanT_korig %>% filter(Stacija == "RIPR99PA") %>% filter(year(Datums) >= 2010 & year(Datums) <= 2020) %>% ggplot() + geom_line(aes(Datums, Merijums)) + scale_y_continuous(limits = c(-25, 25)) + ggtitle("Nehomogenizetie")

# RIGASLU

MeanT_homog %>% 
  filter(Stacija == "RIGASLU") %>%
  filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>% 
  ggplot() + 
  geom_line(aes(Datums, Mean_homog)) +
  scale_y_continuous(limits = c(-25, 25)) +
  ggtitle("Homogenizetie")

# RIGASLU atšķirības
MeanT_homog %>% 
  filter(Stacija %in% c("RIGASLU", "RISE99MS", "RIJE99PA", "RIBA99PA", "RIDO99MS")) %>% 
  spread(Stacija, Mean_homog) %>%
  mutate(Vid_stacijas = (RIBA99PA + RISE99MS + RIJE99PA + RIDO99MS) / 4) %>%
  mutate(Starpiba = RIGASLU - Vid_stacijas) %>%
  filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>%
  ggplot() + geom_line(aes(Datums, Starpiba)) + 
  geom_smooth(aes(Datums, Starpiba)) +
  scale_y_continuous(limits = c(-6, 6)) +
  ggtitle("RIGASLU Homogenizetie")

MeanT_korig %>% 
  filter(Stacija %in% c("RIGASLU", "RISE99MS", "RIJE99PA", "RIBA99PA", "RIDO99MS")) %>% 
  spread(Stacija, Merijums) %>%
  mutate(Vid_stacijas = (RIBA99PA + RISE99MS + RIJE99PA + RIDO99MS) / 4) %>%
  mutate(Starpiba = RIGASLU - Vid_stacijas) %>%
  filter(year(Datums) >= 1991 & year(Datums) <= 2020) %>%
  ggplot() + geom_line(aes(Datums, Starpiba)) + 
  geom_smooth(aes(Datums, Starpiba)) +
  scale_y_continuous(limits = c(-6, 6)) +
  ggtitle("RIGASLU Korigetie")






# RIPR99PA atšķirības
MeanT_korig %>% 
  filter(Stacija %in% c("RIRU99PA", "RIPR99PA", "RIGU99MS")) %>% 
  spread(Stacija, Merijums) %>%
  mutate(Vid_divas = (RIGU99MS + RIRU99PA) / 2) %>%
  mutate(Starpiba = RIPR99PA - Vid_divas) %>%
  # dplyr::select(-RIPR99PA, -RIZO99MS) %>%
  filter(year(Datums) >= 2000 & year(Datums) <= 2018) %>% 
  ggplot() + geom_line(aes(Datums, Starpiba)) + 
  geom_smooth(aes(Datums, Starpiba)) +
  # scale_y_continuous(limits = c(-25, 25)) + 
  ggtitle("Korigetie")


MeanT_homog %>%
  filter(Stacija %in% c("RIGU99PA"))

Pr_homplot <- MeanT_homog %>%
  filter(Stacija %in% c("RIRU99PA", "RIPR99PA", "RIZO99MS", "RISI99PA", "RIGU99MS")) %>%
  spread(Stacija, Mean_homog) %>%
  mutate(Vid = (RIRU99PA + RIZO99MS + RISI99PA + RIGU99MS) / 4,
         Priek_minus_vid = RIPR99PA - Vid) %>%
  mutate(Gads = year(Datums),
         Menesis = month(Datums)) %>%
  group_by(Gads, Menesis) %>%
  summarise(Priek_vid = mean(Priek_minus_vid)) %>%
  mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-"))) %>%
  ggplot() + geom_line(aes(Datums, Priek_vid))

Pr_korplot <- MeanT_korig %>%
  filter(Stacija %in% c("RIRU99PA", "RIPR99PA", "RIZO99MS", "RISI99PA", "RIGU99MS")) %>%
  spread(Stacija, Merijums) %>%
  filter(year(Datums) >= 2010) %>%
  mutate(Vid = (RIZO99MS + RISI99PA + RIRU99PA + RIGU99MS) / 4,
         Priek_minus_vid = RIPR99PA - Vid) %>%
  # mutate(Gads = year(Datums),
  #        Menesis = month(Datums)) %>%
  # group_by(Gads, Menesis) %>%
  # summarise(Priek_vid = mean(Priek_minus_vid)) %>%
  # mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-"))) %>%
  ggplot() + geom_line(aes(Datums, Priek_minus_vid)) + geom_smooth(aes(Datums, Priek_minus_vid))


Pr_homplot
Pr_korplot

t1 <- MeanT_korig %>% filter(Stacija == "RIGASLU") %>%  filter(year(Datums) >= 1991 & year(Datums) <= 2020)
t2 <- MeanT_homog %>% filter(Stacija == "RIGASLU") %>%  filter(year(Datums) >= 1991 & year(Datums) <= 2020)

t3 <- inner_join(t1, t2)

t3plot <- t3 %>%
  mutate(Starpiba = Mean_homog - Merijums) %>%
  mutate(Gads = year(Datums), Menesis = month(Datums)) %>%
  # group_by(Gads, Menesis) %>%
  # summarise(Starpiba = mean(Starpiba, na.rm = T)) %>%
  # mutate(Datums = ymd(str_c(Gads, Menesis, "01", sep = "-"))) %>%
  # filter(month(Datums)) %>%
  mutate(Menesis = month(Datums)) %>%
  ggplot() + geom_point(aes(Datums, Starpiba, col = as.factor(Menesis))) +
  scale_y_continuous(limits = c(-1.5, 1.5)) 

ggplotly(t3plot)


# Pārkārto datus ----------------------------------------------------------

# MeanT_homog <- MeanT_homog %>% 
#   gather(Stacija, Mean_homog, -Datums)

# MeanT_korig <- MeanT_korig %>%
#   rename(Mean_korig = Merijums)
  
# MinT_homog <- MinT_homog %>%
#   gather(Stacija, Min_homog, -Datums)

MinT_korig <- MinT_korig %>%
  rename(Min_korig = Merijums)

MaxT_korig <- MaxT_korig %>%
  rename(Max_korig = Merijums)

Temp_dati_all <- inner_join(MeanT_homog, MinT_korig) %>%
  inner_join(MaxT_korig)


# Starpības
Temp_dati_starpibas <- Temp_dati_all %>%
  transmute(Datums = Datums,
            Stacija = Stacija,
            Homog_minus_Min_korig = Mean_homog - Min_korig,
            Homog_minus_Max_korig = Mean_homog - Max_korig)

t_hist <- Temp_dati_starpibas %>%
  gather(Parametrs, Starpiba, -Datums, -Stacija) %>%
  ggplot() + 
  geom_histogram(aes(Starpiba), binwidth = 0.5, fill = "gray", col = "black") +
  facet_wrap(~Parametrs)
ggplotly(t_hist)  



# Cik ir starpības
Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0) %>%
  summarise(Skaits = n())

Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0) %>%
  summarise(Skaits = n())


# Kur ir starpības?
Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0) %>%
  group_by(Stacija) %>%
  summarise(Skaits = n()) %>% View()


Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0) %>%
  group_by(Stacija) %>%
  summarise(Skaits = n()) %>% View()

# Visvairāk starpību ir Rīgā

# Cik lielas ir Rīgas starpības?
Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0 & Stacija == "RIGASLU") %>%
  ggplot() + 
  geom_histogram(aes(Homog_minus_Min_korig), binwidth = 0.1, fill = "gray", col = "black")

Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0 & Stacija == "RIGASLU") %>%
  ggplot() + 
  geom_histogram(aes(Homog_minus_Max_korig), binwidth = 0.1, fill = "gray", col = "black")

# Cik lielas ir Rucavas starpības?
Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0 & Stacija == "RUCAVA") %>%
  ggplot() + 
  geom_histogram(aes(Homog_minus_Min_korig), binwidth = 0.1, fill = "gray", col = "black")

Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0 & Stacija == "RUCAVA") %>%
  ggplot() + 
  geom_histogram(aes(Homog_minus_Max_korig), binwidth = 0.1, fill = "gray", col = "black")



# Kad ir Rīgas starpības..?
rigminstarpplot <- Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0 & Stacija == "RIGASLU") %>%
  ggplot(aes(Datums, Homog_minus_Min_korig)) + 
  geom_point()

ggplotly(rigminstarpplot)  

rigmaxstarpplot <- Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0 & Stacija == "RIGASLU") %>%
  # summarise(Skaits = n()) %>%
  ggplot(aes(Datums, Homog_minus_Max_korig)) + 
  geom_point()

ggplotly(rigmaxstarpplot)  


# Kad ir Rucavas starpibas?
rucavaminstarpplot <- Temp_dati_starpibas %>%
  filter(Homog_minus_Min_korig < 0 & Stacija == "RUCAVA") %>%
  ggplot(aes(Datums, Homog_minus_Min_korig)) + 
  geom_point()

ggplotly(rucavaminstarpplot)  

rucavamaxstarpplot <- Temp_dati_starpibas %>%
  filter(Homog_minus_Max_korig > 0 & Stacija == "RUCAVA") %>%
  ggplot(aes(Datums, Homog_minus_Max_korig)) + 
  geom_point()

ggplotly(rucavamaxstarpplot)  

Temp_dati_all %>%
  filter(Stacija == "RIGASLU" & Datums == ymd("1960-01-12"))

# Aprēķina gadu vērtības salīdzinājumam --------------------------------------
MeanT_korig %>%
  filter(year(Datums) >= 1981 & year(Datums) <= 2020) %>%
  filter(Stacija == "RIGASLU") %>%
  mutate(Gads = year(Datums), Menesis = month(Datums)) %>%
  group_by(Gads, Menesis) %>%
  summarise(Merijums = round(f_mean_na(round(Merijums, 1), percentage_na = 0.2, consec = T), 1)) %>%
  spread(Menesis, Merijums) %>%
  write_excel_csv2("Dati/Korig_LU_dati_pa_gadiem.csv")


MeanT_homog %>%
  filter(year(Datums) >= 1981 & year(Datums) <= 2020) %>%
  filter(Stacija == "RIGASLU") %>%
  mutate(Gads = year(Datums), Menesis = month(Datums)) %>%
  group_by(Gads, Menesis) %>%
  summarise(Merijums = round(f_mean_na(round(Mean_homog, 1), percentage_na = 0.2, consec = T), 1)) %>%
  spread(Menesis, Merijums) %>%
  write_excel_csv2("Dati/Homog_LU_dati_pa_gadiem.csv")


MeanT_korig %>%
  filter(year(Datums) == 1989) %>%
  filter(Stacija == "RIGASLU") %>% View()



# clean data --------------------------------------------------------------

Clean_data %>%
  filter(Stacija == "RIGASLU") %>%
  filter(year(Datums_laiks) == 2006 & month(Datums_laiks) %in% c(03, 04, 05)) %>% View()

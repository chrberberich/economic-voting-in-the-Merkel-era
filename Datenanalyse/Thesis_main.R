##############################################
################Bachelorarbeit################
##############################################

# Name: Christopher Berberich
# Matrikelnummer: ***
# E-Mail: ***
# Adresse: ***
# Abgabetermin: ***
# Erstgutachter: ***


# Folgende Libraries werden geladen. Falls nicht vorhanden installieren.
library(haven)
library(tidyverse)
library(gtsummary)
library(stargazer)
library(margins)
library(DescTools)
library(car)
library(gt)
library(patchwork)
library(knitr)
library(kableExtra)
library(caret)
library(srvyr)
library(survey)
library(plotly)
library(pandoc)

# !!!Achtung!!! Working directory setzen und gegebenenfalls in read_dta() die
# Pfade anpassen!
setwd("B:/R Projekte/Bachelorarbeit/Datenanalyse")


GLES2009 <- read_dta("GLES 2009-2021 Nachwahl/2009/ZA5301_de_v4-0-2.dta") # nolint
# GLES (2019a). Nachwahl-Querschnitt (GLES 2009). GESIS Datenarchiv, Köln. ZA5301 Datenfile Version 4.0.2, https://doi.org/10.4232/1.13229.
GLES2013 <- read_dta("GLES 2009-2021 Nachwahl/2013/ZA5701_de_v3-0-1.dta")
# GLES (2019b). Nachwahl-Querschnitt (GLES 2013). GESIS Datenarchiv, Köln. ZA5701 Datenfile Version 3.0.1, https://doi.org/10.4232/1.13232.
GLES2017 <- read_dta("GLES 2009-2021 Nachwahl/2017/ZA6801_de_v4-0-1.dta")
# GLES (2019c). Nachwahl-Querschnitt (GLES 2017). GESIS Datenarchiv, Köln. ZA6801 Datenfile Version 4.0.1, https://doi.org/10.4232/1.13235.
GLES2021 <- read_dta("GLES 2009-2021 Nachwahl/2021/ZA7701_v2-1-0.dta")
# GLES (2023). GLES Querschnitt 2021, Nachwahl. GESIS, Köln. ZA7701 Datenfile Version 2.1.0, https://doi.org/10.4232/1.14169.



U <- read.csv("offizielle Statistik/arbeitslosenquote_deutschland_originalwert.csv", fileEncoding = "ISO-8859-1", 
              header = FALSE, skip = 1, sep = ";", dec = ",", na.strings = "."
              )
# Ofizielle Arbeitslosenstatistik (Arbeitslosenquote) der Bundesagentur für Arbeit
# Download unter: https://www.destatis.de/DE/Service/OpenData/konjunkturindikatoren-csv.html#446700

I <- read.csv("offizielle Statistik/verbraucherpreisindex_gesamtindex_bv41.csv",
              skip = 1, header = TRUE, sep = ";", dec = ",", na.strings = " "
              )
# Amtliche Statistik, Verbraucherpreisindex, Gesamtindex
# Download unter https://www.destatis.de/DE/Service/OpenData/konjunkturindikatoren-csv.html#446738

GDP <- read.csv("offizielle Statistik/GDP 81000-0001_$F.csv", ,fileEncoding = "ISO-8859-1",
                skip = 5, header = FALSE, sep = ";", dec = ","
                )
# GDP - GENESIS-Tabelle: 81000-0001
# VGR des Bundes - Bruttowertschöpfung, Bruttoinlandsprodukt
# (nominal/preisbereinigt): Deutschland, Jahre 2004-2023
# !Achtung: Bei download unbedingt "Leerspalten ausblenden" anwählen
# Download: https://www-genesis.destatis.de/genesis//online?operation=table&code=81000-0001&bypass=true&levelindex=0&levelid=1722163025267#abreadcrumb





#############################################################
################### Datenaufbereitung #######################
#############################################################


## Datenaufbereitung U - Arbeitslosenquote, 

colnames(U) <- c("Datum", "Arbeitslosenquote", "Maenner", "Frauen", "u20", "abhängigbeschäftigte")
U <- U[-1, ]
U$Arbeitslosenquote <- gsub(",", ".", U$Arbeitslosenquote)
U$Arbeitslosenquote <- as.numeric(U$Arbeitslosenquote)
U <- select(U, Datum, Arbeitslosenquote)
U$Datum <- as.Date(U$Datum, format = "%d/%m/%Y")


## Berechnung der Variable delta_U
USep <- filter(U, format(Datum, "%m-%d") == "09-01")  


Unemployment09 <- USep %>% 
  filter(format(Datum, "%Y") == "2009") %>% 
  select(Arbeitslosenquote)

Unemployment13 <- USep %>% 
  filter(format(Datum, "%Y") == "2013") %>% 
  select(Arbeitslosenquote)

Unemployment17 <- USep %>% 
  filter(format(Datum, "%Y") == "2017") %>% 
  select(Arbeitslosenquote)

Unemployment21 <- USep %>% 
  filter(format(Datum, "%Y") == "2021") %>% 
  select(Arbeitslosenquote)

U09 <- filter(USep,format(Datum, "%Y") == "2009") - filter(USep,format(Datum, "%Y") == "2005")
U09 <- U09[1, 2]
U13 <- filter(USep,format(Datum, "%Y") == "2013") - filter(USep,format(Datum, "%Y") == "2009")
U13 <- U13[1, 2]
U17 <- filter(USep,format(Datum, "%Y") == "2017") - filter(USep,format(Datum, "%Y") == "2013")
U17 <- U17[1, 2]
U21 <- filter(USep,format(Datum, "%Y") == "2021") - filter(USep,format(Datum, "%Y") == "2017")
U21 <- U21[1, 2]


## Datenaufbereitung I - Verbraucherpreisindex, Gesamtindex
colnames(I)[2:3] <- c("Total",  "Inflation")
I <- select(I,Datum, Inflation)
I$Datum <- as.Date(I$Datum, format = "%d/%m/%Y")
I <- subset(I, Datum >= "2005-03-01")  

ISep <- filter(I, format(Datum,"%m-%d") == "09-01")


## Berechnung der Variable delta_I


I09 <- ISep %>% 
  filter(format(Datum, "%Y") == "2009") %>% 
  select(Inflation)

I13 <- ISep %>% 
  filter(format(Datum, "%Y") == "2013") %>% 
  select(Inflation)

I17 <- ISep %>% 
  filter(format(Datum, "%Y") == "2017") %>% 
  select(Inflation)

I21 <- ISep %>% 
  filter(format(Datum, "%Y") == "2021") %>% 
  select(Inflation)

## Datenaufbereitung GDP - GENESIS-Tabelle: 81000-0001
# VGR des Bundes - Bruttowertschöpfung, Bruttoinlandsprodukt
# (nominal/preisbereinigt): Deutschland, Jahre
# Volkswirtschaftliche Gesamtrechnungen des Bundes

GDP <- as.data.frame(t(GDP))
GDP <- select(GDP, V1, V6, V7, V13, V14)





colnames(GDP) <- c("Datum", "GDP", "Wachstumsrate", "Kettenpreisindex", "WachstumsrateKette") 
GDP <- GDP [-c(1:3), ]
GDP$Datum <- as.Date(GDP$Datum, format = "%Y")
GDP$GDP <- as.numeric(GDP$GDP)
GDP$Wachstumsrate <- as.numeric(GDP$Wachstumsrate)
GDP$WachstumsrateKette <- as.numeric(GDP$WachstumsrateKette)
GDP$Kettenpreisindex <- as.numeric(GDP$Kettenpreisindex)





GDP09 <- GDP %>% 
  filter(format(Datum, "%Y") == "2009") %>% 
  select(Kettenpreisindex)

GDP13 <- GDP %>% 
  filter(format(Datum, "%Y") == "2013") %>% 
  select(Kettenpreisindex)

GDP17 <- GDP %>% 
  filter(format(Datum, "%Y") == "2017") %>% 
  select(Kettenpreisindex)

GDP21 <- GDP %>% 
  filter(format(Datum, "%Y") == "2021") %>% 
  select(Kettenpreisindex)

# Makrodatensatz
Result <- c(33.8, 41.5, 32.9, 24.1)
GDPgrowth <- c(GDP09, GDP13, GDP17, GDP21)
Inflation <- c(I09, I13, I17, I21)
Unemployment <- c(U09, U13, U17, U21)
MacroData <- data.frame(Result) 
rownames(MacroData) <- c("2009", "2013", "2017", "2021")
MacroData$GDPgrowth <- as.numeric(GDPgrowth)
MacroData$Inflation <- as.numeric(Inflation)
MacroData$Unemployment <- as.numeric(Unemployment)




######### Variablenauswahl für GLES 2009 ########### 
GLES09 <- GLES2009 %>%  
  select(q40b, q36, q51b, q63, q59, q1, q2, wei_ow, d176, d183, d179, q166a, 
         d241a, d241b, d173a, d173b, d173c, d173d, d173e, d172) %>% 
  rename(Vote = q40b,
         NonVoter = q36,
         Macro = q63,
         Micro = q59,
         Gender = q1,
         Age = q2,
         East = wei_ow,
         Education = d176,
         Partyidentification = q166a,
         IncomeA = d241a,
         IncomeB = d241b,
         cohabitA = d173a,
         cohabitB = d173b,
         cohabitC = d173c,
         cohabitD = d173d,
         cohabitE = d173e,
         Cohabitors = d172)
         
######### Datenbereinigung für GLES 2009 ########### 
G09 <- GLES09 %>%
  mutate(W = East) %>% 
  mutate(Year = 2009) %>% 
  mutate(GDPgrowth = as.numeric(GDP09)) %>% 
  mutate(Inflation = as.numeric(I09)) %>% 
  mutate(UnemploymentRate = as.numeric(Unemployment09)) %>% 
#  mutate(Vote = ifelse(NonVoter == 2, 0, Vote)) %>% 
  subset(Vote <= 998) %>% 
  mutate(Union = as.factor(ifelse(Vote == 1, 1, 0)),
         SPD = as.factor(ifelse(Vote == 4, 1, 0)),
         FDP = as.factor(ifelse(Vote == 5, 1, 0)),
         GRUENE = as.factor(ifelse(Vote == 6, 1, 0)),
         LINKE = as.factor(ifelse(Vote == 7, 1, 0)),
         AFD = 0,
         Goverment = as.factor(ifelse(Vote == 1, 1, 0)
                                | ifelse(Vote == 4, 1, 0))) %>%
  mutate(Macro = case_when(Macro %in% 1:2 ~ 1,
                           Macro == 3 ~ 0,
                           Macro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  
  mutate(Micro = case_when(Micro %in% 1:2 ~ 1,
                           Micro == 3 ~ 0,
                           Micro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Gender = factor(ifelse(Gender == 2, 0, 1),
                         levels = c(0, 1), 
                         labels = c("Frau", "Mann"))) %>% # Frau = 0, Mann = 1
  mutate(Age = ifelse(Age < 1000, Age, NA)) %>% 
  mutate(East = ifelse(East < 1, 1, 0)) %>%  # Westdeutschland = 0, ostdeutschland = 1
  mutate(Education = case_when(Education == 3 ~ 1,
                               Education == 4 ~ 2,
                               Education == 5 ~ 3,
                               TRUE ~ 0)) %>%  # Mittlere Reife = 1, Fachabi = 2, Abi = 3, Rest 0
  mutate(IncomeA = ifelse(IncomeA < 99990, IncomeA, NA)) %>% 
  mutate(IncomeB = ifelse(IncomeB < 90, IncomeB, NA)) %>%
  filter(!(is.na(IncomeA) & is.na(IncomeB))) %>%
  subset(Partyidentification < 999) %>% 
  mutate(Union_Identification = as.integer(ifelse(Partyidentification == 2 | Partyidentification == 3, 1, 0)),
         SPD_Identification = as.integer(ifelse(Partyidentification == 4, 1, 0)),
         FDP_Identification = as.integer(ifelse(Partyidentification == 5, 1, 0)),
         IdentityGRUENE = as.integer(ifelse(Partyidentification == 6, 1, 0)),
         IdentityLINKE = as.integer(ifelse(Partyidentification == 7, 1, 0)),
         IdentityAFD = as.integer(ifelse(Partyidentification == 322, 1, 0)),
         IdentityGoverment = as.integer(ifelse(Partyidentification == 1, 1, 0) 
                                        | ifelse(Partyidentification == 4, 1, 0))) 
  
G09 <- G09 %>%
  mutate(H = 
    ifelse(cohabitA %in% 1:14, 0.3, ifelse(cohabitA %in% 14:103, 0.5, 0)) +
    ifelse(cohabitB %in% 1:14, 0.3, ifelse(cohabitB %in% 14:103, 0.5, 0)) +
    ifelse(cohabitC %in% 1:14, 0.3, ifelse(cohabitC %in% 14:103, 0.5, 0)) +
    ifelse(cohabitD %in% 1:14, 0.3, ifelse(cohabitD %in% 14:103, 0.5, 0)) +
    ifelse(cohabitE %in% 1:14, 0.3, ifelse(cohabitE %in% 14:103, 0.5, 0)) 
    + 1
  ) %>%
  mutate(IncomeA = IncomeA/H)%>%
  mutate(IncomeB = case_when(
    IncomeB == 1 ~ 375,
    IncomeB == 2 ~ 700,
    IncomeB == 3 ~ 1100,
    IncomeB == 4 ~ 1400,
    IncomeB == 5 ~ 1750,
    IncomeB == 6 ~ 2300,
    IncomeB == 7 ~ 3050,
    IncomeB == 8 ~ 4000,
    IncomeB == 9 ~ 5250,
    IncomeB == 10 ~ 7000,
    IncomeB == 11 ~ 12000
    )/H)%>%
  mutate(Income = coalesce(IncomeA, IncomeB)) %>%
  mutate(Income = Income/100) %>%
  mutate(Income = round(Income, digits = 0))

  
G09 <- select(G09, Year, GDPgrowth, Inflation, UnemploymentRate, Union, SPD, FDP, Macro, Micro, 
              Gender, Age, Education, East, 
              Union_Identification, SPD_Identification, FDP_Identification, Income, cohabitA, cohabitB, cohabitC, cohabitD, cohabitE, W) %>%
              na.omit()   
       
             

  


######### Variablenauswahl für GLES 2013 ########### 
GLES13 <- GLES2013 %>%  
  select(q19bb, q15, q27bb, q10, q64, q1, q2c, ostwest, q169, q173, q189, q159b, q229,
         q166a, q166b, q166c, q166d, q166e, w_ow, q165) %>% 
  rename(Vote = q19bb,
         NonVoter = q15,
         Macro = q10,
         Micro = q64,
         Gender = q1,
         Age = q2c,
         East = ostwest,
         Education = q169,
         Partyidentification = q159b,
         Income = q229,
         cohabitA = q166a,
         cohabitB = q166b,
         cohabitC = q166c,
         cohabitD = q166d,
         cohabitE = q166e,
         Cohabitors = q165)

######### Datenbereinigung für GLES 2013 ########### 
G13 <- GLES13 %>% 
  mutate(W = w_ow) %>% 
  mutate(Year = 2013) %>% 
  mutate(GDPgrowth = as.numeric(GDP13)) %>% 
  mutate(Inflation = as.numeric(I13)) %>% 
  mutate(UnemploymentRate = as.numeric(Unemployment13)) %>%
#  mutate(Vote = ifelse(NonVoter == 2, 0, Vote)) %>% 
  subset(Vote >= 0) %>% 
  mutate(Union = as.factor(ifelse(Vote == 1, 1, 0)),
         SPD = as.factor(ifelse(Vote == 4, 1, 0)),
         FDP = as.factor(ifelse(Vote == 5, 1, 0)),
         GRUENE = as.factor(ifelse(Vote == 6, 1, 0)),
         LINKE = as.factor(ifelse(Vote == 7, 1, 0)),
         AFD = as.factor(ifelse(Vote == 322, 1, 0)),
         Goverment = as.factor(ifelse(Vote == 1, 1, 0)
                                | ifelse(Vote == 5, 1, 0))) %>%
  mutate(Macro = case_when(Macro == 1 ~ 1,
                           Macro == 2 ~ 0,
                           Macro == 3 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Micro = case_when(Micro %in% 1:2 ~ 1,
                           Micro == 3 ~ 0,
                           Micro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Gender = factor(ifelse(Gender == 2, 0, 1),
                         levels = c(0, 1), 
                         labels = c("Frau", "Mann"))) %>%  # Frau = 0, Mann = 1
  mutate(Age = 2013 - ifelse(Age > 0, Age, NA)) %>% 
  mutate(East = ifelse(East == 1, 1, 0)) %>%  # Westdeutschland = 0, ostdeutschland = 1
  mutate(Education = case_when(Education == 3 ~ 1,
                               Education == 4 ~ 2,
                               Education == 5 ~ 3,
                               TRUE ~ 0)) %>%  # Mittlere Reife = 1, Fachabi = 2, Abi = 3, Rest 0
  mutate(Income = ifelse(Income > 0, Income, NA)) %>% na.omit() %>% 
  subset(Partyidentification > -99) %>% 
  mutate(Union_Identification = as.integer(ifelse(Partyidentification == 1 | 
                                              Partyidentification == 2 | Partyidentification == 3, 1, 0)),
         SPD_Identification = as.integer(ifelse(Partyidentification == 4, 1, 0)),
         FDP_Identification = as.integer(ifelse(Partyidentification == 5, 1, 0)),
         IdentityGRUENE = as.integer(ifelse(Partyidentification == 6, 1, 0)),
         IdentityLINKE = as.integer(ifelse(Partyidentification == 7, 1, 0)),
         IdentityAFD = as.integer(ifelse(Partyidentification == 322, 1, 0)),
         IdentityGoverment = as.integer(ifelse(Partyidentification == 1, 1, 0) 
                                        | ifelse(Partyidentification == 5, 1, 0))) 



G13 <- select(G13, Year, GDPgrowth, Inflation, UnemploymentRate, Union, SPD, FDP, Macro, Micro, 
              Gender, Age, East, Education,
              Union_Identification, SPD_Identification, FDP_Identification, Income, cohabitA, cohabitB, cohabitC, cohabitD, cohabitE, W) %>%  
              na.omit() 




     
######### Variablenauswahl für GLES 2017 ########### 
GLES17 <- GLES2017 %>%  
  select(q19bb, q17, q25bb, q14, q56, q1, q2c, q135, q139, q137, q125a, q192, 
         q132a, q132b, q132c, q132d, q132e, ostwest, w_ow, q131) %>% 
  rename(Vote = q19bb,
         NonVoter = q17,
         Macro = q14,
         Micro = q56,
         Gender = q1,
         Age = q2c,
         East = ostwest,
         Education = q135,
         Partyidentification = q125a,
         Income = q192,
         cohabitA = q132a,
         cohabitB = q132b,
         cohabitC = q132c,
         cohabitD = q132d,
         cohabitE = q132e,
         Cohabitors = q131)

######### Datenbereinigung für GLES 2017 ########### 
G17 <- GLES17 %>%
  mutate(W = w_ow) %>% 
  mutate(Year = 2017) %>% 
  mutate(GDPgrowth = as.numeric(GDP17)) %>% 
  mutate(Inflation = as.numeric(I17)) %>% 
  mutate(UnemploymentRate = as.numeric(Unemployment17)) %>%
#  mutate(Vote = ifelse(NonVoter == 2, 0, Vote)) %>%
  subset(Vote >= 0) %>% 
  mutate(Union = as.factor(ifelse(Vote == 1, 1, 0)),
         SPD = as.factor(ifelse(Vote == 4, 1, 0)),
         FDP = as.factor(ifelse(Vote == 5, 1, 0)),
         GRUENE = as.factor(ifelse(Vote == 6, 1, 0)),
         LINKE = as.factor(ifelse(Vote == 7, 1, 0)),
         AFD = as.factor(ifelse(Vote == 322, 1, 0)),
         Goverment = as.factor(ifelse(Vote == 1, 1, 0)
                                | ifelse(Vote == 4, 1, 0))) %>% 
  mutate(Macro = case_when(Macro %in% 1:2 ~ 1,
                           Macro == 3 ~ 0,
                           Macro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Micro = case_when(Micro %in% 1:2 ~ 1,
                           Micro == 3 ~ 0,
                           Micro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Gender = factor(ifelse(Gender == 2, 0, 1),
                         levels = c(0, 1), 
                         labels = c("Frau", "Mann"))) %>%  # Frau = 0, Mann = 1
  mutate(Age = 2017 - ifelse(Age > 0, Age, NA)) %>% 
  mutate(Age = ifelse(Age >= 18, Age, NA)) %>%
  mutate(East = ifelse(East == 0, 1, 0)) %>%  # Westdeutschland = 0, ostdeutschland = 1
  mutate(Education = case_when(Education == 3 ~ 1,
                               Education == 4 ~ 2,
                               Education == 5 ~ 3,
                               TRUE ~ 0)) %>%  # Mittlere Reife = 1, Fachabi = 2, Abi = 3, Rest 0
  mutate(Income = ifelse(Income > 0, Income, NA)) %>% na.omit() %>% 
  subset(Partyidentification > -99) %>%
  mutate(Union_Identification = as.integer(ifelse(Partyidentification == 1 
                                            | Partyidentification == 2 | Partyidentification == 3, 1, 0)),
         SPD_Identification = as.integer(ifelse(Partyidentification == 4, 1, 0)),
         FDP_Identification = as.integer(ifelse(Partyidentification == 5, 1, 0)),
         IdentityGRUENE = as.integer(ifelse(Partyidentification == 6, 1, 0)),
         IdentityLINKE = as.integer(ifelse(Partyidentification == 7, 1, 0)),
         IdentityAFD = as.integer(ifelse(Partyidentification == 322, 1, 0)),
         IdentityGoverment = as.integer(ifelse(Partyidentification == 1, 1, 0) 
                                        | ifelse(Partyidentification == 4, 1, 0))) 


#Auswahl der Spalten
G17 <- select(G17, Year, GDPgrowth, Inflation, UnemploymentRate, Union, SPD, FDP, Macro, Micro, 
              Gender, Age, East, Education,  
              Union_Identification, SPD_Identification, FDP_Identification, Income, cohabitA, cohabitB, cohabitC, cohabitD, cohabitE, W) %>%  
              na.omit()


######### Variablenauswahl für GLES 2021 ########### 
GLES21 <- GLES2021 %>%  
  select(q114ba, q111, q34ba, q24, q14, d1, d2a, ostwest, d7, d11, d9, q122a, d63,
         d4a, d4b, d4c, d4d, d4e, w_ow, d3) %>% 
  rename(Gewicht = ostwest,
         Vote = q114ba,
         NonVoter = q111,
         Macro = q24,
         Micro = q14,
         Gender = d1,
         Age = d2a,
         East = ostwest,
         Education = d7,
         Partyidentification = q122a,
         Income = d63,
         cohabitA = d4a,
         cohabitB = d4b,
         cohabitC = d4c,
         cohabitD = d4d,
         cohabitE = d4e,
         Cohabitors = d3)




######### Datenbereinigung für GLES 2021 ########### 
G21 <- GLES21 %>%
  mutate(W = w_ow) %>% 
  mutate(Year = 2021) %>%
  mutate(GDPgrowth = as.numeric(GDP21)) %>%
  mutate(Inflation = as.numeric(I21)) %>% 
  mutate(UnemploymentRate = as.numeric(Unemployment21)) %>%
#  mutate(Vote = ifelse(NonVoter == 2, 0, Vote)) %>%
  subset(Vote >= 0) %>% 
  mutate(Union = as.factor(ifelse(Vote == 1, 1, 0)),
         SPD = as.factor(ifelse(Vote == 4, 1, 0)),
         FDP = as.factor(ifelse(Vote == 5, 1, 0)),
         GRUENE = as.factor(ifelse(Vote == 6, 1, 0)),
         LINKE = as.factor(ifelse(Vote == 7, 1, 0)),
         AFD = as.factor(ifelse(Vote == 322, 1, 0)),
         Goverment = as.factor(ifelse(Vote == 1, 1, 0) 
                           | ifelse(Vote == 4, 1, 0))) %>% 
  mutate(Macro = case_when(Macro %in% 1:2 ~ 1,
                           Macro == 3 ~ 0,
                           Macro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Micro = case_when(Micro %in% 1:2 ~ 1,
                           Micro == 3 ~ 0,
                           Micro %in% 4:5 ~ -1,
                           TRUE ~ NA)) %>% 
  mutate(Gender = factor(ifelse(Gender == 2, 0, 1),
                         levels = c(0, 1), 
                         labels = c("Frau", "Mann"))) %>% # Frau = 0, Mann = 1
  mutate(Age = as.numeric(ifelse(Age == "1931 oder frueher", 1931, 
                                 ifelse(Age == "-99 keine Angabe", NA, Age)))) %>% #Korrektur von Characterstrings
  mutate(Age = 2021 - ifelse(Age > 0, Age, NA)) %>% 
  mutate(East = ifelse(East == 0, 1, 0)) %>%   # Westdeutschland = 0, ostdeutschland = 1
  mutate(Education = case_when(Education == 3 ~ 1,
                               Education == 4 ~ 2,
                               Education == 5 ~ 3,
                               TRUE ~ 0)) %>%  # Mittlere Reife = 1, Fachabi = 2, Abi = 3, Rest 0 
  mutate(Income = ifelse(Income > 0, Income, NA)) %>% na.omit() %>% 
  subset(Partyidentification > -99 & Partyidentification != -97) %>%
  mutate(Union_Identification = as.integer(ifelse(Partyidentification == 1 | 
                                              Partyidentification == 2 | Partyidentification == 3, 1, 0)),
         SPD_Identification = as.integer(ifelse(Partyidentification == 4, 1, 0)),
         FDP_Identification = as.integer(ifelse(Partyidentification == 5, 1, 0)),
         IdentityGRUENE = as.integer(ifelse(Partyidentification == 6, 1, 0)),
         IdentityLINKE = as.integer(ifelse(Partyidentification == 7, 1, 0)),
         IdentityAFD = as.integer(ifelse(Partyidentification == 322, 1, 0)),
         IdentityGoverment = as.integer(ifelse(Partyidentification == 1, 1, 0) 
                                | ifelse(Partyidentification == 4, 1, 0))) 


G21 <- select(G21, Year, GDPgrowth, Inflation, UnemploymentRate, Union, SPD, FDP, Macro, Micro, 
              Gender, Age, East, Education,
              Union_Identification, SPD_Identification, FDP_Identification, Income, cohabitA, cohabitB, cohabitC, cohabitD, cohabitE, W) %>%  
              na.omit() 


############# Faktorisierung Macro, Micro und Education###########
G09$Macro <- factor(G09$Macro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))
G09$Micro <- factor(G09$Micro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))

G13$Macro <- factor(G13$Macro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))
G13$Micro <- factor(G13$Micro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))

G17$Macro <- factor(G17$Macro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))
G17$Micro <- factor(G17$Micro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))

G21$Macro <- factor(G21$Macro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))
G21$Micro <- factor(G21$Micro, levels = c("0", "-1", "1"), labels = c("_remain", "_decrease", "_increase"))



G09$Education <- factor(G09$Education, levels = c("0", "1", "2", "3"), labels = c("Kein Abschluss oder Hauotschulabschluss", "Realschulabschluss", "Fachabitur", "Abitur"))
G13$Education <- factor(G13$Education, levels = c("0", "1", "2", "3"), labels = c("Kein Abschluss oder Hauotschulabschluss", "Realschulabschluss", "Fachabitur", "Abitur"))
G17$Education <- factor(G17$Education, levels = c("0", "1", "2", "3"), labels = c("Kein Abschluss oder Hauotschulabschluss", "Realschulabschluss", "Fachabitur", "Abitur"))
G21$Education <- factor(G21$Education, levels = c("0", "1", "2", "3"), labels = c("Kein Abschluss oder Hauotschulabschluss", "Realschulabschluss", "Fachabitur", "Abitur"))


############ Alter kategorial ############
group_age <- function(df) {
  df <- df %>%
    mutate(Age_Grouped = case_when(
      Age >= 18 & Age < 23 ~ 1,
      Age >= 23 & Age < 28 ~ 2,
      Age >= 28 & Age < 33 ~ 3,
      Age >= 33 & Age < 38 ~ 4,
      Age >= 38 & Age < 43 ~ 5,
      Age >= 43 & Age < 48 ~ 6,
      Age >= 48 & Age < 53 ~ 7,
      Age >= 53 & Age < 58 ~ 8,
      Age >= 58 & Age < 63 ~ 9,
      Age >= 63 & Age < 68 ~ 10,
      Age >= 68 & Age < 73 ~ 11,
      Age >= 73 & Age < 78 ~ 12,
      Age >= 78 & Age < 83 ~ 13,
      Age >= 83 & Age < 88 ~ 14,
      Age >= 88 & Age < 93 ~ 15,
      Age >= 93 ~ 16,
      TRUE ~ NA_real_
    ))
  return(df)
}

G09 <- group_age(G09)
G13 <- group_age(G13)
G17 <- group_age(G17)
G21 <- group_age(G21)


############### Berechnung Nettoäquivalenzeinkommen für 13, 17 und 21###########


equi <- function(df) {
  df <- df %>%
  mutate(H = 
    ifelse(cohabitA %in% 1:14, 0.3, ifelse(cohabitA %in% 14:103, 0.5, 0)) +
    ifelse(cohabitB %in% 1:14, 0.3, ifelse(cohabitB %in% 14:103, 0.5, 0)) +
    ifelse(cohabitC %in% 1:14, 0.3, ifelse(cohabitC %in% 14:103, 0.5, 0)) +
    ifelse(cohabitD %in% 1:14, 0.3, ifelse(cohabitD %in% 14:103, 0.5, 0)) +
    ifelse(cohabitE %in% 1:14, 0.3, ifelse(cohabitE %in% 14:103, 0.5, 0)) 
    + 1
  ) %>%
  mutate(Income = case_when(
    Income == 1 ~ 375,
    Income == 2 ~ 625,
    Income == 3 ~ 875,
    Income == 4 ~ 1125,
    Income == 5 ~ 1375,
    Income == 6 ~ 1759,
    Income == 7 ~ 2259,
    Income == 8 ~ 2759,
    Income == 9 ~ 3500,
    Income == 10 ~ 4500,
    Income == 11 ~ 6250,
    Income == 12 ~ 8759,
    Income == 13 ~ 15000
  )/H/100)%>%
  mutate(Income = round(Income, digits = 0))
}
G13 <- equi(G13)
G17 <- equi(G17)
G21 <- equi(G21)



G09 <- G09 %>%
  select(-cohabitA, -cohabitB, -cohabitC, -cohabitD, -cohabitE)
G13 <- G13 %>%
  select(-cohabitA, -cohabitB, -cohabitC, -cohabitD, -cohabitE)
G17 <- G17 %>%
  select(-cohabitA, -cohabitB, -cohabitC, -cohabitD, -cohabitE)
G21 <- G21 %>%
  select(-cohabitA, -cohabitB, -cohabitC, -cohabitD, -cohabitE)





############## Zusammenführung der Datensätze ############### 

G <- bind_rows(G09, G13, G17, G21)
#G1 <- bind_rows(G09, G13)
#G2 <- bind_rows(G17, G21)


#############################################################  
############### Deskriptive Statistik  ######################
#############################################################

###### delta_Wahlergebnisse für deskriptive Statistik #######


Delta_Union <- c(33.8-35.2, 41.5-33.8, 32.9-41.5, 24.1-32.9)
Delta_coal_par <- c(23-34.2, 4.8-14.6, 20.5-25.7, 25.7-20.5)
Date <- c(2009, 2013, 2017, 2021)
UNIONVOTE <- data.frame(
  Ergebnis = Result,
  Delta_E = Delta_Union,
  Year = Date
) %>%
  mutate(Datum = as.Date(paste0(Date, "-09-01"), format="%Y-%m-%d"))

#G <- left_join(G, UNIONVOTE, by = "Year")

##### Arbeitslosenquote, Inflation und Wachstumsrate im Zeitverlauf #####


Macrodata <- ggplot()+
  geom_line(data = subset(U, Datum >= "2005-09-01" & Datum <= "2022-01-01"), 
            aes(x = Datum, y = Arbeitslosenquote, linetype = "Arbeitslosenquote"), linewidth = 0.75)+
  geom_line(data = subset(I, Datum >= "2005-09-01" & Datum <= "2022-01-01"), 
            aes(x = Datum, y = Inflation, linetype = "Inflationsrate"))+
  geom_line(data = subset(GDP, Datum >= "2005-09-01" & Datum <= "2022-01-01"), 
            aes(x = Datum, y = WachstumsrateKette, linetype = "Wachstumsrate GDP"))+
  geom_point(data = UNIONVOTE, aes(x = Datum, y = Delta_Union, color = "\u0394 Wahlergebnis\nUnion"), shape = 17, size = 3) +
  scale_x_date(date_labels = "%Y", 
               breaks = as.Date(c("2005-09-01", "2007-09-01", "2009-09-01", "2011-09-01",
                                  "2013-09-01", "2015-09-01", "2017-09-01", "2019-09-01", "2021-09-01")),
               limits = as.Date(c("2005-09-01", "2022-01-01"))) +
  scale_y_continuous(breaks = seq(min(-10), 
                                  max(12), by = 1))+
  labs(title = "Arbeitslosenquote, Inflation und Wachstumsrate im Zeitverlauf",
       x = "Jahr", y = "Prozent", linetype = "Legende")+
  scale_linetype_manual(values = c("Arbeitslosenquote" = "dotted", 
                                   "Inflationsrate" = "longdash" ,
                                 "Wachstumsrate GDP" = "solid")) +
  scale_color_manual(values = c("\u0394 Wahlergebnis\nUnion" = "black")) +
  labs(color = "Form") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7a7a7a")+
  theme_classic()+
  theme(
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 11), 
    legend.key.size = unit(0.5, "lines"),
    legend.key.width = unit(2, "lines"), 
    legend.key.height = unit(0.5, "lines"))+
  guides(linetype = guide_legend(override.aes = list(linewidth = c(0.75, 0.5, 0.5))))  

ggsave(filename = "Output/Macrodata.png", plot = Macrodata, width = 8, height = 5, dpi = 300, units = "in")






####################Deskriptive Statistik####################



T1 <- tbl_summary(
  G09 %>%
    select(Macro, Micro,
           Union, SPD) %>%
    mutate(across(everything(), as.factor)),
  digits = list(all_categorical() ~ c(0, 1)))

T2 <- tbl_summary(
  G13 %>%
    select(Macro, Micro,
           Union, FDP) %>%
    mutate(across(everything(), as.factor)),
  digits = list(all_categorical() ~ c(0, 1)))
  
T3 <- tbl_summary(
  G17 %>%
    select(Macro, Micro, 
           Union, SPD) %>%
    mutate(across(everything(), as.factor)),
  digits = list(all_categorical() ~ c(0, 1)))
  
T4 <- tbl_summary(
  G21 %>%
    select(Macro, Micro, 
           Union, SPD) %>%
    mutate(across(everything(), as.factor)),
  digits = list(all_categorical() ~ c(0, 1)))

T5 <- tbl_summary(
  G %>%
    select(Macro, Micro, 
           Union, SPD, FDP) %>%
    mutate(across(everything(), as.factor)),
  digits = list(all_categorical() ~ c(0, 1)))

table_sum <- tbl_merge(list(T1, T2, T3, T4, T5), 
          tab_spanner = c("2009", "2013", "2017", "2021", "Gesamt")) %>% 
  modify_header(label ~ "**Variabel**") %>% 
  modify_spanning_header(stat_0_1 ~ "**2009**", stat_0_2 ~ "**2013**", 
                         stat_0_3 ~ "**2017**", stat_0_4 ~ "**2021**", 
                         stat_0_5 ~ "Gesamt")
write.csv(table_sum, "Output/table_sum.csv", row.names = FALSE)



T1f <- tbl_summary(
  G09 %>%
    select(Union, SPD, FDP, Macro, Micro, Gender, Age, 
           East, Education, Income, Union_Identification, SPD_Identification, FDP_Identification) %>%
    mutate(across(c(Union, SPD, FDP, Macro, Micro, Gender, 
           East, Education, Union_Identification, SPD_Identification, FDP_Identification), as.factor)),
  digits = list(all_categorical() ~ c(0, 1))
)

T2f <- tbl_summary(
  G13 %>%
    select(Union, SPD, FDP, Macro, Micro, Gender, Age, 
           East, Education, Income, Union_Identification, SPD_Identification, FDP_Identification) %>%
    mutate(across(c(Union, SPD, FDP, Macro, Micro, Gender, 
           East, Education, Union_Identification, SPD_Identification, FDP_Identification), as.factor)),
  digits = list(all_categorical() ~ c(0, 1))
)

T3f <- tbl_summary(
  G17 %>%
    select(Union, SPD, FDP, , Macro, Micro, Gender, Age, 
           East, Education, Income, Union_Identification, SPD_Identification, FDP_Identification) %>%
    mutate(across(c(Union, SPD, FDP, Macro, Micro, Gender,  
           East, Education, Union_Identification, SPD_Identification, FDP_Identification), as.factor)),
  digits = list(all_categorical() ~ c(0, 1))
)

T4f <- tbl_summary(
  G21 %>%
    select(Union, SPD, FDP, Macro, Micro, Gender, Age, 
           East, Education, Income, Union_Identification, SPD_Identification, FDP_Identification) %>%
    mutate(across(c(Union, SPD, FDP, Macro, Micro, Gender, 
           East, Education, Union_Identification, SPD_Identification, FDP_Identification), as.factor)),
  digits = list(all_categorical() ~ c(0, 1))
)

T5f <- tbl_summary(
  G %>%
    select(Union, SPD, FDP, Macro, Micro, Gender, Age, 
           East, Education, Income, Union_Identification, SPD_Identification, FDP_Identification) %>%
    mutate(across(c(Union, SPD, FDP, Macro, Micro, Gender, 
           East, Education, Union_Identification, SPD_Identification, FDP_Identification), as.factor)),
  digits = list(all_categorical() ~ c(0, 1))
)

table_sum2 <- tbl_merge(list(T1f, T2f, T3f, T4f, T5f), 
          tab_spanner = c("2009", "2013", "2017", "2021", "Gesamt")) %>% 
  modify_header(label ~ "**Variabel**") %>% 
  modify_spanning_header(stat_0_1 ~ "**2009**", stat_0_2 ~ "**2013**", 
                         stat_0_3 ~ "**2017**", stat_0_4 ~ "**2021**", 
                         stat_0_5 ~ "Gesamt")
write.csv(table_sum2, "Output/table_sum_full.csv", row.names = FALSE)


#bedingte Wahrscheinlichkeitstabelle

conprob <- function(x, y){
       prob_tbl <- round(prop.table(table(x, y),margin =1)*100, 1)
       row_order <- c("_decrease", "_remain", "_increase")
       col_order <- c("_decrease", "_remain", "_increase")
       row_order <- intersect(row_order, rownames(prob_tbl))
       col_order <- intersect(col_order, colnames(prob_tbl))
       prob_tbl <- prob_tbl[row_order, col_order]
       prob_tbl <- kable(prob_tbl, format = "html", summary = FALSE)%>%
       add_header_above(c(" " = 1, "Macro Values" = 3))
       return(prob_tbl)
}

contbl09 <- conprob(G09$Micro, G09$Macro)
contbl13 <- conprob(G13$Micro, G13$Macro)
contbl17 <- conprob(G17$Micro, G17$Macro)
contbl21 <- conprob(G21$Micro, G21$Macro)

con_html_table <- paste(
  "<div style='display: flex; flex-wrap: wrap; justify-content: space-between; gap: 20px;'>",  # flex-wrap bleibt gleich
  "<div style='flex: 1; min-width: 42%; margin-bottom: 20px;'>", contbl09, "</div>",  # Erste Tabelle
  "<div style='flex: 1; min-width: 42%; margin-bottom: 20px;'>", contbl13, "</div>",  # Zweite Tabelle
  "<div style='flex: 1; min-width: 42%; margin-bottom: 20px;'>", contbl17, "</div>",  # Dritte Tabelle
  "<div style='flex: 1; min-width: 42%; margin-bottom: 20px;'>", contbl21, "</div>",  # Vierte Tabelle
  "</div>"
)

writeLines(con_html_table, "Output/contingency_table.html")


#############################################################
#####################Regressionsmodelle#######################
#############################################################

#########2009#########

M1 <- glm(Union ~ Macro + Age + Gender + East + Education + Income + Union_Identification, data = G09, 
          family = binomial(link = "logit"))

M2 <- glm(SPD ~ Macro + Age + Gender + East + Education + Income + SPD_Identification, data = G09, 
          family = binomial(link = "logit"))
          
M3 <- glm(Union ~ Micro + Age + Gender + East + Education + Income + Union_Identification, data = G09, 
          family = binomial(link = "logit"))

M4 <- glm(SPD ~ Micro + Age + Gender + East + Education + Income + SPD_Identification, data = G09, 
          family = binomial(link = "logit")) 

##########2013##########

M5 <- glm(Union ~ Macro + Age + Gender + East + Education + Income + Union_Identification, data = G13, 
          family = binomial(link = "logit"))

M6 <- glm(FDP ~ Macro + Age + Gender + East + Education + Income + FDP_Identification, data = G13, 
          family = binomial(link = "logit"))

M7<- glm(Union ~ Micro + Age + Gender + East + Education + Income + Union_Identification, data = G13, 
          family = binomial(link = "logit"))

M8 <- glm(FDP ~ Micro + Age + Gender + East + Education + Income + FDP_Identification, data = G13, 
          family = binomial(link = "logit"))


##########2017##########

M9 <- glm(Union ~  Macro + Age + Gender + East + Education + Income + Union_Identification, data = G17, 
          family = binomial(link = "logit"))
          
M10 <- glm(SPD ~ Macro + Age + Gender + East + Education + Income + SPD_Identification, data = G17, 
          family = binomial(link = "logit"))

M11 <- glm(Union ~ Micro + Age + Gender + East + Education + Income + Union_Identification, data = G17, 
          family = binomial(link = "logit"))

M12 <- glm(SPD ~ Micro + Age + Gender + East + Education + Income + SPD_Identification, data = G17, 
          family = binomial(link = "logit"))

##########2021##########

M13 <- glm(Union ~  Macro + Age + Gender + East + Education + Income + Union_Identification, data = G21,
          family = binomial(link = "logit"))

M14 <- glm(SPD ~ Macro + Age + Gender + East + Education + Income + SPD_Identification, data = G21, 
          family = binomial(link = "logit"))

M15 <- glm(Union ~ Micro + Age + Gender + East + Education + Income + Union_Identification, data = G21, 
          family = binomial(link = "logit"))

M16 <- glm(SPD ~ Micro + Age + Gender + East + Education + Income + SPD_Identification, data = G21, 
          family = binomial(link = "logit"))



M01 <- glm(Union ~ Age + Gender + East + Education + Income + Union_Identification, data = G09, 
          family = binomial(link = "logit"))

M02 <- glm(Union ~ Age + Gender + East + Education + Income + Union_Identification, data = G13, 
          family = binomial(link = "logit"))

M03 <- glm(Union ~ Age + Gender + East + Education + Income + Union_Identification, data = G17, 
          family = binomial(link = "logit"))

M04 <- glm(Union ~ Age + Gender + East + Education + Income + Union_Identification, data = G21,
          family = binomial(link = "logit"))




#Erstellen der ersten Tabelle


nagel_r2_M1 <- PseudoR2(M1, which = "Nagelkerke") 
nagel_r2_M2 <- PseudoR2(M2, which = "Nagelkerke")
nagel_r2_M3 <- PseudoR2(M3, which = "Nagelkerke")
nagel_r2_M4 <- PseudoR2(M4, which = "Nagelkerke")
nagel_r2_M5 <- PseudoR2(M5, which = "Nagelkerke")
nagel_r2_M6 <- PseudoR2(M6, which = "Nagelkerke")
nagel_r2_M7 <- PseudoR2(M7, which = "Nagelkerke")
nagel_r2_M8 <- PseudoR2(M8, which = "Nagelkerke")
nagel_r2_M9 <- PseudoR2(M9, which = "Nagelkerke") 
nagel_r2_M10 <- PseudoR2(M10, which = "Nagelkerke")
nagel_r2_M11 <- PseudoR2(M11, which = "Nagelkerke")
nagel_r2_M12 <- PseudoR2(M12, which = "Nagelkerke")
nagel_r2_M13 <- PseudoR2(M13, which = "Nagelkerke") 
nagel_r2_M14 <- PseudoR2(M14, which = "Nagelkerke")
nagel_r2_M15 <- PseudoR2(M15, which = "Nagelkerke")
nagel_r2_M16 <- PseudoR2(M16, which = "Nagelkerke")

nagel_r2_M01 <- PseudoR2(M01, which = "Nagelkerke")
nagel_r2_M02 <- PseudoR2(M02, which = "Nagelkerke")
nagel_r2_M03 <- PseudoR2(M03, which = "Nagelkerke")
nagel_r2_M04 <- PseudoR2(M04, which = "Nagelkerke")



promargins <- function(x) {
    y <- summary(margins(x))   
    y$AME <- round(y$AME * 100, 4)
    y$lower <- round(y$lower * 100, 4)
    y$upper <- round(y$upper * 100, 4)
    return(y)
  }


ame_M1 <- promargins(M1)
ame_M2 <- promargins(M2)
ame_M3 <- promargins(M3)
ame_M4 <- promargins(M4)
ame_M5 <- promargins(M5)
ame_M6 <- promargins(M6)
ame_M7 <- promargins(M7)
ame_M8 <- promargins(M8)
ame_M9 <- promargins(M9)
ame_M10 <- promargins(M10)
ame_M11 <- promargins(M11)
ame_M12 <- promargins(M12)
ame_M13 <- promargins(M13)
ame_M14 <- promargins(M14)
ame_M15 <- promargins(M15)
ame_M16 <- promargins(M16)

ame_M01 <- promargins(M01)
ame_M02 <- promargins(M02)
ame_M03 <- promargins(M03)
ame_M04 <- promargins(M04)

ame1 <- as.vector(ame_M1$AME)
ame2 <- as.vector(ame_M2$AME)
ame3 <- as.vector(ame_M3$AME)
ame4 <- as.vector(ame_M4$AME)
ame5 <- as.vector(ame_M5$AME)
ame6 <- as.vector(ame_M6$AME)
ame7 <- as.vector(ame_M7$AME)
ame8 <- as.vector(ame_M8$AME)
ame9 <- as.vector(ame_M9$AME)
ame10 <- as.vector(ame_M10$AME)
ame11 <- as.vector(ame_M11$AME)
ame12 <- as.vector(ame_M12$AME)
ame13 <- as.vector(ame_M13$AME)
ame14 <- as.vector(ame_M14$AME)
ame15 <- as.vector(ame_M15$AME)
ame16 <- as.vector(ame_M16$AME)

ame01 <- as.vector(ame_M01$AME)
ame02 <- as.vector(ame_M02$AME)
ame03 <- as.vector(ame_M03$AME)
ame04 <- as.vector(ame_M04$AME)

names(ame1) <- ame_M1$factor
names(ame2) <- ame_M2$factor
names(ame3) <- ame_M3$factor
names(ame4) <- ame_M4$factor
names(ame5) <- ame_M5$factor
names(ame6) <- ame_M6$factor
names(ame7) <- ame_M7$factor
names(ame8) <- ame_M8$factor
names(ame9) <- ame_M9$factor
names(ame10) <- ame_M10$factor
names(ame11) <- ame_M11$factor
names(ame12) <- ame_M12$factor
names(ame13) <- ame_M13$factor
names(ame14) <- ame_M14$factor
names(ame15) <- ame_M15$factor
names(ame16) <- ame_M16$factor

names(ame01) <- ame_M01$factor
names(ame02) <- ame_M02$factor
names(ame03) <- ame_M03$factor
names(ame04) <- ame_M04$factor

p1 <- ame_M1$p
p2 <- ame_M2$p
p3 <- ame_M3$p
p4 <- ame_M4$p
p5 <- ame_M5$p
p6 <- ame_M6$p
p7 <- ame_M7$p
p8 <- ame_M8$p
p9 <- ame_M9$p
p10 <- ame_M10$p
p11 <- ame_M11$p
p12 <- ame_M12$p
p13 <- ame_M13$p
p14 <- ame_M14$p
p15 <- ame_M15$p
p16 <- ame_M16$p

p01 <- ame_M01$p
p02 <- ame_M02$p
p03 <- ame_M03$p
p04 <- ame_M04$p

names(p1) <- ame_M1$factor
names(p2) <- ame_M2$factor
names(p3) <- ame_M3$factor
names(p4) <- ame_M4$factor
names(p5) <- ame_M5$factor
names(p6) <- ame_M6$factor
names(p7) <- ame_M7$factor
names(p8) <- ame_M8$factor
names(p9) <- ame_M9$factor
names(p10) <- ame_M10$factor
names(p11) <- ame_M11$factor
names(p12) <- ame_M12$factor
names(p13) <- ame_M13$factor
names(p14) <- ame_M14$factor
names(p15) <- ame_M15$factor
names(p16) <- ame_M16$factor

names(p01) <- ame_M01$factor
names(p02) <- ame_M02$factor
names(p03) <- ame_M03$factor
names(p04) <- ame_M04$factor

ci1 <- ame_M1[, c("lower", "upper")]
ci2 <- ame_M2[, c("lower", "upper")]
ci3 <- ame_M3[, c("lower", "upper")]
ci4 <- ame_M4[, c("lower", "upper")]
ci5 <- ame_M5[, c("lower", "upper")]
ci6 <- ame_M6[, c("lower", "upper")]
ci7 <- ame_M7[, c("lower", "upper")]
ci8 <- ame_M8[, c("lower", "upper")]
ci9 <- ame_M9[, c("lower", "upper")]
ci10 <- ame_M10[, c("lower", "upper")]
ci11 <- ame_M11[, c("lower", "upper")]
ci12 <- ame_M12[, c("lower", "upper")]
ci13 <- ame_M13[, c("lower", "upper")]
ci14 <- ame_M14[, c("lower", "upper")]
ci15 <- ame_M15[, c("lower", "upper")]
ci16 <- ame_M16[, c("lower", "upper")]

ci01 <- ame_M01[, c("lower", "upper")]
ci02 <- ame_M02[, c("lower", "upper")]
ci03 <- ame_M03[, c("lower", "upper")]
ci04 <- ame_M04[, c("lower", "upper")]

rownames(ci1) <- ame_M1$factor
rownames(ci2) <- ame_M2$factor
rownames(ci3) <- ame_M3$factor
rownames(ci4) <- ame_M4$factor
rownames(ci5) <- ame_M5$factor
rownames(ci6) <- ame_M6$factor
rownames(ci7) <- ame_M7$factor
rownames(ci8) <- ame_M8$factor
rownames(ci9) <- ame_M9$factor
rownames(ci10) <- ame_M10$factor
rownames(ci11) <- ame_M11$factor
rownames(ci12) <- ame_M12$factor
rownames(ci13) <- ame_M13$factor
rownames(ci14) <- ame_M14$factor
rownames(ci15) <- ame_M15$factor
rownames(ci16) <- ame_M16$factor

rownames(ci01) <- ame_M01$factor
rownames(ci02) <- ame_M02$factor
rownames(ci03) <- ame_M03$factor
rownames(ci04) <- ame_M04$factor

#Erstelle Modelle mit AMEs
ameM1 <- M1
ameM2 <- M2
ameM3 <- M3
ameM4 <- M4
ameM5 <- M5
ameM6 <- M6
ameM7 <- M7
ameM8 <- M8
ameM9 <- M9
ameM10 <- M10
ameM11 <- M11
ameM12 <- M12
ameM13 <- M13
ameM14 <- M14
ameM15 <- M15
ameM16 <- M16

ameM01 <- M01
ameM02 <- M02
ameM03 <- M03
ameM04 <- M04

ameM1$coefficients <- ame1
ameM2$coefficients <- ame2
ameM3$coefficients <- ame3
ameM4$coefficients <- ame4
ameM5$coefficients <- ame5
ameM6$coefficients <- ame6
ameM7$coefficients <- ame7
ameM8$coefficients <- ame8
ameM9$coefficients <- ame9
ameM10$coefficients <- ame10
ameM11$coefficients <- ame11
ameM12$coefficients <- ame12
ameM13$coefficients <- ame13
ameM14$coefficients <- ame14
ameM15$coefficients <- ame15
ameM16$coefficients <- ame16

ameM01$coefficients <- ame01
ameM02$coefficients <- ame02
ameM03$coefficients <- ame03
ameM04$coefficients <- ame04

order1 <- c("GDPgrowth", "Inflation", "UnemploymentRate", "Macro", "Micro", 
            "Gender", "Age", "Age_Grouped", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7", "Age8", "East", "Education", "Abitur", "Fachabitur", "Realschule",
            "Income", "Union_Identification", "SPD_Identification", "FDP_Identification")




combined_models1 <- bind_rows(
  ame_M13, 
  ame_M9, 
  ame_M5, 
  ame_M1,
  .id = "model"  
)

combined_models1$factor <- factor(combined_models1$factor, levels = rev(c(
  "Macro", "Micro", "Gender", "Age",
  "Age_Grouped", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7",
  "Age8", "East", "Education", "Abitur", "Fachabitur", "Realschule",
  "Income", "Union_Identification", "SPD_Identification", "FDP_Identification")))

combined_models2 <- bind_rows(
  ame_M15, 
  ame_M11, 
  ame_M7, 
  ame_M3,
    .id = "model")  

combined_models2$factor <- factor(combined_models2$factor, levels = rev(c(
  "Macro", "Micro", "Gender", "Age",
  "Age_Grouped", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7",
  "Age8", "East", "Education", "Abitur", "Fachabitur", "Realschule", 
  "Union_Identification", "SPD_Identification", "FDP_Identification")))
                                                


# Confusionmatrix
predictM1 <- predict(M1, type = "response")
predictM2 <- predict(M2, type = "response")
predictM3 <- predict(M3, type = "response")
predictM4 <- predict(M4, type = "response")
predictM5 <- predict(M5, type = "response")
predictM6 <- predict(M6, type = "response")
predictM7 <- predict(M7, type = "response")
predictM8 <- predict(M8, type = "response")
predictM9 <- predict(M9, type = "response")
predictM10 <- predict(M10, type = "response")
predictM11 <- predict(M11, type = "response")
predictM12 <- predict(M12, type = "response")
predictM13 <- predict(M13, type = "response")
predictM14 <- predict(M14, type = "response")
predictM15 <- predict(M15, type = "response")
predictM16 <- predict(M16, type = "response")

predictM01 <- predict(M01, type = "response")
predictM02 <- predict(M02, type = "response")
predictM03 <- predict(M03, type = "response")
predictM04 <- predict(M04, type = "response")


confusionM1 <- data.frame(Vote = as.factor(G09$Union), predictM1)
confusionM2 <- data.frame(Vote = as.factor(G09$SPD), predictM2)
confusionM3 <- data.frame(Vote = as.factor(G09$Union), predictM3)
confusionM4 <- data.frame(Vote = as.factor(G09$SPD), predictM4)
confusionM5 <- data.frame(Vote = as.factor(G13$Union), predictM5)
confusionM6 <- data.frame(Vote = as.factor(G13$FDP), predictM6)
confusionM7 <- data.frame(Vote = as.factor(G13$Union), predictM7)
confusionM8 <- data.frame(Vote = as.factor(G13$FDP), predictM8)
confusionM9 <- data.frame(Vote = as.factor(G17$Union), predictM9)
confusionM10 <- data.frame(Vote = as.factor(G17$SPD), predictM10)
confusionM11 <- data.frame(Vote = as.factor(G17$Union), predictM11)
confusionM12 <- data.frame(Vote = as.factor(G17$SPD), predictM12)
confusionM13 <- data.frame(Vote = as.factor(G21$Union), predictM13)
confusionM14 <- data.frame(Vote = as.factor(G21$SPD), predictM14)
confusionM15 <- data.frame(Vote = as.factor(G21$Union), predictM15)
confusionM16 <- data.frame(Vote = as.factor(G21$SPD), predictM16)

confusionM01 <- data.frame(Vote = as.factor(G09$Union), predictM01)
confusionM02 <- data.frame(Vote = as.factor(G13$Union), predictM02)
confusionM03 <- data.frame(Vote = as.factor(G17$Union), predictM03)
confusionM04 <- data.frame(Vote = as.factor(G21$Union), predictM04)


predictM1 <- as.factor(ifelse(confusionM1$predictM1 < 0.5, 0, 1))
predictM2 <- as.factor(ifelse(confusionM2$predictM2 < 0.5, 0, 1))
predictM3 <- as.factor(ifelse(confusionM3$predictM3 < 0.5, 0, 1))
predictM4 <- as.factor(ifelse(confusionM4$predictM4 < 0.5, 0, 1))
predictM5 <- as.factor(ifelse(confusionM5$predictM5 < 0.5, 0, 1))
predictM6 <- as.factor(ifelse(confusionM6$predictM6 < 0.5, 0, 1))
predictM7 <- as.factor(ifelse(confusionM7$predictM7 < 0.5, 0, 1))
predictM8 <- as.factor(ifelse(confusionM8$predictM8 < 0.5, 0, 1))
predictM9 <- as.factor(ifelse(confusionM9$predictM9 < 0.5, 0, 1))
predictM10 <- as.factor(ifelse(confusionM10$predictM10 < 0.5, 0, 1))
predictM11 <- as.factor(ifelse(confusionM11$predictM11 < 0.5, 0, 1))
predictM12 <- as.factor(ifelse(confusionM12$predictM12 < 0.5, 0, 1))
predictM13 <- as.factor(ifelse(confusionM13$predictM13 < 0.5, 0, 1))
predictM14 <- as.factor(ifelse(confusionM14$predictM14 < 0.5, 0, 1))
predictM15 <- as.factor(ifelse(confusionM15$predictM15 < 0.5, 0, 1))
predictM16 <- as.factor(ifelse(confusionM16$predictM16 < 0.5, 0, 1))

predictM01 <- as.factor(ifelse(confusionM01$predictM01 < 0.5, 0, 1))
predictM02 <- as.factor(ifelse(confusionM02$predictM02 < 0.5, 0, 1))
predictM03 <- as.factor(ifelse(confusionM03$predictM03 < 0.5, 0, 1))
predictM04 <- as.factor(ifelse(confusionM04$predictM04 < 0.5, 0, 1))


conma1 <- confusionMatrix(predictM1, confusionM1$Vote)
conma2 <- confusionMatrix(predictM2, confusionM2$Vote)
conma3 <- confusionMatrix(predictM3, confusionM3$Vote)
conma4 <- confusionMatrix(predictM4, confusionM4$Vote)
conma5 <- confusionMatrix(predictM5, confusionM5$Vote)
conma6 <- confusionMatrix(predictM6, confusionM6$Vote)
conma7 <- confusionMatrix(predictM7, confusionM7$Vote)
conma8 <- confusionMatrix(predictM8, confusionM8$Vote)
conma9 <- confusionMatrix(predictM9, confusionM9$Vote)
conma10 <- confusionMatrix(predictM10, confusionM10$Vote)
conma11 <- confusionMatrix(predictM11, confusionM11$Vote)
conma12 <- confusionMatrix(predictM12, confusionM12$Vote)
conma13 <- confusionMatrix(predictM13, confusionM13$Vote)
conma14 <- confusionMatrix(predictM14, confusionM14$Vote)
conma15 <- confusionMatrix(predictM15, confusionM15$Vote)
conma16 <- confusionMatrix(predictM16, confusionM16$Vote)

conma01 <- confusionMatrix(predictM01, confusionM01$Vote)
conma02 <- confusionMatrix(predictM02, confusionM02$Vote)
conma03 <- confusionMatrix(predictM03, confusionM03$Vote)
conma04 <- confusionMatrix(predictM04, confusionM04$Vote)

#### Regressionstabellen

stargazer(ameM1, ameM2, ameM3, ameM4, 
          out = "Output/Reg_Tabelle_1.html",
          p = list(p1, p2, p3, p4),
          ci.custom = list(ci1, ci2, ci3, ci4),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M1, 3), signif(nagel_r2_M2, 3), 
                             signif(nagel_r2_M3, 3), signif(nagel_r2_M4, 3)),
                           c("Accuracy (%)", signif(conma1$overall[1]*100, 3), signif(conma2$overall[1]*100, 3),
                           signif(conma3$overall[1]*100, 3), signif(conma4$overall[1]*100, 3))),
          order = order1, 
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("Modell 1", "Modell 2", "Modell 3", "Modell 4"),
          title= "Average Marginal Effects (AME) in %, 2009")

stargazer(ameM5, ameM6, ameM7, ameM8, 
          out = "Output/Reg_Tabelle_2.html", 
          p = list(p5, p6, p7, p8),
          ci.custom = list(ci5, ci6, ci7, ci8),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M5, 3), signif(nagel_r2_M6, 3), 
                                                 signif(nagel_r2_M7, 3), signif(nagel_r2_M8, 3)),
                           c("Accuracy (%)", signif(conma5$overall[1]*100, 3), signif(conma6$overall[1]*100, 3),
                           signif(conma7$overall[1]*100, 3), signif(conma8$overall[1]*100, 3))),
          order = order1,
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("Modell 5", "Modell 6", "Modell 7", "Modell 8"),
          title="Average Marginal Effects (AME) in %, 2013")

stargazer(ameM9, ameM10, ameM11, ameM12, 
          out = "Output/Reg_Tabelle_3.html", 
          p = list(p9, p10, p11, p12),
          ci.custom = list(ci9, ci10, ci11, ci12),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M9, 3), signif(nagel_r2_M10, 3), 
                                                 signif(nagel_r2_M11, 3), signif(nagel_r2_M12, 3)),
                           c("Accuracy (%)", signif(conma9$overall[1]*100, 3), signif(conma10$overall[1]*100, 3),
                           signif(conma11$overall[1]*100, 3), signif(conma12$overall[1]*100, 3))),
          order = order1,
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("Modell 9", "Modell 10", "Modell 11", "Modell 12"),
          title="Average Marginal Effects (AME) in %, 2017")

stargazer(ameM13, ameM14, ameM15, ameM16, 
          out = "Output/Reg_Tabelle_4.html", 
          p = list(p13, p14, p15, p16),
          ci.custom = list(ci13, ci14, ci15, ci16),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M13, 3), signif(nagel_r2_M14, 3), 
                                                 signif(nagel_r2_M15, 3), signif(nagel_r2_M16, 3)),
                           c("Accuracy (%)", signif(conma13$overall[1]*100, 3), signif(conma14$overall[1]*100, 3),
                           signif(conma15$overall[1]*100, 3), signif(conma16$overall[1]*100, 3))),                      
          order = order1,
          model.numbers = FALSE,
          omit.stat = "aic", 
          column.labels = c("Modell 13", "Modell 14", "Modell 15", "Modell 16"),
          title="Average Marginal Effects (AME) in %, 2021")

### Regressionstabellen mit Basismodellen

stargazer(ameM01, ameM1, ameM2, ameM3, ameM4, 
          out = "Output/Reg_Tabelle_01.html",
          p = list(p01, p1, p2, p3, p4),
          ci.custom = list(ci01, ci1, ci2, ci3, ci4),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M01, 3), signif(nagel_r2_M1, 3), signif(nagel_r2_M2, 3), 
                             signif(nagel_r2_M3, 3), signif(nagel_r2_M4, 3)),
                           c("Accuracy (%)", signif(conma01$overall[1]*100, 3), signif(conma1$overall[1]*100, 3), signif(conma2$overall[1]*100, 3),
                           signif(conma3$overall[1]*100, 3), signif(conma4$overall[1]*100, 3))),
          order = order1, 
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("control", "Modell 1", "Modell 2", "Modell 3", "Modell 4"),
          title= "Average Marginal Effects (AME) in %, 2009")

stargazer(ameM02, ameM5, ameM6, ameM7, ameM8, 
          out = "Output/Reg_Tabelle_02.html", 
          p = list(p02, p5, p6, p7, p8),
          ci.custom = list(ci02, ci5, ci6, ci7, ci8),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M02, 3), signif(nagel_r2_M5, 3), signif(nagel_r2_M6, 3), 
                                                 signif(nagel_r2_M7, 3), signif(nagel_r2_M8, 3)),
                           c("Accuracy (%)", signif(conma02$overall[1]*100, 3), signif(conma6$overall[1]*100, 3),
                           signif(conma7$overall[1]*100, 3), signif(conma8$overall[1]*100, 3))),
          order = order1,
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("control", "Modell 5", "Modell 6", "Modell 7", "Modell 8"),
          title="Average Marginal Effects (AME) in %, 2013")

stargazer(ameM03, ameM9, ameM10, ameM11, ameM12, 
          out = "Output/Reg_Tabelle_03.html", 
          p = list(p03, p9, p10, p11, p12),
          ci.custom = list(ci03, ci9, ci10, ci11, ci12),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M03, 3), signif(nagel_r2_M9, 3), signif(nagel_r2_M10, 3), 
                                                 signif(nagel_r2_M11, 3), signif(nagel_r2_M12, 3)),
                           c("Accuracy (%)", signif(conma03$overall[1]*100, 3), signif(conma9$overall[1]*100, 3), signif(conma10$overall[1]*100, 3),
                           signif(conma11$overall[1]*100, 3), signif(conma12$overall[1]*100, 3))),
          order = order1,
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("control", "Modell 9", "Modell 10", "Modell 11", "Modell 12"),
          title="Average Marginal Effects (AME) in %, 2017")

stargazer(ameM04, ameM13, ameM14, ameM15, ameM16, 
          out = "Output/Reg_Tabelle_04.html", 
          p = list(p04, p13, p14, p15, p16),
          ci.custom = list(ci04, ci13, ci14, ci15, ci16),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M04, 3), signif(nagel_r2_M13, 3), signif(nagel_r2_M14, 3), 
                                                 signif(nagel_r2_M15, 3), signif(nagel_r2_M16, 3)),
                           c("Accuracy (%)", signif(conma04$overall[1]*100, 3), signif(conma13$overall[1]*100, 3), signif(conma14$overall[1]*100, 3),
                           signif(conma15$overall[1]*100, 3), signif(conma16$overall[1]*100, 3))),                      
          order = order1,
          model.numbers = FALSE,
          omit.stat = "aic", 
          column.labels = c("control", "Modell 13", "Modell 14", "Modell 15", "Modell 16"),
          title="Average Marginal Effects (AME) in %, 2021")



### Baisismodelle

stargazer(ameM01, ameM02, ameM03, ameM04, 
          out = "Output/Reg_Tabelle_5.html", 
          p = list(p01, p02, p03, p04),
          ci.custom = list(ci01, ci02, ci03, ci04),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_M01, 3), signif(nagel_r2_M02, 3), 
                                                 signif(nagel_r2_M03, 3), signif(nagel_r2_M04, 3)),
                           c("Accuracy (%)", signif(conma01$overall[1]*100, 3), signif(conma02$overall[1]*100, 3),
                           signif(conma03$overall[1]*100, 3), signif(conma04$overall[1]*100, 3))),                      
          order = order1,
          model.numbers = FALSE,
          omit.stat = "aic", 
          column.labels = c("control", "2009", "2013", "2017", "2021"),
          title="Average Marginal Effects (AME) in %, Basismodelle")





########## Confusion Plots ############
conplot1 <- ggplot(confusionM1, aes(predictM1, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
        labs(x = "Predictions", title = "Modell 1")

conplot2 <- ggplot(confusionM2, aes(predictM2, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
        labs(x = "Predictions", title = "Modell 2")

conplot3 <- ggplot(confusionM3, aes(predictM3, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 3")

conplot4 <- ggplot(confusionM4, aes(predictM4, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 4")

conplot5 <- ggplot(confusionM5, aes(predictM5, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 5")

conplot6 <- ggplot(confusionM6, aes(predictM6, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 6")

conplot7 <- ggplot(confusionM7, aes(predictM7, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 7")

conplot8 <- ggplot(confusionM8, aes(predictM8, y = 0, color = Vote))+
geom_jitter(height = 0.05,width = 0)+
geom_vline(xintercept = 0.5, linetype = "dashed", color = "black")+
scale_y_continuous(breaks = NULL)+
scale_color_manual(values = c("1" = "black", "0" = "grey")) + 
theme_minimal() +
theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
labs(x = "Predictions", title = "Modell 8")

predplot <- (conplot1 | conplot2)/(conplot3 | conplot4)/(conplot5 | conplot6)/(conplot7 | conplot8)+plot_layout(guides = "collect")&theme(legend.position = "bottom")

ggsave(filename = "Output/Predictionplots.png", plot = predplot, height = 18, width = 10.8)


AMEplot1 <- ggplot(combined_models1, aes(y = factor, x = AME, color = model, shape = model)) +     
  geom_point(position = position_dodge(width = 0.55), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.37, 
                position = position_dodge(width = 0.55)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey", linewidth = 0.5) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05)),
                  labels = c("Macro_increase" = "Macro increase", 
                            "Macro_decrease" = "Macro decrease",
                            "Gender" = "Gender",
                            "Age_Grouped" = "Age",
                            "East" = "East",
                            "Education" = "Education",
                            "Income" = "Income",
                            "Union_Identification" = "Party identification")) +
  labs(title = "Average Marginal Effects (AME) in %, Macro",
       subtitle = "Dependent variable: CDU/CSU",
       y = "Predictors", x = "AME in percent") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size =12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),          
        legend.title = element_text(size = 12),                      
        legend.text = element_text(size = 10)) +
  scale_color_manual(values = c("4" = "black", "3" = "#53535381", "2" = "black", "1" = "#53535381"), 
                     name = "Election year",
                     guide = guide_legend(reverse = TRUE), 
                     labels = c("2021", "2017", "2013", "2009")) +  
  scale_shape_manual(values = c(19, 17, 15, 18),   
                     name = "Election year", 
                     guide = guide_legend(reverse = TRUE), 
                     labels = c("2021", "2017", "2013", "2009")) 
 
ggsave("Output/Macro_models.jpg", plot = AMEplot1, width = 10, height=9, dpi = 2000)


AMEplot2 <- ggplot(combined_models2, aes(y = factor, x = AME, color = model, shape = model)) +     
  geom_point(position = position_dodge(width = 0.55), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.37, 
                position = position_dodge(width = 0.55)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey", linewidth = 0.5) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05)),
                  labels = c("Micro_increase" = "Micro increase", 
                            "Micro_decrease" = "Micro decrease",
                            "Gender" = "Gender",
                            "Age_Grouped" = "Age",
                            "East" = "East",
                            "Education" = "Education",
                            "Income" = "Income", 
                            "Union_Identification" = "Party identification")) +
  labs(title = "Average Marginal Effects (AME) in %, Micro",
       subtitle = "Dependent variable: CDU/CSU",
       y = "Predictors", x = "AME in percent") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("4" = "black", "3" = "#53535381", "2" = "black", "1" = "#53535381"), 
                     name = "Election year",
                     guide = guide_legend(reverse = TRUE),
                     labels = c("2021", "2017", "2013", "2009")) +  
  scale_shape_manual(values = c(19, 17, 15, 18), 
                     name = "Election year", 
                     guide = guide_legend(reverse = TRUE),
                     labels = c("2021", "2017", "2013", "2009")) 
 
ggsave("Output/Micro_models.jpg", plot = AMEplot2, width = 10, height = 9, dpi = 2000)

# Plots für Präsi


create_plot <- function(data, title) {
  dummy_vars <- select(data, Macro, Micro)
  filtered_data <- lapply(dummy_vars, function(col) {
    col <- factor(col, levels = c("_decrease", "_increase", "_remain"))
    table(col)
  })

 
  plot_data <- do.call(rbind, lapply(names(filtered_data), function(name) {
    freq_table <- filtered_data[[name]]
    data.frame(
      Variable = name,
      Category = names(freq_table),
      Frequency = as.numeric(freq_table) / sum(freq_table) n
    )
  }))

 
  plot_data$Variable <- factor(plot_data$Variable, levels = unique(plot_data$Variable))
  plot_data$Category <- factor(plot_data$Category, levels = c("_decrease", "_increase", "_remain"))


  ggplot(plot_data, aes(x = Variable, y = Frequency, fill = Category)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
    labs(
      title = title,
      x = "",
      y = "Relative Häufigkeit"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("_decrease" = "red", "_increase" = "#56B4E9", "_remain" = "green")) +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(
      aes(label = scales::percent(Frequency)),
      position = position_dodge(width = 0.6),  
      size = 3
    ) +
    theme(legend.position = "none")  



plot_G09 <- create_plot(G09, "2009")
plot_G13 <- create_plot(G13, "2013")
plot_G17 <- create_plot(G17, "2017")
plot_G21 <- create_plot(G21, "2021")


combined_plot <- plot_G09 + plot_G13 + plot_G17 + plot_G21 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")  


combined_plot
ggsave("Output/combined_plot.png", plot = combined_plot, width = 14, height = 8, dpi = 300)

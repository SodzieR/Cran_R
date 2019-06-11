# Setup

library(dplyr)
library(ggplot2)
# 11

dane <- 
	read.csv("D:/Studenci/Biostatystyka/Janucik Adrian/Zaliczenie Pakiety/dane_dzieci_csv.csv", sep=';', dec=',')

# 12

RBC_mean <- mean(dane$RBC)
RBC_median <- median(dane$RBC)
RBC_min <- min(dane$RBC)
RBC_max <- max(dane$RBC)

HGB_mean <- mean(dane$HGB)
HGB_median <- median(dane$HGB)
HGB_min <- min(dane$HGB)
HGB_max <- max(dane$HGB)

# 13

dzieci_niedokrwistosc <- length(dane$Niedokrwistosc[dane$Niedokrwistosc == 1])
dzieci_alergia  <- length(dane$Alergia[dane$Alergia == 1])
dzieci_choroba_przewlekla <- length(dane$ChorobaPrzewl[dane$ChorobaPrzewl == 1])

# 14

PrzyrostMasy <- (dane$MasaAktualna - dane$MasaUrodz)

# PM to przyrost masy 

PM_mean <- mean(PrzyrostMasy)
PM_median <- median(PrzyrostMasy)
PM_min <- min(PrzyrostMasy)
PM_max <- max(PrzyrostMasy)

# 15

HGBKategorie <- cut(dane$HGB, breaks=c(8,10,12,14,16,18,20))
HGBKategorie_liczba <- summary(HGBKategorie)

# 16

dane_dzieci_16 <- dane[dane$Alergia == 1 & dane$HGB<12,]
liczba_dzieci <- nrow(dane_dzieci_16)
RBC_dzieci16 <- summary(dane_dzieci_16$RBC)

# 17



# 18

his_HGB <- ggplot(dane,aes(x=dane$HGB))+
	geom_histogram()


his_RBC <- ggplot(dane,aes(x=dane$RBC))+
	geom_histogram()

# 19

w_rozrzutu_19 <- ggplot(dane, aes(x=dane$Wiek_MIES,y=dane$HGB,color=dane$Plec))+
	geom_point(shape=dane$Alergia) + 
	stat_smooth()

# 20 


ramka_wasy_20 <- ggplot(dane,aes(x=HGBKategorie, y=dane$RBC))+
	geom_boxplot()



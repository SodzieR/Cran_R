#dane <- read.table('dane.data')
#str(dane)
#write.table(dane,'dane_pacjentow.data')

#library(foreign)
#dane2 <- read.spss('dane.sav', to.data.frame=TRUE)
#str(dane2)
#summary(dane2)
#dane3 <- read.dta('dane.dta')
#str(dane3)

#library(psych)
#describe(dane3)

---------------------------------------------------------------------------------
# pakiety_statystyczne_ggplot

library(ggplot2)
dane <- read.csv('danesoc.csv', sep=';')
str(dane)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))
wykres <- wykres + geom_point()

#lub

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(shape = 3)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(color='red')

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(shape = 3, color='green', size = 3)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, shape=plec))+
geom_point()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point(size = 3)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, shape=plec, color=praca))+
geom_point()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color = wiek))+
geom_point(size = 4)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe,color=praca, shape=plec, size = wiek))+
geom_point()

# Standard
wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size=3)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3) +
stat_smooth()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point(size = 3) +
stat_smooth()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point(size = 3) +
stat_smooth(se=FALSE)

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
scale_x_log10(breaks=c(100,120,140,160,180))

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe, color=plec))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
scale_x_log10(breaks=c(100,120,140,160,180))+
scale_y_reverse()

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
xlab('Cisnienie skurczowe [mmHg]')

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
ylab('Cisnienie rozkurczowe [mmHg]')

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
ggtitle('Cisnienie krwi')

wykres <- ggplot(dane, aes(x = cisnienie.skurczowe, y = cisnienie.rozkurczowe))+
geom_point(size = 3) +
stat_smooth(se=FALSE)+
theme_dark()

# Zadanie

#1
wykres <- ggplot(dane, aes(x = wiek, y = cisnienie.skurczowe, shape = plec,color = wyksztalcenie ,size = praca))+
geom_point()

#2
wykres <- ggplot(dane, aes(x = wiek, y = cisnienie.skurczowe))+
geom_point() +
stat_smooth(se=FALSE)

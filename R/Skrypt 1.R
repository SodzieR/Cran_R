dane <- 
	read.csv("D:/Studenci/Biostatystyka/Janucik Adrian/R/Powtórzenie z R/dane3.csv", sep=';', dec=',')

dane[1:10,]
dane$BMI
dane[,2:4]
dane[1:10,2:4]
summary(dane)

dane$frakcjaHDL <- dane$HDL / dane$cholest*100

dane$katBMI <- cut(dane$BMI, breaks=c(20,25,30,50))

bill <- dane[dane$bilirubina>1,]

#zbior danych tylko z mezczyznami
mezczyzni <- dane[dane$P³eæ=='M',]
summary(mezczyzni)

grupa2 <- dane[dane$HDL>40 & dane$HDL<50,]

#zapisuje 'grupa' tabele do pliku
write.table(grupa2, 'D:/Studenci/Biostatystyka/Janucik Adrian/R/Powtórzenie z R/grupa.csv',sep=';', dec=',')
table(dane$P³eæ)

table(dane$katBMI)

table(dane$katBMI,dane$P³eæ)

--------------------------------------------------------------------------

library(ggplot2)

w <- ggplot(dane, aes(x=cholest,y=LDL))+
	geom_point()


w <- ggplot(dane, aes(x=cholest,y=LDL))+
	geom_point(color='green', size=4, shape=3)



w <- ggplot(dane, aes(x=cholest,y=LDL,color=P³eæ))+
	geom_point(size=4, shape=2)

w <- ggplot(dane, aes(x=cholest,y=LDL,shape=P³eæ))+
	geom_point(size=4)

w <- ggplot(dane, aes(x=cholest,y=LDL,shape=P³eæ))+
	geom_point(size=4, shape=2) + 
	stat_smooth()


w <- ggplot(dane, aes(x=cholest,y=LDL,shape=P³eæ))+
	geom_point(size=4, shape=2) + 
	stat_smooth()


p = ggplot(dane, aes(x=Glukoza))+
	geom_histogram()

w=ggplot(dane,aes(x=Glukoza))+
	geom_histogram()

w=ggplot(dane,aes(x=Glukoza))+
	geom_histogram(color='black', fill='red')


w=ggplot(dane,aes(x=Glukoza))+
	geom_histogram(color='black', fill='red',bins=10)

w=ggplot(dane,aes(x=katBMI, y=Glukoza))+
	geom_boxplot()

w=ggplot(dane,aes(x=katBMI, y=Glukoza,fill=katBMI))+
	geom_boxplot()



















































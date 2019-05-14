library(ggplot2)
library(foreign)
dane<- read.dta('D:/Studenci/Biostatystyka/Janucik Adrian/R/dane2.dta')

w <- ggplot(dane, aes(x=BMI, y=Cholesterol))+
geom_point()

w <- ggplot(dane, aes(x=BMI, y=Cholesterol, color=Nadcisnienie))+
geom_point()

w <- ggplot(dane, aes(x=BMI, y=Cholesterol, color=Nadcisnienie, size=Glukoza))+
geom_point()

w <- ggplot(dane, aes(x=BMI, y=Cholesterol))+
geom_path()

dane2 <- data.frame('a'=c(2,7,3,5),'b'=c(4,1,0,6),'c'=c(3,1,5,8),'d'=c(3,1,5,8),'e'=c(1,2,3,4))

w <-ggplot(dane2, aes(x=a, y=b))+
geom_path()

w <- ggplot(dane2, aes(x=a, y=b)) +
geom_line()

w <- ggplot(dane, aes(x=BMI, y=Cholesterol))+
geom_line()

w <- ggplot(dane, aes(x=BMI, y=Cholesterol, color=Nadcisnienie))+
geom_line()

w <- ggplot(dane2, aes(x=e,ymax=a, ymin=b))+
geom_ribbon()

w <- ggplot(dane2, aes(x=a,y=b,xend=c,yend=d))+
geom_segment()

dane3=data.frame(x1=c(1,3,1,5,4),x2=c(2,4,3,6,6),y1=c(1,1,4,1,3),y2=c(2,2,5,3,5), t=c('a','a','a','b','b'),r=1:5)

w <- ggplot(dane3,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2))+
geom_rect()

w <- ggplot(dane3,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=t))+
geom_rect(color='black')


w <- ggplot(dane3,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=t,x=x1,y=y1,label=r))+
geom_rect(color='black') +
geom_text()

w <- ggplot(dane3,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=t,x=(x1+x2)/2,y=(y1+y2)/2,label=r))+
geom_rect(color='black') +
geom_text()

w <- ggplot(dane3)+
geom_rect(aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=t),color='black') +
geom_text(aes(x=(x1+x2)/2,y=(y1+y2)/2,label=r))

w <- ggplot(dane2, aes(x=a,y=b)) + geom_polygon()

w <- ggplot(dane, aes(Grupa)) +geom_bar()

w<- ggplot(dane, aes(Grupa, fill=Plec)) +geom_bar()

w<- ggplot(dane, aes(Grupa, fill=Plec)) +geom_bar(position='fill')

w<- ggplot(dane, aes(Grupa, fill=Plec)) +geom_bar(position='dodge')

w<- ggplot(dane, aes(Grupa, fill=Plec)) +geom_bar(position='dodge') +
coord_flip()

w<- ggplot(dane,aes(Glukoza)) + geom_histogram(fill='green',color='black')

w<- ggplot(dane,aes(Glukoza)) + geom_density()

w<- ggplot(dane,aes(Glukoza)) + geom_dotplot()

w<- ggplot(dane,aes(Glukoza)) + geom_freqpoly()

w<- ggplot(dane,aes(x=BMI, y=Cholesterol, color=Nadcisnienie)) + geom_point() + geom_rug() + geom_smooth()

w<- ggplot(dane,aes(x=BMI, y=Cholesterol)) + geom_bin2d()

w<- ggplot(dane,aes(x=BMI, y=Cholesterol)) + geom_density2d()

w<- ggplot(dane,aes(x=Grupa, y=Cholesterol)) + geom_count()

w<- ggplot(dane,aes(x=Grupa, y=Cholesterol)) + geom_violin()

w<- ggplot(dane,aes(x=Grupa, y=Cholesterol)) + geom_jitter()

dane4 <- data.frame('Rok'=2010:2019, 'Zgony'=c(145,231,179,190,212,247,268,242,251,228))

w<- ggplot(dane4,aes(Rok, Zgony)) + geom_line()

wr <- ggplot(dane,aes(x=BMI, y=Cholesterol)) + geom_point(color='red')
wp <- ggplot(dane,aes(x=Grupa, y=Kreatynina, fill=Grupa)) + geom_boxplot()
wp + scale_fill_manual(values=c('green','blue','red')) 
wp + scale_fill_brewer(palette='Setl')
wr + scale_color_brewer(palette='Spectral')

wr <- ggplot(dane, aes(x=BMI, y=Kreatynina , color=Glukoza)) + geom_point()
wr + scale_color_gradient(low='blue',high='red')
wr+scale_color_gradientn(colors=c('blue','yellow','green'))
wr+scale_color_gradientn(colors=rainbow(5))

wh <- ggplot(dane,aes(x= Kreatynina, fill=..count..)) + geom_histogram()
wh + scale_fill_gradient(low='blue',high='red')
wh + scale_fill_gradientn(colors=rainbow(5))
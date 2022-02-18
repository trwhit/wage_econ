# this  will be dealing withh covered emploiyment as a metric that can be better quantified
rm(list=ls())
library(readxl)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data.xlsx")
attach(data)
library(foreign)
View(data)
options(scipen=100)
gce<-100*diff(log(ce))
plot(date[2:260],gce)
plot(ce,(unem_real))
cereg<-lm(unem_real~ce*COVID)
summary(cereg)
data$ur<-unem_real

plot(date, avg_cc, type='l')
plot(date, ce, type='l')

ccreg<-lm(log(ce)~log(avg_cc))
summary(ccreg)

plot(date,log(avg_cc),type='l',col='black',lwd=1) 
lines(date,log(ce),type='l', col='red',lwd=1)


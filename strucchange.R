#structural break test
rm(list=ls())
library(zoo)
library("strucchange")
library(tseries)
library(readxl)
strucc <- read_excel("~/Desktop/2021 Unemployment Capstone/Data/strucchange_wage.xlsx")
attach(strucc)

#total national private earnings
strucc1<-strucc['nat_awe']
strucc1<-ts(strucc1,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc1,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo1<- efp(strucc1~time,type='OLS-CUSUM',data=strucc)
plot(hypo1)
f1<- Fstats(strucc1~time)
plot(f1)
#no evidence of structural break

#manufacturing
strucc2<-strucc['awe_m']
strucc2<-ts(strucc2,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc2,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo2<- efp(strucc2~time,type='OLS-CUSUM',data=strucc)
plot(hypo2)
f2<- Fstats(strucc2~time)
plot(f2)
#no major evidence of structural break

#daycare workers
strucc3<-strucc['awe_dc']
strucc3<-ts(strucc3,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc3,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo3<- efp(strucc3~time,type='OLS-CUSUM',data=strucc)
plot(hypo3)
f3<- Fstats(strucc3~time)
plot(f3)
#significant evidence of structural break

#education and health
strucc4<-strucc['awe_eh']
strucc4<-ts(strucc4,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc4,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo4<- efp(strucc4~time,type='OLS-CUSUM',data=strucc)
plot(hypo4)
f4<- Fstats(strucc4~time)
plot(f4)
#

#social assistance
strucc5<-strucc['awe_sa']
strucc5<-ts(strucc5,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc5,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo5<- efp(strucc5~time,type='OLS-CUSUM',data=strucc)
plot(hypo5)
f5<- Fstats(strucc5~time)
plot(f5)
#

#leisure and hospitality
strucc6<-strucc['awe_lh']
strucc6<-ts(strucc6,start=c(2006,3),end=c(2021,9),frequency=12)
time <- c(1:187)
plot(strucc6,type="l",las=1,xaxs='i',yaxs='i',xlab='',ylab='',col='blue')
hypo6<- efp(strucc6~time,type='OLS-CUSUM',data=strucc)
plot(hypo6)
f6<- Fstats(strucc6~time)
plot(f6)
#
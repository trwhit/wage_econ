# this document exists solely to find a model connecting daycare and social assistance data
# to the unemployment rate or labor force participation rate or overal employment level

rm(list=ls())
library(foreign)
library(readxl)
library(fpp2)
library(tseries)
library(ggplot2)
library(forecast)
library(quantmod)
library(TTR)
library(plm)
library(stargazer)
dataw<-read_excel("~/Desktop/2021 Unemployment Capstone/data.wage.xlsx")
attach(dataw)
View(dataw)

# objective : create model that connects daycare shortage data to unemployment data

sma<- data.frame(c(date),
                 c(TTR::SMA(awh, n = 7)),
                 c(TTR::SMA(awh_dc, n = 7)),
                 c(TTR::SMA(awh_lh, n = 7)),
                 c(TTR::SMA(awh_eh, n = 7)),
                 c(TTR::SMA(awh_sa, n = 7)),
                 c(TTR::SMA(awh_manu, n = 7)),
                 c(TTR::SMA(lfpr, n = 7)),
                 c(TTR::SMA(ur, n = 7)),
                 covid
)
index<- c(1:10)
colnames<- c("date","sma_awh","sma_dc","sma_lh","sma_eh","sma_sa","sma_manu","sma_lfpr","ur","cov")
names(sma)[index]<-colnames
sma <- na.omit(sma)
presma_ts<- ts(sma[1:168,],start=c(2006,9),end=c(2020,2),freq=12)
sma_ts<- ts(sma,start=c(2006,9),frequency=12)
plot(sma_ts)

# i think that this is actually a good regression. test this
dcreg<- lm(lfpr~(awh)*covid,data=dataw)
summary(dcreg)






rm(list=ls())
library(foreign)
library(readxl)
library(fpp2)
library(tseries)
library(ggplot2)
library(forecast)
library(quantmod)
library(TTR)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data.wage.xlsx")
attach(data)
View(data)

sa<- data.frame(date,awh_sa1,manu_sa,eh_sa,sa_sa,dc_sa,lh_sa,lfpr)
pre_sa_ts<- ts(sa[1:168,],start=c(2006,3),end=c(2020,2),freq=12)
sa_ts<- ts(sa,start=c(2006,3),frequency=12)
plot(sa_ts, 
     main= "Average Weekly Hours Over Time for Different Industries")

awh_pre_ts <- ts(sa$awh_sa1,start=c(2006,3),end=c(2020,2),frequency=12)
awh_ts     <- ts(sa$awh_sa1,start=c(2006,3),frequency=12)

hw       <- HoltWinters(awh_pre_ts, beta = FALSE, gamma = TRUE) 
awh_fc   <- forecast(hw, h = 20) 

autoplot(awh_fc, PI= FALSE, 
         xlab= "Date", 
         ylab= "Total Private Average Weekly Hours", 
         main= "Real Average Weekly Hours VS. Forecasted from Feb. 2020") +
  autolayer(awh_ts, series= "Raw Data") +
  autolayer(awh_fc$mean, series= "Forecast")

print(awh_fc$mean)
change<- (sa$awh_sa1[168:188]-awh_fc$mean)/awh_fc$mean

change<- data.frame(sa$awh_sa1[169:188],awh_fc$mean)

names(change)[1]<- "awh"
names(change)[2]<- "fc"
print(change)

change$growth <- (change$awh-change$fc)/change$fc
print(change)
change$date <- data$date[169:188]

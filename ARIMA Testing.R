#basic forecasting for modeing
rm(list=ls())
library(foreign)
library(readxl)
library(forecast)
library(tseries)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data.xlsx")
attach(data)
class(lfprts)
lfprts<-ts(lfpr[c(0:240)],start=0,frequency=12)
lfprts
plot(date[c(0:240)],lfprts,type='l')
acf(lfprts)
pacf(lfprts)
adf.test(lfprts)

#creating arima model
lfprmodel<-auto.arima(lfprts,ic='aic',trace=TRUE)
#testing stationarity
acf(ts(lfprmodel$residuals))
pacf(ts(lfprmodel$residuals))

lfprfc<-forecast(lfprmodel,level=c(95),h=20)
lfprfc
plot(lfprfc)
lines(lfpr, type='l')

#learn about this type of test
Box.test(lfprfc$residuals,lag=5,type='Ljung-Box')

library(ffp2)

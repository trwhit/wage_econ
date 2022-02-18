rm(list=ls())
library(foreign)
library(readxl)
library(tseries)
library(forecast)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data.xlsx")
attach(data)
options(scipen=100)

#employment by industry - unemployed against job openings
data$manu_hire<-manu_hire*1000
data$manu_open<-manu_open*1000
data$eh_hire<-eh_hire*1000
data$eh_open<-eh_open*1000
data$lh_hire<-lh_hire*1000
data$lh_open<-lh_open*1000

plot(date, lh_open,type='l',col='black',lwd=1, xlab = "Date",
     ylab = "Job Openings", main = "Job Openings Over Time")
lines(date,eh_open,type='l',col='blue',lwd=1)
lines(date,manu_open,type='l',col='red',lwd=1)
lines(date,open,type='l',col='green',lwd=1)
legend("topleft", legend = c("Lesiure and Hospitality",
                             "Education and Health", 
                             "Manufacturing"),
                              text.col = c("black","blue","red"))

open_reg<- lm(lfpr~log(open))
summary(open_reg)

data$open_ratio<-unem_lev/open
open_ratio<-unem_lev/open
plot(date,open_ratio,type='l',col='black',lwd=1)
print(open_ratio)

plot(date,lfpr, type="l", col="black", lwd=1, 
     xlab="Date", 
     ylab="Labor Force Participation Rate", 
     main = "Labor Force Participation Rate Since 2000")




#wage statistics - prove main break from previous average wage trend
dataw <- read_excel("~/Desktop/2021 Unemployment Capstone/data.wage.xlsx")
attach(dataw)
colnames(dataw)
View(dataw)
gawh<-100*diff(log(awh))
gdc<-100*diff(log(dc))
gsa<-100*diff(log(sa))
geh<-100*diff(log(eh))
glh<-100*diff(log(lh))
gmanu<-100*diff(log(manu))
glfpr<-100*diff(log(lfpr))

datawg<-data.frame(date[-c(1,1)],covid[-c(1,1)],gdc,gsa,geh,glh,gmanu,glfpr)
colnames(datawg)

plot(date,lfpr,type='l',col='black')
  lines(date,awh,col='red',lwd=.1)
# using covered employment as a metric
  
#main regression
reg1<- lm(lfpr~awh, data=dataw)
print(summary(reg1))
plot(reg1$residuals) 
xlab = "Residuals"
ylab ="Index"
Main = "Plot of Regression Residuals"


plot(clfpl,awe)

print(reg1$residuals)
abline(lm(lfpr~awh,data=dataw))

if(awh>awh2){dataw$dummy=1}else{dataw$dummy=0}

# Granger causality test
library(lmtest)
# Null: AWH does not granger cause LFPR
# Alt: AWH does granger cause LFPR
grangertest(lfpr~awh, order=2, data=dataw)  
# reject null, AWH has clear impact on changes in LFPR

# Inverse test
# Null: gr
# Alt: AWH does granger cause LFPR
grangertest(awh~lfpr, order=2, data=dataw)

acf(reg1$residuals)
pacf(reg1$residuals)
adf.test(reg1$residuals)

df<- data.frame(date,lfpr,awh)
ts<- ts(df$lfpr, start=c(2006,3),freq=12)
print(head(ts))
mod1<-auto.arima(ts,ic='aic',trace=TRUE)
acf(ts(mod1$residuals))
pacf(ts(mod1$residuals))

df<- data.frame(date,lfpr,awh)
ts1<- ts(df$awh, start=c(2006,3),freq=12)
print(head(ts1))
mod2<-auto.arima(ts1,ic='aic',trace=TRUE)
acf(ts(mod2$residuals))
pacf(ts(mod2$residuals))

library(car)
linearHypothesis(reg1,c("awh=-1"))

bgtest(reg1,order = 2)
# Reject null hypothesis, we have evidence of serial correlation

durbinWatsonTest(reg1)
# Evidence of positive autocorrelation

bptest(reg1)
# Fail to reject null hypothesis, concludes that the errors are equal

resettest(reg1,power = 2:3)

Box.test(awh, lag = 1, type = "Ljung-Box")

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

change<- data.frame(sa$awh_sa1[169:188],awh_fc$mean)

names(change)[1]<- "awh"
names(change)[2]<- "fc"
print(change)

change$growth <- (change$awh-change$fc)/change$fc
change$date <- dataw$date[169:188]
print(change)

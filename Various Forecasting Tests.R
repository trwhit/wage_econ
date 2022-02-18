# forecasting test 2
rm(list=ls())
library(foreign)
library(readxl)
library(fpp2)
library(tseries)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data.xlsx")
attach(data)
y<-ts(data$lfpr,start=c(2000,1), end=c(2020,1),frequency=12)
autoplot(y)+
  ggtitle("lfpr over time") +
  ylab("lfpr")

#get rid of data trend
# take first difference of data to remove the trend
# this looks at the change of data over periods

dy <- diff(y)
autoplot(dy)+
  ggtitle("difference lfpr over time") +
  ylab("change lfpr")

# seasonaility in the data - do fluctuations happen in the same month every year?
# appears stationary except for april 2020
# exclude this information from the analysis?

ggseasonplot(dy) + ggtitle("seasonal plot change") + ylab("change ratio")

# look at another seasonal plot - seasonal subseries plot
ggsubseriesplot(dy)

# benchmark forecast method
# mean of data and every month going forward will be the same as the mean going foward

# use seasonal naive method - y_t = y_(t-s) + e_t

fit <-snaive(dy)
print(summary(fit))
checkresiduals(fit)

#different model
# guide to how strong the forecast is is the standard deviation of the residuals
# and how well the acf fits

# exponential smoothing model
# class of tsf - ets method. R will compute the best possible smoothing model
fit_ets <- ets(y)
print(summary(fit_ets))
checkresiduals(fit)

#arima modeling
fit_arima<- auto.arima(y,d=1,D=1,stepwise=FALSE,approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#forecasting
#arima model fits the best so we will use that to forecast
fcst<-forecast(fit_arima, h= 20)
autoplot(fcst)
print(summary(fcst))


# forecast for wage data - awh_lh, awh_dc, awh_sa, awh_eh
# 
fclh<-ts(data$awh_lh,end=c(2020,1),frequency=12)
fclh2<-ts(data$awh_lh,end=c(2021,9),frequency=12)
autoplot(fclh)+
  ggtitle("awh_lh over time") +
  ylab("awh_lh")

#get rid of data trend
# take first difference of data to remove the trend
# this looks at the change of data over periods

dfclh <- diff(fclh)
autoplot(dfclh)+
  ggtitle("difference awh_lh over time") +
  ylab("change awh_lh")

# seasonaility in the data - do fluctuations happen in the same month every year?
# appears stationary except for april 2020
# exclude this information from the analysis?

ggseasonplot(dfclh) + ggtitle("seasonal plot change") + ylab("change ratio")
# no aparent seasonality

# look at another seasonal plot - seasonal subseries plot
ggsubseriesplot(dfclh)

# benchmark forecast method

fit_lh <-snaive(dfclh)
print(summary(fit_lh))
checkresiduals(fit_lh)

# exponential smoothing model
fit_fclh <- ets(fclh)
print(summary(fit_fclh))
checkresiduals(fit_fclh)

#arima modeling
fit_arima_fclh<- auto.arima(fclh,stepwise=FALSE,approximation = FALSE, trace = TRUE)
print(summary(fit_arima_fclh))
checkresiduals(fit_arima_fclh)

#actual forecast plot
fcst_lh<-forecast(fit_fclh, h= 20)
autoplot(fcst_lh, ci = -1, include=60)+
autolayer(fclh2, include =60)

plot(fcst_lh, color ='b', ci=0,include=60)
lines(fclh2,include=60, col ='red')

# holtwinters test
hw<- HoltWinters(fclh,beta=TRUE,gamma=FALSE)
hwfc<-forecast(hw,h=20)
autoplot(hwfc) +
  autolayer(fclh2, series="Data") +
  autolayer(hwfc$mean, series="Forecasts")


y<-ts(data$lfpr,start=c(2000,1), end=c(2020,1),frequency=12)
autoplot(y)+
  ggtitle("lfpr over time") +
  ylab("lfpr")

#get rid of data trend
# take first difference of data to remove the trend
# this looks at the change of data over periods

dy <- diff(y)
autoplot(dy)+
  ggtitle("difference lfpr over time") +
  ylab("change lfpr")

# seasonaility in the data - do fluctuations happen in the same month every year?
# appears stationary except for april 2020
# exclude this information from the analysis?

ggseasonplot(dy) + ggtitle("seasonal plot change") + ylab("change ratio")

# look at another seasonal plot - seasonal subseries plot
ggsubseriesplot(dy)

# benchmark forecast method
# mean of data and every month going forward will be the same as the mean going foward

# use seasonal naive method - y_t = y_(t-s) + e_t

fit <-snaive(dy)
print(summary(fit))
checkresiduals(fit)

#different model
# guide to how strong the forecast is is the standard deviation of the residuals
# and how well the acf fits

# exponential smoothing model
# class of tsf - ets method. R will compute the best possible smoothing model
fit_ets <- ets(y)
print(summary(fit_ets))
checkresiduals(fit)

#arima modeling
fit_arima<- auto.arima(y,d=1,D=1,stepwise=FALSE,approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#forecasting
#arima model fits the best so we will use that to forecast
fcst<-forecast(fit_arima, h= 20)
autoplot(fcst)
print(summary(fcst))

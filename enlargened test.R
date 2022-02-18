# employment, earnings, and bread

# here we are looking at the price of bread over time as a baseline

rm(list=ls())
dataw <- read_excel("~/Desktop/2021 Unemployment Capstone/data.wage.xlsx")
attach(dataw)

plot(log(bread),log(awe))

plot(clfpl,log(awe))
plot(clfpl_sa,log(bread))

#change statistics


plot(date,log(awe),type='l')
plot(date,bread,type='l')


# growth formula
# (new-old)/old



library(dplyr)
library(tidyverse)



reg1 <- lm(log(clfpl)~bread+awe)
summary(reg1)

# reasoning that i want to test
# 1. if the price of bread increases (which everyone buys), people have to work more
# 2. if the price of bread increases at a rate disproportionate to the rate at which awe 
# increases, then more people should have to enter the labor force than when it doesnt
# 3. this idea should correspond more strongly to the regression on awh
# 4. create the dummy variable when the price of bread increases at a rate x higher than awe
# 5. add this dummy variable back into the original regression

growth <- data.frame(bread,awe,awh,lfpr,clfpl)

growth$change <- (log(growth$awe)-log(growth$bread))/log(growth$bread)
growth$avg <- mean(growth$change)
growth$dummy<- ifelse(growth$change>growth$avg,1,0)
print(growth)



library(tseries)
ts<- ts(growth,start=c(2006,3),freq=12)
plot(ts)


reg2<- lm(log(clfpl)~dummy*(awh+awe+bread+change),data=growth)
summary(reg2)


reg3<-lm(lfpr~dummy*change,data=growth)
summary(reg3)

reg4<- lm(lfpr~bread)
summary(reg4)


growth$change2 <- (lead(growth$bread)-growth$bread)/growth$bread
View(growth)
growth<- growth[1:187,]
growth$avg2 <- mean(growth$change2)
growth$dummy2<- ifelse(growth$change2>growth$avg2,1,0)
print(growth)
View(growth)

# gas prices, grain prices




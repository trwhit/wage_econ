rm(list=ls())
library(foreign)
library(readxl)
data <- read_excel("~/Desktop/2021 Unemployment Capstone/data/awh_all.xlsx")
attach(data)
View(data)
colnames(data)
head(data)

datareg<-lm(lfpr~covid*tp+gp+psp+ml+c+manu+dg+ndg+ttu+wt+rt+tw+u+i+fa+pbs+ehs+lh+os)
summary(datareg)

library(car)
bgtest(datareg,order=2)



gtp<-100*diff(log(tp))
ggp<-100*diff(log(gp))
gpsp<-100*diff(log(psp))
gml<-100*diff(log(ml))
gc<-100*diff(log(c))
gmanu<-100*diff(log(manu))
gdg<-100*diff(log(dg))
gndg<-100*diff(log(ndg))
gttu<-100*diff(log(ttu))
gwt<-100*diff(log(wt))
grt<-100*diff(log(rt))
gtw<-100*diff(log(tw))
gu<-100*diff(log(u))
gi<-100*diff(log(i))
gfa<-100*diff(log(fa))
gpbs<-100*diff(log(pbs))
gehs<-100*diff(log(ehs))
glh<-100*diff(log(lh))
gos<-100*diff(log(os))
glfpr<-100*diff(log(lfpr))

gdata<-data.frame(date[-c(1,1)],gtp,ggp,gpsp,gml,gc,gmanu,gdg,gndg,gttu,gwt,grt,gtw,gu,gi,gfa,gpbs,gehs,glh,gos,glfpr)
colnames(gdata)
plot.ts(gdata[c(1:10)])
plot.ts(gdata[c(11:20)])
plot.ts(gdata[c(21)])

greg<-lm(glfpr~covid[-c(1,1)]*gtp+ggp+gpsp+gml+gc+gmanu+gdg+gndg+gttu+gwt+grt+gtw+gu+gi+gfa+gpbs+gehs+glh+gos)
summary(greg)

data1 <- read_excel("~/Desktop/2021 Unemployment Capstone/data/awe_all.xlsx")
head(data1)

#growth overall lfpr rate affected by total private average weekly hours and growth rate of tp
lfpr1<- lfpr[-c(1,1)]
tp1<- tp[-c(1,1)]
data1reg<-lm(lfpr1~tp1+gtp)
summary(data1reg)
print(data1reg)

bgtest(data1reg,order=2)

plot(data1reg$residuals)
plot(lfpr,tp)
lines(data1reg$residals)

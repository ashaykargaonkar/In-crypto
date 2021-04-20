setwd("D:/_College/6th Quarter/DSC 465/Project/Milestone 4")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(zoo)
library(fBasics)
library(xts)
library(lubridate)
library(Hmisc)
library(fUnitRoots)
library(tidyquant)
library(fUnitRoots)

source("Backtest.R")
source("eacf.R")
source("backtestGarch.R")

library(dynlm)
library(fGarch)
library(forecast)
library(fGarch)
library(rugarch)
library(tseries)
library(lmtest)


data = read.csv("LTC_USD_2020-02-16_2021-02-15-CoinDesk.csv")
head(data)
tail(data)


data$Date = as.Date(data$Date,format="%m/%d/%Y")

dataOrig = ts(data$X24h.High..USD., start = c(2020, 02), frequency = 365.25)
autoplot(dataOrig)



#below both tests say that the series is non-stationary, therefore we have to try to convert it to stationary.
adf.test(dataOrig)
kpss.test(dataOrig)

kpss.test(lag(dataOrig))

diff1 = diff(dataOrig)

#still the series is non-staionary
adf.test(diff1)
kpss.test(diff1)

#after taking diff 2 times, the series becomes stationary and useful for further analysis
diff2 = diff(diff1)
kpss.test(diff2)
adf.test(diff2)

dataTS = diff2


autoplot(dataTS)

#There are multiple significant lag values. Maybe have to use GARCH 
Acf(dataTS, lag.max = 20)

pacf(dataTS, lag.max = 20)

#by looking at eacf, it looks like there might be a (1, 2) ARMA model
eacf(dataTS)


autoarima = auto.arima(dataTS) 
autoarima
coeftest(autoarima)

#looking at the coeftest ar1, ar2 and ma1 have high significant values:
#good estimate std values, less p value, error is also less.
#less sigma-square estimate value which is also good.

#AUTOARIMA gives ARIMA model of order (2, 0, 1) as the best fit.


#trying different orders of arima

arima1 = Arima(dataTS, order = c(2,0,1))  #best model, same as the autoarima
arima1
coeftest(arima1)

arima2 = Arima(dataTS, order = c(2,0,2))
arima2
coeftest(arima2)

#ma2 order is not significant

arima3 = Arima(dataTS, order = c(1,0,1))
arima3
coeftest(arima3)

arima4 = Arima(dataTS, order = c(3,0,1))
arima4
coeftest(arima1)


#by doing some manual testing we can say that model suggested by auto-arima model is the best fit.


b1 = backtest(autoarima, dataTS, h=1, orig = 0.9*length(dataTS))
b1


########arch efftects
Acf(arima1$residuals,lag.max=15)
Acf(arima1$residuals^2,lag.max=15)
###############Arch model
gfit = garchFit(~ arma(2,2) + garch(1,1), data = dataTS, trace = F)
gfit


predict = predict(gfit,n.ahead =5)
predict


testLen = floor(length(dataTS) * .95)

bgarch = backtestGarch(gfit, dataTS, testLen, 1)
bgarch

barima = backtest(autoarima, dataTS, testLen, 1)
barima
library(quantmod)
library(xts)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(rugarch)
###AAPL daily price 
getSymbols("AAPL",from= "2021-01-01", to ="2024-08-11")
chartSeries(AAPL)
tail(AAPL)

###Creating a Daily return series
return<-CalculateReturns(AAPL$AAPL.Adjusted)
return
return<-na.omit(return)
return
return<-return[-1]

hist(return)#plotin' Histogram to see the distribution of the rtn series
chart.Histogram(return,
                methods=c('add.density','add.normal'),
                colorset=c('black','green','red'))
#Note: One can clearly see in the AAPL.Adjusted that the distribution of the rtn series has Positive Kurtosis [Leptokurtic]
#This knowledge of distribution, That it has Fatter tail will help us to determine whether we shall choose distribution assumption for error term 
#Normally distributed or some other skewed distribution.
chartSeries(return)
#By looking at the log rtn series plot it seems to have 0 mean and within a constant variance.


#By observing Pacf and Acf plot of rtn, we conclude that it has no serial correlation in it. 
library(fpp2)
Pacf(return,lag.max = 12,plot=T)
tsdisplay(return)
tsdisplay(return^2)
#There is a correlation in return^2 series, Let us try to capture the volatility by fitting appropriate Garch model.
#Annualized volatility
chart.RollingPerformance(R=return["2021::2024"],
                         width = 252,
                         FUN="sd.annualized",
                         main="Apple's yearly rolling volatility")
#1.sGARCH model with constant mean
s<-ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=TRUE),
              variance.model = list(model="apARCH",garchOrder=c(1,0)),
              distribution.model = 'sstd')
m<-ugarchfit(data = return,spec = s)
m
plot(m,which="all")
f<-ugarchforecast(fitORspec = m,n.ahead = 20)
plot(fitted(f))
plot(sigma(f))
#it is a constant mean model. so, Basically our prediction for mean forecasting is constant
#plot(sigma()) is for variability. it expect that the volatility for next 20 will decrease.

#Application example - portfolio allocation
v<-sqrt(252)*sigma(m)
w<-0.1/v
plot(merge(v,w),multi.panel=T)
# Clearly it can be seen that whenever the volatility increases the value of w (weigth assigned to risky assets) decreases. 
tail(w)
# the value of w on 2024-08-09 is  32.9%, let's say I have 1million $ to invest. Now How much shall i invest in the market based on the volatility of the stock ( how risky it would be at 10% risk of volatility)

1000000*0.3842864
#   $328976.2 should be invested 
   1000000-384286.4
 # one can put it into risk free assets such as bank.(FD)
sfinal<-s
setfixed(sfinal)<-as.list(coef(m))
f2021<-ugarchforecast(data=return["/2021-01"],
                      fitORspec = sfinal,
                      n.ahead = 252)
f2024<-ugarchforecast(data=return["/2024-09"],
                      fitORspec = sfinal,
                      n.ahead = 252)
par(mfrow=c(1,1))
plot(sigma(f2021))
plot(sigma(f2024))
sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*20,
                  rseed = 123)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
p <- 291.52*apply(fitted(sim), 2, 'cumsum') + 291.52
matplot(p, type = "l", lwd = 3)
p

#####################################################################
################Time Series analysis#########################################
################################################################

#Description
names(WAGE)
#W=log(wage); OBS=time; P=log(Price index); L=total labour
#Identifying GDP as a time series data
W.GDP.ts=ts(WAGE$GDP,start = c(min(WAGE$OBS.),1))
#Checking data
summary(W.GDP.ts)
summary(WAGE)
class(W.GDP.ts)
start(W.GDP.ts)
frequency(W.GDP.ts)
plot(WAGE$GDP,type = 'l')
#Plot of log(GDP) time series
plot.ts(W.GDP.ts,ylab="Log(GDP)",col="blue",
        main='Evolution of GDP over the years')
library(ggplot2)
ggplot(data=WAGE, aes(x=OBS.,y=W.GDP.ts,group=1))+
  geom_line()+ylab("Log(GDP)")+xlab("Years")+
  ggtitle("Evolution of Logarithmic transformed GDP with time")

#Autocorrelation plot of log(GDP)
acf(W.GDP.ts, main="Correlogram for Wage time series")

#Test for stationarity
library(CADFtest)
max.lag<-round(sqrt(length(W.GDP.ts)))
#CADFtest(W.GDP.ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)
adf.test(W.GDP.ts,alternative = "stationary",k=0)
#Plot of differenced time series
plot.ts(diff(W.GDP.ts),ylab="Log(GDP_t)-Log(GDP_t-1)",col="blue",
        main='Evolution of increasing Logarithmic transformed GDP over the years')
monthplot(diff(W.GDP.ts))

#Test for stationarity in differenced time series
#max.lag<-round(sqrt(length(diff(W.GDP.ts))))
#CADFtest(diff(W.GDP.ts), type= "drift", criterion= "BIC", max.lag.y=max.lag)
adf.test(diff(W.GDP.ts),alternative = "stationary",k=0)

#####Univariate Analysis#######
par(mfrow=c(1,2))
acf(diff(W.GDP.ts), main="AC with intergration order=1")

pacf(diff(W.GDP.ts),main="PAC with intergration order=1")
#library(forecast)
#seasonplot(diff(WAGE$GDP),frequency=4, start=c(1900))
library(forecast)
model1=Arima(W.GDP.ts, order = c(1,1,1))
model1
summary(model1)
acf(model1$residuals,main="residual correlogram for ARIMA(1,1,1)")
Box.test(model1$residuals)

model2=Arima(W.GDP.ts, order = c(0,1,1))
summary(model2)
acf(model2$residuals,main="residual correlogram for ARIMA(0,1,1)")
Box.test(model2$residuals)

model3=Arima(W.GDP.ts, order = c(1,1,0))
summary(model3)
acf(model3$residuals,main="residual correlogram for ARIMA(1,1,0)")
Box.test(model3$residuals)

model4=Arima(W.GDP.ts, order = c(2,1,2))
summary(model4)
acf(model4$residuals,main="residual correlogram for ARIMA(2,1,2)")
Box.test(model4$residuals)

model5=Arima(W.GDP.ts, order = c(1,1,2))
summary(model5)
acf(model5$residuals,main="residual correlogram for ARIMA(1,1,2)")
Box.test(model5$residuals)

model6=Arima(W.GDP.ts, order = c(2,1,1),method = "ML")
summary(model6)
acf(model6$residuals,main="residual correlogram for ARIMA(2,1,1)")
Box.test(model6$residuals)

auto.arima(W.GDP.ts)
#Forcast
library(forecast)
uni.forc=predict(model6,n.ahead = 20)
names(uni.forc)
uni.forc$pred[60]
ts.plot(uni.forc$pred)
plot(forecast(model6,h=10),
     main="Original time series with forcast of h=10",
     xlab="Year",ylab="Log(GDP)")
lines(model4$fitted,col="red")
legend(1970, 11, legend = c("Original", "Fitted", "Forecast"),
       col = c("black", "red", "blue"), lty=1, cex=0.8)

#####Multivariate Analysis########
#Plots of time series
names(WAGE)
W.employ.ts=ts(WAGE$E,start = c(min(WAGE$OBS.),1))
ts.plot(W.employ.ts,W.GDP.ts)
par(mfrow=c(1,2))
plot.ts(W.GDP.ts,ylab="Log(GDP) & Log(Employment)",col="blue",
        main='Time series of GDP and Employment',
        ylim=c(9.2,12.7))
lines(W.employ.ts,ylab="Log(Employment)",col="green",
        main='Evolution of Employment over the years')
legend(1950, 11, legend = c("Log(GDP)l", "Log(Employment"),
       col = c("blue", "green"), lty=1, cex=0.8)

#plot.ts(W.wage.ts,ylab="Wage",col="blue",
 #       main='Evolution of Wage over the years')

#Cointegration test
mult.mod4 <- lm(E~GDP,data=WAGE)
CADFtest(mult.mod4$residuals)
acf(mult.mod4$residuals)

max.lag<-round(sqrt(length(W.Labor.ts)))
CADFtest(diff(W.Labor.ts), type= "drift", criterion= "BIC", max.lag.y=max.lag)

max.lag<-round(sqrt(length(W.employ.ts)))
CADFtest(diff(W.employ.ts), type= "drift", criterion= "BIC", max.lag.y=max.lag)


#Distributed lag model
adf.test(WAGE$E,alternative = "stationary",k=0)
adf.test(diff(WAGE$E),alternative = "stationary",k=0)
lag=2
T=length(diff(WAGE$E))
x=diff(WAGE$GDP)[(lag+1):T]
x.1=diff(WAGE$GDP)[(lag):(T-1)]
x.2=diff(WAGE$GDP)[(lag-1):(T-2)]
DL.mod1=lm(diff(E)[-c(1,2)]~x+x.1+x.2,data=WAGE)
summary(DL.mod1)
AIC(DL.mod1)
acf(DL.mod1$residuals)
Box.test(DL.mod1$residuals)



y=diff(WAGE$E)[lag:(T-1)]
y.1=diff(WAGE$E)[lag:(T-1)]
y.2=diff(WAGE$E)[(lag-1):(T-2)]
DL.mod2=lm(diff(GDP)[-c(1,2)]~y+y.1+y.2,data = WAGE)
summary(DL.mod2)
AIC(DL.mod2)

#Autoregressive model
AR.mod1=lm(diff(E)[-c(1,2)]~y.1+y.2+x.1+x.2,data=WAGE)
summary(AR.mod1)
AIC(AR.mod1)
BIC(AR.mod1)
AR.mod2=lm(diff(E)[-c(1,2)]~y.1+y.2,data=WAGE)
summary(AR.mod2)
anova(AR.mod1,AR.mod2)

Box.test(AR.mod1$residuals)


AR.mod1.2=lm(diff(GDP)[-c(1,2)]~x.1+x.2+y.1+y.2,data=WAGE)
AIC(AR.mod1.2)
summary(AR.mod1.2)
AR.mod2.2=lm(diff(GDP)[-c(1,2)]~x.1+x.2,data = WAGE)
anova(AR.mod1.2,AR.mod2.2)


#VAR model
install.packages("vars")
library(vars)
cGDP=diff(WAGE$GDP)
cEmp=diff(WAGE$E)
new.wage=cbind(cEmp,cGDP)
VARselect(new.wage)$criteria
VARselect(new.wage)$selection
var.mod1=VAR(new.wage,p=1,type="const")
summary(var.mod1)
VARselect(new.wage)$criteria[1]
acf(resid(var.mod1))

irf_var<-irf(var.mod1,ortho=FALSE,boot=TRUE)
irf_var
par(mfrow=c(2,2))
plot(irf_var)
names(irf_var)
vec_yrs=1:nrow(forcast$fcst$cGDP)
plot(forcast$fcst$cGDP~,
     main="Original time series with forcast of h=10",
     xlab="Year",ylab="Log(GDP)",type="l")
forcast=predict(var.mod1,n.ahead=5)
as.data.frame(forcast)
is.data.frame(forcast$fcst$cGDP)
is.matrix(forcast$fcst$cGDP)
dim(forcast$fcst$cGDP)

library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Kunlun Energy Company 

####################### Getting Data ###################################
getSymbols("HRELF", from="2013-01-01", to="2018-02-28")
class(HRELF)
str(HRELF)
head(HRELF)
tail(HRELF)
HRELF$HRELF.Close[1:5]
# dates -> index
dt=index(HRELF); length(dt); dt[1]; dt[length(dt)]
sum(is.na(HRELF)) ## NO MISSING 
######################### Plotting ######################################
#plot data
chartSeries(HRELF, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
            subset = NULL,
            show.grid = TRUE, 
            name = NULL,
            log.scale = FALSE,
            TA = 'addVo()',
            TAsep=';',
            line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("black"))

# plot a subset of the data
chartSeries(HRELF, subset='2014-01-01/2014-12-31', type='line', name ='HRELF.Close', TA=NULL)
chartSeries(HRELF, subset='last 4 months', type='bars', name ='HRELF.Close', TA=NULL)
chartSeries(HRELF, subset='last 4 months', theme="black", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
HRELF.c=HRELF$HRELF.Close
plot(HRELF.c, main='HRELF.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(HRELF.c, n = 50), on=1, col="green")

# add month end points to the chart
points(HRELF.c[endpoints(HRELF.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(HRELF)
HRELF.c=HRELF$HRELF.Close

# simple return
simple.ret = Delt(HRELF.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(HRELF.c, main='HRELF.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(HRELF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(HRELF.c, main='HRELF.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(HRELF$HRELF.Volume[-1])
df.tmp$HRELF.lrt <- log.ret
HRELF.xts.lrt = as.xts(df.tmp$HRELF.lrt)
chartSeries(HRELF.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='HRELF.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
HRELF.c.v=as.numeric(HRELF.c);
acf(HRELF.c.v)
plot(HRELF.c.v, type='l')
summary(HRELF.c.v)
log.ret.v=as.numeric(log.ret);
plot(log.ret.v, type='l')
########################### Normality tests ###################################
#need additional tools
library(moments)
library(fitdistrplus)
library(metRology)  #has implementation of scaled and shifted t distribution.
library(goftest)
## Perform the Shapiro-Wilk normality test
shapiro.test(log.ret.v)
#Anderson-Darling test 
ad.test(log.ret.v,null='pnorm')
#Cramer-Von Mises Test
cvm.test(log.ret.v, null='pnorm')  

###############################################################################
###################### Normal and log-Normal distributions ######################
mean(HRELF.c); var(HRELF.c); skewness(HRELF.c); kurtosis(HRELF.c)
mean(log.ret); var(log.ret); skewness(log.ret); kurtosis(log.ret)

plot(density(log.ret.v))
plot(density(exp(log.ret.v)))
plot(ecdf(log.ret.v),main="Empirical CDF")
fit <- fitdist(log.ret.v, "norm")
plot(fit, histo = FALSE, demp = TRUE)
gofstat(fit)
#or one by one
cdfcomp(fit, addlegend=FALSE)
denscomp(fit, addlegend=FALSE,breaks=100)
ppcomp(fit, addlegend=FALSE)
qqcomp(fit, addlegend=FALSE)          

######################## Check stationarity ##################################
### Rejecting the null hypothesis suggests that a time series is stationary
adf.test(HRELF.c);#- p=0.1741 # non-stationary
kpss.test(HRELF.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(HRELF.c, type="log")[-1]  
adf.test(log.ret);#- p<<0.01  # stationary
kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!

#chartSeries(TCEHY)
chartSeries(log.ret)          

###################### Is the TS really stationary? ####################################
log.ret.1 <- log.ret['2014-09-22/2015-09-21']
mean(log.ret.1); var(log.ret.1)
log.ret.2 <- log.ret['2016-09-22/2017-09-21']
mean(log.ret.2); var(log.ret.2)

#mu2 > 5*mu1;  var2 < 0.25*var1 
#Conclusion the TS of log-returns of BABA is NOn-Stationary!!!!!!!!!!!!!!!
###########################################################################
################# Check serial correlation vs. i.i.d. ####################
############### Dynamic trend / Seasonality #################################
#############################################################################################
acf(log.ret)  
acf(log.ret.1) 
acf(log.ret.2) 
#Conclusion - the market's regime has changed
#Box-Pierce test for serial correlation
Box.test(log.ret)   #p-value = 0.2479 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.003632Conclusion - there is serial correlation
Box.test(log.ret.2)  #p-value = 0.01707 - there is serial correlation !!!!

############################# Modelling Patterns ##############################
# Plot daily closing prices for Alibab (HRELF & AMZN)
getSymbols("HRELF", src="google")
plot(Cl(HRELF))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
HRELFrt = diff(log(Cl(HRELF)))[-1]
head(HRELFrt); tail(HRELFrt)
plot(HRELFrt)
plot(density(HRELFrt))
acf(HRELFrt, na.action=na.omit)
wilcox.test(as.numeric(HRELFrt))
adf.test(HRELFrt) #-stationary
kpss.test(HRELFrt)
Box.test(HRELFrt) 

amznrt = diff(log(Cl(AMZN)))[-1]
head(amznrt); tail(amznrt)
plot(amznrt)
plot(density(amznrt))
acf(amznrt, na.action=na.omit)
wilcox.test(as.numeric(amznrt))  #not Normal
adf.test(amznrt) #-stationary
kpss.test(amznrt)
Box.test(amznrt) #- p-value = 0.6298 - No serial correlation !!!!

# Plot the correlogram
acf(abs(HRELFrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_HRELF <- fitdist(as.numeric(HRELFrt), "norm")
gofstat(fit_HRELF)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_HRELF, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(HRELFrt))  #### not normal!!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_HRELF <- fitdist(as.numeric(HRELFrt),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(HRELFrt)),sd=sd(as.numeric(HRELFrt))))
plot(fit_HRELF, histo = FALSE, demp = TRUE)
fit_HRELF$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
HRELFrt.ma <- arima(HRELFrt, order=c(0, 0, 1))
acf(HRELFrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
HRELFrt.ma <- arima(HRELFrt, order=c(0, 0, 2))
acf(HRELFrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
HRELFfinal.aic <- Inf
HRELFfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
  HRELFcurrent.aic <- AIC(arima(HRELFrt, order=c(i, 0, j)))
  if (HRELFcurrent.aic < HRELFfinal.aic) {
    HRELFfinal.aic <- HRELFcurrent.aic
    HRELFfinal.order <- c(i, 0, j)
    HRELFfinal.arma <- arima(HRELFrt, order=HRELFfinal.order)
  }
}

# Output the results of the fit
HRELFfinal.order  #[1] 2 0 4 

# Plot the residuals of the final model
acf(resid(HRELFfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(HRELFfinal.arma), lag=20, type="Ljung-Box")

plot(density(HRELFfinal.arma$residuals))

wilcox.test(resid(HRELFfinal.arma))
adf.test(resid(HRELFfinal.arma)) #-stationary
kpss.test(resid(HRELFfinal.arma))

# Plot the correlogram
acf(abs(resid(HRELFfinal.arma)))   ## still correlated

#fit to normal
fit_HRELF <- fitdist(as.numeric(resid(HRELFfinal.arma)), "norm")
gofstat(fit_HRELF)
#summary(fit)
plot(fit_HRELF, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_HRELF <- fitdist(as.numeric(resid(HRELFfinal.arma)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(HRELFfinal.arma))),sd=sd(as.numeric(resid(HRELFfinal.arma)))))
plot(fit_HRELF, histo = FALSE, demp = TRUE)
fit_HRELF$estimate 

library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("HRELF", src="google")
HRELF.c=HRELF$HRELF.Close
log.ret = Delt(HRELF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(HRELF.c, main='HRELF.Close')
plot(log.ret, type="l", on=NA)
acf(log.ret)  #no correlation in log-returns
pacf(log.ret)
Box.test(log.ret)
#proxy for volatility
plot(abs(log.ret), type="l", on=NA)
acf(abs(log.ret))
Box.test(abs(log.ret))  # p-value = 0.0009208: NO correlation in volatility?
#########################################################
# Determine the best fitting ARIMA model
HRELFfinal.aic <- Inf
HRELFfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  HRELFcurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
  if (HRELFcurrent.aic < HRELFfinal.aic) {
    HRELFfinal.aic <- HRELFcurrent.aic
    HRELFfinal.order <- c(p, d, q)
    HRELFfinal.arima <- arima(log.ret, order=HRELFfinal.order)
  }
}

HRELFfinal.order  ## 2 0 4 

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(HRELFfinal.arima), na.action=na.omit)
Box.test(resid(HRELFfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(HRELFfinal.arima))
acf(abs(resid(HRELFfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_HRELF <- fitdist(as.numeric(resid(HRELFfinal.arima)), "norm")
plot(fit_HRELF, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_HRELF <- fitdist(as.numeric(resid(HRELFfinal.arima)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(HRELFfinal.arima))),sd=sd(as.numeric(resid(HRELFfinal.arima)))))
plot(fit_HRELF, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################

#use the forecast library to automatically determine ARIMA coefficients
fit_HRELF <- auto.arima(log.ret)
fit_HRELF ## 0 0 0 ???
fit_HRELF$arima$coef  #forecast pkg yields AR(1)
plot(fit_HRELF$residuals)
Acf(fit_HRELF$residuals)  # bad fitting
plot(forecast(HRELF.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(tencnetfinal.arima)
ft.garch <- garch(resid(HRELFfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(HRELFfinal.arima))
#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
plot(fit, histo = FALSE, demp = TRUE)

# Plot the residuals and abs residuals
acf(ft.res)
acf(abs(ft.res))  # no correlations/no information left!!!         





########## Uderlying Stock: Hong Kong Exchange ###############
### library(Quandl)
### Quandl.api_key("xPCxT4avL3ymcTUyvT7N")
### aacay <- Quandl("XHKG/00914", start_date="2013-01-01", end_date="2018-01-01")

getSymbols("1169.HK", from="2013-01-01", to="2018-02-28")

head(`1169.HK`)
tail(`1169.HK`)
sum(is.nan(`1169.HK`))  ### no missing values
# dates -> index
dt=index(`1169.HK`); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(`1169.HK`, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
            subset = NULL,
            show.grid = TRUE, 
            name = NULL,
            log.scale = FALSE,
            TA = 'addVo()',
            TAsep=';',
            line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("black"))
#######---> in HK more volitile...????

# plot a subset of the data
chartSeries(`1169.HK`, subset='2014-01-01/2014-12-31', type='line', name ='1169.HK.Close', TA=NULL)
chartSeries(`1169.HK`, subset='last 4 months', type='bars', name ='1169.HK.Close', TA=NULL)
chartSeries(`1169.HK`, subset='last 4 months', theme="white", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

###### the same steps
##### Normality
##### Stationality
##### ARMA & ARIMA
##### GARCH
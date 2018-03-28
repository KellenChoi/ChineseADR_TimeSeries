library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Kunlun Energy Company 

####################### Getting Data ###################################
getSymbols("KUNUF", from="2013-01-01", to="2018-02-28")
class(KUNUF)
str(KUNUF)
head(KUNUF)
tail(KUNUF)
KUNUF$KUNUF.Close[1:5]
# dates -> index
dt=index(KUNUF); length(dt); dt[1]; dt[length(dt)]
sum(is.na(KUNUF)) ## NO MISSING 
######################### Plotting ######################################
#plot data
chartSeries(KUNUF, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(KUNUF, subset='2014-01-01/2014-12-31', type='line', name ='KUNUF.Close', TA=NULL)
chartSeries(KUNUF, subset='last 4 months', type='bars', name ='KUNUF.Close', TA=NULL)
chartSeries(KUNUF, subset='last 4 months', theme="black", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
KUNUF.c=KUNUF$KUNUF.Close
plot(KUNUF.c, main='KUNUF.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(KUNUF.c, n = 50), on=1, col="green")

# add month end points to the chart
points(KUNUF.c[endpoints(KUNUF.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(KUNUF)
KUNUF.c=KUNUF$KUNUF.Close

# simple return
simple.ret = Delt(KUNUF.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(KUNUF.c, main='KUNUF.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(KUNUF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(KUNUF.c, main='KUNUF.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(KUNUF$KUNUF.Volume[-1])
df.tmp$KUNUF.lrt <- log.ret
KUNUF.xts.lrt = as.xts(df.tmp$KUNUF.lrt)
chartSeries(KUNUF.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='KUNUF.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
KUNUF.c.v=as.numeric(KUNUF.c);
acf(KUNUF.c.v)
plot(KUNUF.c.v, type='l')
summary(KUNUF.c.v)
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
mean(KUNUF.c); var(KUNUF.c); skewness(KUNUF.c); kurtosis(KUNUF.c)
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
adf.test(KUNUF.c);#- p=0.1741 # non-stationary
kpss.test(KUNUF.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(KUNUF.c, type="log")[-1]  
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
# Plot daily closing prices for Alibab (KUNUF & AMZN)
getSymbols("KUNUF", src="google")
plot(Cl(KUNUF))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
KUNUFrt = diff(log(Cl(KUNUF)))[-1]
head(KUNUFrt); tail(KUNUFrt)
plot(KUNUFrt)
plot(density(KUNUFrt))
acf(KUNUFrt, na.action=na.omit)
wilcox.test(as.numeric(KUNUFrt))
adf.test(KUNUFrt) #-stationary
kpss.test(KUNUFrt)
Box.test(KUNUFrt) 

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
acf(abs(KUNUFrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_KUNUF <- fitdist(as.numeric(KUNUFrt), "norm")
gofstat(fit_KUNUF)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_KUNUF, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(KUNUFrt))  #### not normal!!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_KUNUF <- fitdist(as.numeric(KUNUFrt),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(KUNUFrt)),sd=sd(as.numeric(KUNUFrt))))
plot(fit_KUNUF, histo = FALSE, demp = TRUE)
fit_KUNUF$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
KUNUFrt.ma <- arima(KUNUFrt, order=c(0, 0, 1))
acf(KUNUFrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
KUNUFrt.ma <- arima(KUNUFrt, order=c(0, 0, 2))
acf(KUNUFrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
KUNUFfinal.aic <- Inf
KUNUFfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
  KUNUFcurrent.aic <- AIC(arima(KUNUFrt, order=c(i, 0, j)))
  if (KUNUFcurrent.aic < KUNUFfinal.aic) {
    KUNUFfinal.aic <- KUNUFcurrent.aic
    KUNUFfinal.order <- c(i, 0, j)
    KUNUFfinal.arma <- arima(KUNUFrt, order=KUNUFfinal.order)
  }
}

# Output the results of the fit
KUNUFfinal.order  #[1] 2 0 4 

# Plot the residuals of the final model
acf(resid(KUNUFfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(KUNUFfinal.arma), lag=20, type="Ljung-Box")

plot(density(KUNUFfinal.arma$residuals))

wilcox.test(resid(KUNUFfinal.arma))
adf.test(resid(KUNUFfinal.arma)) #-stationary
kpss.test(resid(KUNUFfinal.arma))

# Plot the correlogram
acf(abs(resid(KUNUFfinal.arma)))   ## still correlated

#fit to normal
fit_KUNUF <- fitdist(as.numeric(resid(KUNUFfinal.arma)), "norm")
gofstat(fit_KUNUF)
#summary(fit)
plot(fit_KUNUF, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_KUNUF <- fitdist(as.numeric(resid(KUNUFfinal.arma)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(KUNUFfinal.arma))),sd=sd(as.numeric(resid(KUNUFfinal.arma)))))
plot(fit_KUNUF, histo = FALSE, demp = TRUE)
fit_KUNUF$estimate 

library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("KUNUF", src="google")
KUNUF.c=KUNUF$KUNUF.Close
log.ret = Delt(KUNUF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(KUNUF.c, main='KUNUF.Close')
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
KUNUFfinal.aic <- Inf
KUNUFfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  KUNUFcurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
  if (KUNUFcurrent.aic < KUNUFfinal.aic) {
    KUNUFfinal.aic <- KUNUFcurrent.aic
    KUNUFfinal.order <- c(p, d, q)
    KUNUFfinal.arima <- arima(log.ret, order=KUNUFfinal.order)
  }
}

KUNUFfinal.order  ## 2 0 4 

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(KUNUFfinal.arima), na.action=na.omit)
Box.test(resid(KUNUFfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(KUNUFfinal.arima))
acf(abs(resid(KUNUFfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_KUNUF <- fitdist(as.numeric(resid(KUNUFfinal.arima)), "norm")
plot(fit_KUNUF, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_KUNUF <- fitdist(as.numeric(resid(KUNUFfinal.arima)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(KUNUFfinal.arima))),sd=sd(as.numeric(resid(KUNUFfinal.arima)))))
plot(fit_KUNUF, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################

#use the forecast library to automatically determine ARIMA coefficients
fit_KUNUF <- auto.arima(log.ret)
fit_KUNUF ## 0 0 0 ???
fit_KUNUF$arima$coef  #forecast pkg yields AR(1)
plot(fit_KUNUF$residuals)
Acf(fit_KUNUF$residuals)  # bad fitting
plot(forecast(KUNUF.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(tencnetfinal.arima)
ft.garch <- garch(resid(KUNUFfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(KUNUFfinal.arima))
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

getSymbols("0135.HK", from="2013-01-01", to="2018-02-28")

head(`0135.HK`)
tail(`0135.HK`)
sum(is.nan(`0135.HK`))  ### no missing values
# dates -> index
dt=index(`0135.HK`); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(`0135.HK`, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(`0135.HK`, subset='2014-01-01/2014-12-31', type='line', name ='0135.HK.Close', TA=NULL)
chartSeries(`0135.HK`, subset='last 4 months', type='bars', name ='0135.HK.Close', TA=NULL)
chartSeries(`0135.HK`, subset='last 4 months', theme="white", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

###### the same steps
##### Normality
##### Stationality
##### ARMA & ARIMA
##### GARCH
library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Kunlun Energy Company 

####################### Getting Data ###################################
getSymbols("LNGPF", from="2013-01-01", to="2018-02-28")
class(LNGPF)
str(LNGPF)
head(LNGPF)
tail(LNGPF)
LNGPF$LNGPF.Close[1:5]
# dates -> index
dt=index(LNGPF); length(dt); dt[1]; dt[length(dt)]
sum(is.na(LNGPF)) ## NO MISSING 
######################### Plotting ######################################
#plot data
chartSeries(LNGPF, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(LNGPF, subset='2014-01-01/2014-12-31', type='line', name ='LNGPF.Close', TA=NULL)
chartSeries(LNGPF, subset='last 4 months', type='bars', name ='LNGPF.Close', TA=NULL)
chartSeries(LNGPF, subset='last 4 months', theme="black", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
LNGPF.c=LNGPF$LNGPF.Close
plot(LNGPF.c, main='LNGPF.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(LNGPF.c, n = 50), on=1, col="green")

# add month end points to the chart
points(LNGPF.c[endpoints(LNGPF.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(LNGPF)
LNGPF.c=LNGPF$LNGPF.Close

# simple return
simple.ret = Delt(LNGPF.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(LNGPF.c, main='LNGPF.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(LNGPF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(LNGPF.c, main='LNGPF.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(LNGPF$LNGPF.Volume[-1])
df.tmp$LNGPF.lrt <- log.ret
LNGPF.xts.lrt = as.xts(df.tmp$LNGPF.lrt)
chartSeries(LNGPF.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='LNGPF.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
LNGPF.c.v=as.numeric(LNGPF.c);
acf(LNGPF.c.v)
plot(LNGPF.c.v, type='l')
summary(LNGPF.c.v)
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
mean(LNGPF.c); var(LNGPF.c); skewness(LNGPF.c); kurtosis(LNGPF.c)
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
adf.test(LNGPF.c);#- p=0.1741 # non-stationary
kpss.test(LNGPF.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(LNGPF.c, type="log")[-1]  
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
# Plot daily closing prices for Alibab (LNGPF & AMZN)
getSymbols("LNGPF", src="google")
plot(Cl(LNGPF))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
LNGPFrt = diff(log(Cl(LNGPF)))[-1]
head(LNGPFrt); tail(LNGPFrt)
plot(LNGPFrt)
plot(density(LNGPFrt))
acf(LNGPFrt, na.action=na.omit)
wilcox.test(as.numeric(LNGPFrt))
adf.test(LNGPFrt) #-stationary
kpss.test(LNGPFrt)
Box.test(LNGPFrt) 

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
acf(abs(LNGPFrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_LNGPF <- fitdist(as.numeric(LNGPFrt), "norm")
gofstat(fit_LNGPF)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_LNGPF, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(LNGPFrt))  #### not normal!!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_LNGPF <- fitdist(as.numeric(LNGPFrt),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(LNGPFrt)),sd=sd(as.numeric(LNGPFrt))))
plot(fit_LNGPF, histo = FALSE, demp = TRUE)
fit_LNGPF$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
LNGPFrt.ma <- arima(LNGPFrt, order=c(0, 0, 1))
acf(LNGPFrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
LNGPFrt.ma <- arima(LNGPFrt, order=c(0, 0, 2))
acf(LNGPFrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
LNGPFfinal.aic <- Inf
LNGPFfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
  LNGPFcurrent.aic <- AIC(arima(LNGPFrt, order=c(i, 0, j)))
  if (LNGPFcurrent.aic < LNGPFfinal.aic) {
    LNGPFfinal.aic <- LNGPFcurrent.aic
    LNGPFfinal.order <- c(i, 0, j)
    LNGPFfinal.arma <- arima(LNGPFrt, order=LNGPFfinal.order)
  }
}

# Output the results of the fit
LNGPFfinal.order  #[1] 2 0 4 

# Plot the residuals of the final model
acf(resid(LNGPFfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(LNGPFfinal.arma), lag=20, type="Ljung-Box")

plot(density(LNGPFfinal.arma$residuals))

wilcox.test(resid(LNGPFfinal.arma))
adf.test(resid(LNGPFfinal.arma)) #-stationary
kpss.test(resid(LNGPFfinal.arma))

# Plot the correlogram
acf(abs(resid(LNGPFfinal.arma)))   ## still correlated

#fit to normal
fit_LNGPF <- fitdist(as.numeric(resid(LNGPFfinal.arma)), "norm")
gofstat(fit_LNGPF)
#summary(fit)
plot(fit_LNGPF, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_LNGPF <- fitdist(as.numeric(resid(LNGPFfinal.arma)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(LNGPFfinal.arma))),sd=sd(as.numeric(resid(LNGPFfinal.arma)))))
plot(fit_LNGPF, histo = FALSE, demp = TRUE)
fit_LNGPF$estimate 

library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("LNGPF", src="google")
LNGPF.c=LNGPF$LNGPF.Close
log.ret = Delt(LNGPF.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(LNGPF.c, main='LNGPF.Close')
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
LNGPFfinal.aic <- Inf
LNGPFfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  LNGPFcurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
  if (LNGPFcurrent.aic < LNGPFfinal.aic) {
    LNGPFfinal.aic <- LNGPFcurrent.aic
    LNGPFfinal.order <- c(p, d, q)
    LNGPFfinal.arima <- arima(log.ret, order=LNGPFfinal.order)
  }
}

LNGPFfinal.order  ## 2 0 4 

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(LNGPFfinal.arima), na.action=na.omit)
Box.test(resid(LNGPFfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(LNGPFfinal.arima))
acf(abs(resid(LNGPFfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_LNGPF <- fitdist(as.numeric(resid(LNGPFfinal.arima)), "norm")
plot(fit_LNGPF, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_LNGPF <- fitdist(as.numeric(resid(LNGPFfinal.arima)),"t.scaled",
                     start=list(df=3,mean=mean(as.numeric(resid(LNGPFfinal.arima))),sd=sd(as.numeric(resid(LNGPFfinal.arima)))))
plot(fit_LNGPF, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################

#use the forecast library to automatically determine ARIMA coefficients
fit_LNGPF <- auto.arima(log.ret)
fit_LNGPF ## 0 0 0 ???
fit_LNGPF$arima$coef  #forecast pkg yields AR(1)
plot(fit_LNGPF$residuals)
Acf(fit_LNGPF$residuals)  # bad fitting
plot(forecast(LNGPF.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(tencnetfinal.arima)
ft.garch <- garch(resid(LNGPFfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(LNGPFfinal.arima))
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

getSymbols("0960.HK", from="2013-01-01", to="2018-02-28")

head(`0960.HK`)
tail(`0960.HK`)
sum(is.nan(`0960.HK`))  ### no missing values
# dates -> index
dt=index(`0960.HK`); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(`0960.HK`, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(`0960.HK`, subset='2014-01-01/2014-12-31', type='line', name ='0960.HK.Close', TA=NULL)
chartSeries(`0960.HK`, subset='last 4 months', type='bars', name ='0960.HK.Close', TA=NULL)
chartSeries(`0960.HK`, subset='last 4 months', theme="white", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

###### the same steps
##### Normality
##### Stationality
##### ARMA & ARIMA
##### GARCH
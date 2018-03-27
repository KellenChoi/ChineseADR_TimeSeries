library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Anhui Conch Cement Co Ltd ADR: AHCHY
####################### Getting Data ###################################
getSymbols("AHCHY", from="2013-01-01", to="2018-02-28")
class(AHCHY)
str(AHCHY)
head(AHCHY)
tail(AHCHY)
AHCHY$AHCHY.Close[1:5]
# dates -> index
dt=index(AHCHY); length(dt); dt[1]; dt[length(dt)]
sum(is.na(AHCHY)) 
######################### Plotting ######################################
#plot data
chartSeries(AHCHY, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(AHCHY, subset='2014-01-01/2014-12-31', type='line', name ='AHCHY.Close', TA=NULL)
chartSeries(AHCHY, subset='last 4 months', type='bars', name ='AHCHY.Close', TA=NULL)
chartSeries(AHCHY, subset='last 4 months', theme="black", TA=NULL) #draw the chart 
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
AHCHY.c=AHCHY$AHCHY.Close
plot(AHCHY.c, main='AHCHY.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(AHCHY.c, n = 50), on=1, col="green")

# add month end points to the chart
points(AHCHY.c[endpoints(AHCHY.c, on = "months")], col="red", pch=40, on=1)
# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(AHCHY)
AHCHY.c=AHCHY$AHCHY.Close

# simple return
simple.ret = Delt(AHCHY.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(AHCHY.c, main='AHCHY.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(AHCHY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(AHCHY.c, main='AHCHY.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(AHCHY$AHCHY.Volume[-1])
df.tmp$AHCHY.lrt <- log.ret
AHCHY.xts.lrt = as.xts(df.tmp$AHCHY.lrt)
chartSeries(AHCHY.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='AHCHY.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
AHCHY.c.v=as.numeric(AHCHY.c);
acf(AHCHY.c.v)
plot(AHCHY.c.v, type='l')
summary(AHCHY.c.v)
log.ret.v=as.numeric(log.ret);

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
mean(AHCHY.c); var(AHCHY.c); skewness(AHCHY.c); kurtosis(AHCHY.c)
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
adf.test(AHCHY.c);#- p=0.9536 # non-stationary
kpss.test(AHCHY.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(AHCHY.c, type="log")[-1]  
adf.test(log.ret);#- p<<0.01  # stationary
kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!

#chartSeries(AHCHY)
chartSeries(log.ret)          

###################### Is the TS really stationary? ####################################
log.ret.1 <- log.ret['2014-09-22/2015-09-21']
mean(log.ret.1); var(log.ret.1)
log.ret.2 <- log.ret['2016-09-22/2017-09-21']
mean(log.ret.2); var(log.ret.2)

#mu2 > 5*mu1;  var2 < 0.25*var1 
#Conclusion the TS of log-returns of AHCHY is NOn-Stationary!!!!!!!!!!!!!!!
###########################################################################
################# Check serial correlation vs. i.i.d. ####################
############### Dynamic trend / Seasonality #################################
#############################################################################################
acf(log.ret)  
acf(log.ret.1) 
acf(log.ret.2) 
#Conclusion - the market's regime has changed
#Box-Pierce test for serial correlation
Box.test(log.ret)   #p-value = 0.7853 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.1898 Conclusion - there is NO serial correlation
Box.test(log.ret.2)  #p-value = 0.5436 - there is no serial correlation !!!!

############################# Modelling Patterns ##############################
# Plot daily closing prices for Alibab (AHCHY & AMZN)
getSymbols("AHCHY", src="google")
plot(Cl(AHCHY))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
ahchyrt = diff(log(Cl(AHCHY)))[-1]
head(ahchyrt); tail(ahchyrt)
plot(ahchyrt)
plot(density(ahchyrt))
acf(ahchyrt, na.action=na.omit)
wilcox.test(as.numeric(ahchyrt))
adf.test(ahchyrt) #-stationary
kpss.test(ahchyrt)
Box.test(ahchyrt) 

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
acf(abs(ahchyrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_ahchy <- fitdist(as.numeric(ahchyrt), "norm")
gofstat(fit_ahchy)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_ahchy, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(ahchyrt))  #### 1.14043

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_ahchy <- fitdist(as.numeric(ahchyrt),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(ahchyrt)),sd=sd(as.numeric(ahchyrt))))
plot(fit_ahchy, histo = FALSE, demp = TRUE)
fit_ahchy$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
ahchyrt.ma <- arima(ahchyrt, order=c(0, 0, 1))
acf(ahchyrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
ahchyrt.ma <- arima(ahchyrt, order=c(0, 0, 2))
acf(ahchyrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
ahchyfinal.aic <- Inf
ahchyfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
        ahchycurrent.aic <- AIC(arima(ahchyrt, order=c(i, 0, j)))
        if (ahchycurrent.aic < ahchyfinal.aic) {
                ahchyfinal.aic <- ahchycurrent.aic
                ahchyfinal.order <- c(i, 0, j)
                ahchyfinal.arma <- arima(ahchyrt, order=ahchyfinal.order)
        }
}

# Output the results of the fit
ahchyfinal.order  #[1] 0 0 0 ????

# Plot the residuals of the final model
acf(resid(ahchyfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(ahchyfinal.arma), lag=20, type="Ljung-Box")

plot(density(ahchyfinal.arma$residuals))

wilcox.test(resid(ahchyfinal.arma))
adf.test(resid(ahchyfinal.arma)) #-stationary
kpss.test(resid(ahchyfinal.arma))

# Plot the correlogram
acf(abs(resid(ahchyfinal.arma)))

#fit to normal
fit_ahchy <- fitdist(as.numeric(resid(ahchyfinal.arma)), "norm")
gofstat(fit_ahchy)
#summary(fit)
plot(fit_ahchy, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_ahchy <- fitdist(as.numeric(resid(ahchyfinal.arma)),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(resid(ahchyfinal.arma))),sd=sd(as.numeric(resid(ahchyfinal.arma)))))
plot(fit_ahchy, histo = FALSE, demp = TRUE)

library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("AHCHY", src="google")
AHCHY.c=AHCHY$AHCHY.Close
log.ret = Delt(AHCHY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(AHCHY.c, main='AHCHY.Close')
plot(log.ret, type="l", on=NA)
acf(log.ret)  #no correlation in log-returns???
pacf(log.ret)
Box.test(log.ret)
#proxy for volatility
plot(abs(log.ret), type="l", on=NA)
acf(abs(log.ret))
Box.test(abs(log.ret))  # p-value = 0.005079: correlation in volatility??
#########################################################
# Determine the best fitting ARIMA model
ahchyfinal.aic <- Inf
ahchyfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
        ahchycurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
        if (ahchycurrent.aic < ahchyfinal.aic) {
                ahchyfinal.aic <- ahchycurrent.aic
                ahchyfinal.order <- c(p, d, q)
                ahchyfinal.arima <- arima(log.ret, order=ahchyfinal.order)
        }
}

# Output the best ARIMA order
ahchyfinal.order   ## 1 0 1

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(ahchyfinal.arima), na.action=na.omit)
Box.test(resid(ahchyfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(ahchyfinal.arima))
acf(abs(resid(ahchyfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_ahchy <- fitdist(as.numeric(resid(ahchyfinal.arima)), "norm")
plot(fit_ahchy, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_ahchy <- fitdist(as.numeric(resid(ahchyfinal.arima)),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(resid(ahchyfinal.arima))),sd=sd(as.numeric(resid(ahchyfinal.arima)))))
plot(fit_ahchy, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################
#use the forecast library to automatically determine ARIMA coefficients
fit_ahchy <- auto.arima(log.ret)
fit_ahchy  ### ARIMA(0,0,0) with zero mean 
fit_ahchy$arima$coef  #forecast pkg yields AR(1)
plot(fit_ahchy$residuals)
Acf(fit_ahchy$residuals)  # bad fitting
plot(forecast(AHCHY.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(tencnetfinal.arima)
ft.garch <- garch(resid(ahchyfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(ahchyfinal.arima))
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

getSymbols("0914.HK", from="2013-01-01", to="2018-02-28")

head(`0914.HK`)
tail(`0914.HK`)
sum(is.na(`0914.HK`))  ### no missing values
# dates -> index
dt=index(`0914.HK`); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(`0914.HK`, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(`0914.HK`, subset='2014-01-01/2014-12-31', type='line', name ='0914.HK.Close', TA=NULL)
chartSeries(`0914.HK`, subset='last 4 months', type='bars', name ='0914.HK.Close', TA=NULL)

chartSeries(`0914.HK`, subset='last 4 months', theme="white", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

###### the same steps
##### Normality
##### Stationality
##### ARMA & ARIMA
##### GARCH

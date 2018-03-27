library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. AAC Technologies Holdings Inc
####################### Getting Data ###################################
getSymbols("AACAY", from= "2013-01-01", to="2018-02-28")
class(AACAY)
str(AACAY)
head(AACAY)
tail(AACAY)
AACAY$AACAY.Close[1:5]
# dates -> index
dt=index(AACAY); length(dt); dt[1]; dt[length(dt)]  
#### [1] 1734
#### [1] "2011-04-07"
#### [1] "2018-02-27"
is.na(AACAY)
sum(is.na(AACAY))  ### no missing values 

######################### Plotting ######################################
#plot data
chartSeries(AACAY, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(AACAY, subset='2014-01-01/2014-12-31', type='line', name ='AACAY.Close', TA=NULL)
chartSeries(AACAY, subset='last 4 months', type='bars', name ='AACAY.Close', TA=NULL)
chartSeries(AACAY, subset='last 4 months', theme = 'white', name ='AACAY.Close', TA=NULL)
addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
AACAY.c=AACAY$AACAY.Close
plot(AACAY.c, main='AACAY.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(AACAY.c, n = 50), on=1, col="green")

# add month end points to the chart
points(AACAY.c[endpoints(AACAY.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(AACAY)
AACAY.c=AACAY$AACAY.Close

# simple return
simple.ret = Delt(AACAY.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(AACAY.c, main='AACAY.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(AACAY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(AACAY.c, main='AACAY.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(AACAY$AACAY.Volume[-1])
df.tmp$AACAY.lrt <- log.ret
AACAY.xts.lrt = as.xts(df.tmp$AACAY.lrt)
chartSeries(AACAY.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='AACAY.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
AACAY.c.v=as.numeric(AACAY.c);
acf(AACAY.c.v)
plot(AACAY.c.v, type='l')
summary(AACAY.c.v)
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
cvm.test(log.ret.v, null='pnorm')  ######### is log.ret normal?

###############################################################################
###################### Normal and log-Normal distributions ######################
mean(AACAY.c); var(AACAY.c); skewness(AACAY.c); kurtosis(AACAY.c)
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
# ------> NOT NORMAL
######################## Check stationarity ##################################
### Rejecting the null hypothesis suggests that a time series is stationary
adf.test(AACAY.c);#- p=0.7842 # non-stationary
kpss.test(AACAY.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(AACAY.c, type="log")[-1]  
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
Box.test(log.ret)   #p-value = 0.7011 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.7441 Conclusion - there is NO serial correlation
Box.test(log.ret.2)  #p-value = 0.9685 - there is no serial correlation ????????

############################# Modelling Patterns ##############################
# Plot daily closing prices (AACAY & AMZN) ### comparing with US company in construction biz
getSymbols("AACAY", src="google")
plot(Cl(AACAY))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
aacayrt = diff(log(Cl(AACAY)))[-1]
head(aacayrt); tail(aacayrt)
plot(aacayrt)
plot(density(aacayrt))
acf(aacayrt, na.action=na.omit)
wilcox.test(as.numeric(aacayrt))
adf.test(aacayrt) #-stationary
kpss.test(aacayrt)
Box.test(aacayrt) 

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
acf(abs(aacayrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_aacay <- fitdist(as.numeric(aacayrt), "norm")
gofstat(fit_aacay)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_aacay, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(aacayrt))

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_aacay <- fitdist(as.numeric(aacayrt),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(aacayrt)),sd=sd(as.numeric(aacayrt))))
plot(fit_aacay, histo = FALSE, demp = TRUE)
fit_aacay$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
aacayrt.ma <- arima(aacayrt, order=c(0, 0, 1))
acf(aacayrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
aacayrt.ma <- arima(aacayrt, order=c(0, 0, 2))
acf(aacayrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
aacayfinal.aic <- Inf
aacayfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
        aacaycurrent.aic <- AIC(arima(aacayrt, order=c(i, 0, j)))
        if (aacaycurrent.aic < aacayfinal.aic) {
                aacayfinal.aic <- aacaycurrent.aic
                aacayfinal.order <- c(i, 0, j)
                aacayfinal.arma <- arima(aacayrt, order=aacayfinal.order)
        }
}

# Output the results of the fit
aacayfinal.order  #[1] 4 0 4

# Plot the residuals of the final model
acf(resid(aacayfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(aacayfinal.arma), lag=20, type="Ljung-Box")

plot(density(aacayfinal.arma$residuals))

wilcox.test(resid(aacayfinal.arma))
adf.test(resid(aacayfinal.arma)) #-stationary
kpss.test(resid(aacayfinal.arma))

# Plot the correlogram
acf(abs(resid(aacayfinal.arma)))

#fit to normal
fit_aacay <- fitdist(as.numeric(resid(aacayfinal.arma)), "norm")
gofstat(fit_aacay)
#summary(fit)
plot(fit_aacay, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_aacay <- fitdist(as.numeric(resid(aacayfinal.arma)),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(resid(aacayfinal.arma))),sd=sd(as.numeric(resid(aacayfinal.arma)))))


library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("AACAY", src="google")
head(AACAY); tail(AACAY); str(AACAY)
AACAY.c=AACAY$AACAY.Close
log.ret = Delt(AACAY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(AACAY.c, main='AACAY.Close')
plot(log.ret, type="l", on=NA)
acf(log.ret)  # correlation in log-returns ???
pacf(log.ret)
Box.test(log.ret)
#proxy for volatility
plot(abs(log.ret), type="l", on=NA)
acf(abs(log.ret))
Box.test(abs(log.ret))  # p-value = 0.01355: NO correlation in volatility?
#########################################################
# Determine the best fitting ARIMA model
aacayfinal.aic <- Inf
aacayfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
        aacaycurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
        if (aacaycurrent.aic < aacayfinal.aic) {
                aacayfinal.aic <- aacaycurrent.aic
                aacayfinal.order <- c(p, d, q)
                aacayfinal.arima <- arima(log.ret, order=aacayfinal.order)
        }
}


# Output the best ARIMA order
aacayfinal.order

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(aacayfinal.arima), na.action=na.omit)
Box.test(resid(aacayfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(aacayfinal.arima))
acf(abs(resid(aacayfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_aacay <- fitdist(as.numeric(resid(aacayfinal.arima)), "norm")
plot(fit_aacay, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_aacay <- fitdist(as.numeric(resid(aacayfinal.arima)),"t.scaled",
                       start=list(df=3,mean=mean(as.numeric(resid(aacayfinal.arima))),sd=sd(as.numeric(resid(aacayfinal.arima)))))
plot(fit_aacay, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################
#use the forecast library to automatically determine ARIMA coefficients
fit_aacay <- auto.arima(log.ret)
fit_aacay    ## 4 0 2
fit_aacay$arima$coef  #forecast pkg yields AR(1)
plot(fit_aacay$residuals)
Acf(fit_aacay$residuals)  # bad fitting
plot(forecast(AACAY.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(aacayfinal.arima)
ft.garch <- garch(resid(aacayfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(aacayfinal.arima))
#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
plot(fit, histo = FALSE, demp = TRUE)

# Plot the residuals and abs residuals
acf(ft.res)
acf(abs(ft.res))  # no correlations/no information left!!!         









########## Uderlying Stock: Hong Kong Exchange ###############
library(Quandl)
Quandl.api_key("xPCxT4avL3ymcTUyvT7N")
aacay <- Quandl("XHKG/00914", start_date="2013-01-01", end_date="2018-01-01")

getSymbols("2018.HK", from="2013-04-07", to="2018-02-28")

head(`2018.HK`)
tail(`2018.HK`)
sum(is.na(`2018.HK`))   #### no missing values
# dates -> index
dt=index(`2018.HK`); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(`2018.HK`, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(`2018.HK`, subset='2014-01-01/2014-12-31', type='line', name ='0914.HK.Close', TA=NULL)
chartSeries(`2018.HK`, subset='last 4 months', type='bars', name ='0914.HK.Close', TA=NULL)

chartSeries(`2018.HK`, subset='last 4 months', theme="white", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

###### the same steps
##### Normality
##### Stationality
##### ARMA & ARIMA
##### GARCH

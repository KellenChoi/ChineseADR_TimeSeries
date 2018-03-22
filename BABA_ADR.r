library(forecast)
#library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Alibaba
####################### Getting Data ###################################
getSymbols("BABA",from="2013-01-01", to="2018-02-28")
class(BABA)
str(BABA)
head(BABA)
tail(BABA)
BABA$BABA.Close[1:5]
# dates -> index
dt=index(BABA); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(BABA, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(BABA, subset='2014-09-01/2014-12-01', type='line', name ='BABA.Close', TA=NULL)
chartSeries(BABA, subset='last 4 months', type='bars', name ='BABA.Close', TA=NULL)

#### BABA.xts <- BABA.xts['201-015-01/2016-01-01']

chartSeries(BABA, subset='last 4 months', theme="white", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
BABA.c=BABA$BABA.Close
plot(BABA.c, main='BABA.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(BABA.c, n = 50), on=1, col="green")

# add month end points to the chart
points(BABA.c[endpoints(BABA.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red")
         
######################### Returns #############################
#######################################################################
head(BABA)
BABA.c=BABA$BABA.Close
# simple return
simple.ret = Delt(BABA.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(BABA.c, main='BABA.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(BABA.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(BABA.c, main='BABA.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(BABA$BABA.Volume[-1])
df.tmp$BABA.lrt <- log.ret
BABA.xts.lrt = as.xts(df.tmp$BABA.lrt)
chartSeries(BABA.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='BABA.log.ret')
#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

###############################Distributional properties of returns##############
#############################################################################
BABA.c.v=as.numeric(BABA.c);
acf(BABA.c.v)
plot(BABA.c.v, type='l')
summary(BABA.c.v)
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
mean(BABA.c); var(BABA.c); skewness(BABA.c); kurtosis(BABA.c)
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
adf.test(BABA.c);#- p=0.951  # non-stationary
kpss.test(BABA.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(BABA.c, type="log")[-1]  
adf.test(log.ret);#- p<<0.01
kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!

#chartSeries(BABA)
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
acf(log.ret)  # anticorrelation & 7 lags memory !!!!
acf(log.ret.1) #strong anticorrelation & 7 lags memory
acf(log.ret.2) #very (5% conf interval) small anticorrelation at lag 4
#Conclusion - the market's regime has changed
#Box-Pierce test for serial correlation
Box.test(log.ret)   #p-value = 0.1314 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.1523 Conclusion - there is NO serial correlation
Box.test(log.ret.2)  #p-value = 0.9645 - there is no serial correlation !!!!

############################# Modelling Patterns ##############################
# Plot daily closing prices for Alibab (BABA & AMZN)
getSymbols("BABA", src="google")
plot(Cl(BABA))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
babart = diff(log(Cl(BABA)))[-1]
head(babart); tail(babart)
plot(babart)
plot(density(babart))
acf(babart, na.action=na.omit)
wilcox.test(as.numeric(babart))
adf.test(babart) #-stationary
kpss.test(babart)
Box.test(babart) #- p-value = 0.1096 - no Strong serial correlation???

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
acf(abs(babart))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_baba <- fitdist(as.numeric(babart), "norm")
gofstat(fit_baba)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_baba, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(babart))

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
               start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_baba <- fitdist(as.numeric(babart),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(babart)),sd=sd(as.numeric(babart))))
plot(fit_baba, histo = FALSE, demp = TRUE)
fit_baba$estimate 

###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
babart.ma <- arima(babart, order=c(0, 0, 1))
acf(babart.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
babart.ma <- arima(babart, order=c(0, 0, 2))
acf(babart.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
babafinal.aic <- Inf
babafinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
        babacurrent.aic <- AIC(arima(babart, order=c(i, 0, j)))
        if (babacurrent.aic < babafinal.aic) {
                babafinal.aic <- babacurrent.aic
                babafinal.order <- c(i, 0, j)
                babafinal.arma <- arima(babart, order=babafinal.order)
        }
}

# Output the results of the fit
babafinal.order

# Plot the residuals of the final model
acf(resid(babafinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(babafinal.arma), lag=20, type="Ljung-Box")

plot(density(babafinal.arma$residuals))

wilcox.test(resid(babafinal.arma))
adf.test(resid(babafinal.arma)) #-stationary
kpss.test(resid(babafinal.arma))

# Plot the correlogram
acf(abs(resid(babafinal.arma)))

#fit to normal
fit_baba <- fitdist(as.numeric(resid(babafinal.arma)), "norm")
gofstat(fit_baba)
#summary(fit)
plot(fit_baba, histo = FALSE, demp = TRUE)

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_baba <- fitdist(as.numeric(resid(babafinal.arma)),"t.scaled",
               start=list(df=3,mean=mean(as.numeric(resid(babafinal.arma))),sd=sd(as.numeric(resid(babafinal.arma)))))


library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("BABA", src="google")
head(BABA); tail(BABA); str(BABA)
BABA.c=BABA$BABA.Close
log.ret = Delt(BABA.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(BABA.c, main='BABA.Close')
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
babafinal.aic <- Inf
babafinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
        babacurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
        if (babacurrent.aic < babafinal.aic) {
                babafinal.aic <- babacurrent.aic
                babafinal.order <- c(p, d, q)
                babafinal.arima <- arima(log.ret, order=babafinal.order)
        }
}

# Output the best ARIMA order
babafinal.order

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(babafinal.arima), na.action=na.omit)
Box.test(resid(babafinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(babafinal.arima))
acf(abs(resid(babafinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_baba <- fitdist(as.numeric(resid(babafinal.arima)), "norm")
plot(fit_baba, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_baba <- fitdist(as.numeric(resid(babafinal.arima)),"t.scaled",
               start=list(df=3,mean=mean(as.numeric(resid(babafinal.arima))),sd=sd(as.numeric(resid(babafinal.arima)))))
plot(fit_baba, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################
#use the forecast library to automatically determine ARIMA coefficients
fit_baba <- auto.arima(log.ret)
fit_baba
fit_baba$arima$coef  #forecast pkg yields AR(1)
plot(fit_baba$residuals)
Acf(fit_baba$residuals)  # bad fitting
plot(forecast(BABA.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(babafinal.arima)
ft.garch <- garch(resid(babafinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(babafinal.arima))
#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
plot(fit, histo = FALSE, demp = TRUE)

# Plot the residuals and abs residuals
acf(ft.res)
acf(abs(ft.res))  # no correlations/no information left!!!



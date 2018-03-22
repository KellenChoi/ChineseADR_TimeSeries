library(forecast)
#library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. ZTE
####################### Getting Data ###################################
getSymbols("ZTCOY",from="2013-01-01", to="2018-02-28")
class(ZTCOY)
str(ZTCOY)
head(ZTCOY)
tail(ZTCOY)
ZTCOY$ZTCOY.Close[1:5]
# dates -> index
dt=index(ZTCOY); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(ZTCOY, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(ZTCOY, subset='2014-09-01/2014-12-01', type='line', name ='ZTCOY.Close', TA=NULL)
chartSeries(ZTCOY, subset='last 4 months', type='bars', name ='ZTCOY.Close', TA=NULL)

#### ZTCOY.xts <- ZTCOY.xts['201-015-01/2016-01-01']

chartSeries(ZTCOY, subset='last 4 months', theme="white", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
ZTCOY.c=ZTCOY$ZTCOY.Close
plot(ZTCOY.c, main='ZTCOY.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(ZTCOY.c, n = 50), on=1, col="green")

# add month end points to the chart
points(ZTCOY.c[endpoints(ZTCOY.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red")
          
          ######################### Returns #############################
          #######################################################################
          head(ZTCOY)
          ZTCOY.c=ZTCOY$ZTCOY.Close
          # simple return
          simple.ret = Delt(ZTCOY.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
          # plot the close and add a panel with the simple returns
          plot(ZTCOY.c, main='ZTCOY.Close')
          lines(simple.ret, type="h", on=NA)
          
          # log return
          log.ret = Delt(ZTCOY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
          # plot the close and add a panel with the simple returns
          plot(ZTCOY.c, main='ZTCOY.Close')
          lines(log.ret, type="h", on=NA)
          
          ###############################################################################
          ##################################################################################
          #if we want to use xts format for derived variables:
          df.tmp = data.frame(ZTCOY$ZTCOY.Volume[-1])
          df.tmp$ZTCOY.lrt <- log.ret
          ZTCOY.xts.lrt = as.xts(df.tmp$ZTCOY.lrt)
          chartSeries(ZTCOY.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='ZTCOY.log.ret')
          #please read quantmod manual to familiarize yourself with capabilities
          addSMA() #simple moving average
          addEMA(100) #exponential moving average
          ###addVo() #add volume 
          addBBands() #add Bollinger Bands 
          addCCI() #add Commodity Channel Index    
          
          ###############################Distributional properties of returns##############
          #############################################################################
          ZTCOY.c.v=as.numeric(ZTCOY.c);
          acf(ZTCOY.c.v)
          plot(ZTCOY.c.v, type='l')
          summary(ZTCOY.c.v)
          log.ret.v=as.numeric(log.ret);
          
          ########################### Normality tests ###################################
          #need additional tools
          library(MASS)
          library(survival)
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
          
          # all p value is really small.
          
          ###############################################################################
          ###################### Normal and log-Normal distributions ######################
          mean(ZTCOY.c); var(ZTCOY.c); skewness(ZTCOY.c); kurtosis(ZTCOY.c)
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
          
          # not normal.
          
          ######################## Check stationarity ##################################
          ### Rejecting the null hypothesis suggests that a time series is stationary
          library(tseries)
          adf.test(ZTCOY.c);#- p=0.951  # non-stationary
          kpss.test(ZTCOY.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          
          
          log.ret = Delt(ZTCOY.c, type="log")[-1]  
          adf.test(log.ret);#- p<<0.01
          kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!
          
          #chartSeries(ZTCOY)
          library(quantmod)
          chartSeries(log.ret)          
          
          ###################### Is the TS really stationary? ####################################
          log.ret.1 <- log.ret['2014-09-22/2015-09-21']
          mean(log.ret.1); var(log.ret.1)
          log.ret.2 <- log.ret['2016-09-22/2017-09-21']
          mean(log.ret.2); var(log.ret.2)
          
          #mu2 > 5*mu1;  var2 < 0.25*var1 
          #Conclusion the TS of log-returns of ZTCOY is NOn-Stationary!!!!!!!!!!!!!!!
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
          # Plot daily closing prices for Alibab (ZTCOY & AMZN)
          getSymbols("ZTCOY", src="google")
          plot(Cl(ZTCOY))
          
          getSymbols("AMZN", src="google")
          plot(Cl(AMZN))
          
          # Create differenced log returns 
          # and plot their values and correlogram
          ZTCOYrt = diff(log(Cl(ZTCOY)))[-1]
          head(ZTCOYrt); tail(ZTCOYrt)
          plot(ZTCOYrt)
          plot(density(ZTCOYrt))
          acf(ZTCOYrt, na.action=na.omit)
          wilcox.test(as.numeric(ZTCOYrt))
          adf.test(ZTCOYrt) #-stationary
          kpss.test(ZTCOYrt)
          Box.test(ZTCOYrt) #- p-value = 0.1096 - no Strong serial correlation???
          
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
          acf(abs(ZTCOYrt))
          acf(abs(amznrt))
          
          #fit to normal
          fit_amzn <- fitdist(as.numeric(amznrt), "norm")
          gofstat(fit_amzn)
          
          fit_ZTCOY <- fitdist(as.numeric(ZTCOYrt), "norm")
          gofstat(fit_ZTCOY)
          
          #summary(fit)
          plot(fit_amzn, histo = FALSE, demp = TRUE)
          kurtosis(as.numeric(amznrt))
          
          plot(fit_ZTCOY, histo = FALSE, demp = TRUE)
          kurtosis(as.numeric(ZTCOYrt))
          
          #fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
          library("metRology") #--#has implementation of the non-standard t-distribution
          fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                              start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
          plot(fit_amzn, histo = FALSE, demp = TRUE)
          fit_amzn$estimate                    
          
          #fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
          library("metRology") #--#has implementation of the non-standard t-distribution
          fit_ZTCOY <- fitdist(as.numeric(ZTCOYrt),"t.scaled",
                              start=list(df=3,mean=mean(as.numeric(ZTCOYrt)),sd=sd(as.numeric(ZTCOYrt))))
          plot(fit_ZTCOY, histo = FALSE, demp = TRUE)
          fit_ZTCOY$estimate 
          
          ###################################################################################
          ########################### MA(q) process #######################################
          # Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
          # and plot the correlogram of the residuals
          ZTCOYrt.ma <- arima(ZTCOYrt, order=c(0, 0, 1))
          acf(ZTCOYrt.ma$res[-1])
          
          # Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
          # and plot the correlogram of the residuals
          ZTCOYrt.ma <- arima(ZTCOYrt, order=c(0, 0, 2))
          acf(ZTCOYrt.ma$res[-1]) 
          # the distribution of ZTCOY MA[1] AND MA[2] are THE SAME!!!
          
          #######################################################################
          ################## ARMA(p,q) ###########################################
          # Loop over p = 0 to 4, q = 0 to 4 and create each
          # ARMA(p,q) model, then fit to the previous amznrt 
          # returns, using the AIC to find the best fit
          ZTCOYfinal.aic <- Inf
          ZTCOYfinal.order <- c(0,0,0)
          for (i in 0:4) for (j in 0:4) {
            ZTCOYcurrent.aic <- AIC(arima(ZTCOYrt, order=c(i, 0, j)))
            if (ZTCOYcurrent.aic < ZTCOYfinal.aic) {
              ZTCOYfinal.aic <- ZTCOYcurrent.aic
              ZTCOYfinal.order <- c(i, 0, j)
              ZTCOYfinal.arma <- arima(ZTCOYrt, order=ZTCOYfinal.order)
            }
          }
          
          # Output the results of the fit
          ZTCOYfinal.order
          
          # Plot the residuals of the final model
          acf(resid(ZTCOYfinal.arma), na.action=na.omit)
          
          # Carry out a Ljung-Box test for realisation
          # of discrete white noise
          Box.test(resid(ZTCOYfinal.arma), lag=20, type="Ljung-Box")
          
          plot(density(ZTCOYfinal.arma$residuals))
          
          wilcox.test(resid(ZTCOYfinal.arma))
          adf.test(resid(ZTCOYfinal.arma)) #-stationary
          kpss.test(resid(ZTCOYfinal.arma))
          
          # Plot the correlogram
          acf(abs(resid(ZTCOYfinal.arma)))
          
          #fit to normal
          fit_ZTCOY <- fitdist(as.numeric(resid(ZTCOYfinal.arma)), "norm")
          gofstat(fit_ZTCOY)
          #summary(fit)
          plot(fit_ZTCOY, histo = FALSE, demp = TRUE)
          
          #fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
          fit_ZTCOY <- fitdist(as.numeric(resid(ZTCOYfinal.arma)),"t.scaled",
                              start=list(df=3,mean=mean(as.numeric(resid(ZTCOYfinal.arma))),sd=sd(as.numeric(resid(ZTCOYfinal.arma)))))
          
          
          library(PerformanceAnalytics)  #great pkg
          #######################################################################
          ################## ARIMA ###########################################
          getSymbols("ZTCOY", src="google")
          head(ZTCOY); tail(ZTCOY); str(ZTCOY)
          ZTCOY.c=ZTCOY$ZTCOY.Close
          log.ret = Delt(ZTCOY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
          # plot the close and add a panel with the simple returns
          plot(ZTCOY.c, main='ZTCOY.Close')
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
          ZTCOYfinal.aic <- Inf
          ZTCOYfinal.order <- c(0,0,0)
          for (p in 1:4) for (d in 0:1) for (q in 1:4) {
            ZTCOYcurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
            if (ZTCOYcurrent.aic < ZTCOYfinal.aic) {
              ZTCOYfinal.aic <- ZTCOYcurrent.aic
              ZTCOYfinal.order <- c(p, d, q)
              ZTCOYfinal.arima <- arima(log.ret, order=ZTCOYfinal.order)
            }
          }
          
          # Output the best ARIMA order
          ZTCOYfinal.order
          
          # Plot a correlogram of the residuals, calculate 
          # the Ljung-Box test and predict the next 25 daily
          # values of the series
          acf(resid(ZTCOYfinal.arima), na.action=na.omit)
          Box.test(resid(ZTCOYfinal.arima), lag=20, type="Ljung-Box") #no correlations!
          plot(resid(ZTCOYfinal.arima))
          acf(abs(resid(ZTCOYfinal.arima)))  #still have some unused information
          #try to fit to normal distribution
          fit_ZTCOY <- fitdist(as.numeric(resid(ZTCOYfinal.arima)), "norm")
          plot(fit_ZTCOY, histo = FALSE, demp = TRUE)  # not normal!!!
          
          #fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
          fit_ZTCOY <- fitdist(as.numeric(resid(ZTCOYfinal.arima)),"t.scaled",
                              start=list(df=3,mean=mean(as.numeric(resid(ZTCOYfinal.arima))),sd=sd(as.numeric(resid(ZTCOYfinal.arima)))))
          plot(fit_ZTCOY, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
          ##############################################################
          #use the forecast library to automatically determine ARIMA coefficients
          library(forecast)
          fit_ZTCOY <- auto.arima(log.ret)
          fit_ZTCOY
          fit_ZTCOY$arima$coef  #forecast pkg yields AR(1)
          plot(fit_ZTCOY$residuals)
          Acf(fit_ZTCOY$residuals)  # bad fitting
          plot(forecast(ZTCOY.c, h=50)) #bad forcast
          
          ############################### GARCH ######################################################
          # Fit a GARCH model to resid(ZTCOYfinal.arima)
          ft.garch <- garch(resid(ZTCOYfinal.arima), trace=F)
          ft.res <- ft.garch$res[-1]
          length(resid(ZTCOYfinal.arima))
          #fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
          fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
          plot(fit, histo = FALSE, demp = TRUE)
          
          # Plot the residuals and abs residuals
          acf(ft.res)
          acf(abs(ft.res))  # no correlations/no information left!!!
          
          
          
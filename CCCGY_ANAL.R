library(quantmod)
library(xts)
library(TTR)
library(tseries)
library(fitdistrplus)
library(goftest)
library(moments)
library(metRology)

##China Communications Construction Company Limited  ADR: CCCGY
####################### Getting Data ###################################


getSymbols("CCCGY", src="yahoo", from="2013-01-01")

# missing values check
sum(is.na(CCCGY))
head(CCCGY)
CCCGY.o=CCCGY$CCCGY.Open
CCCGY.c=CCCGY$CCCGY.Close
CCCGY.a=CCCGY$CCCGY.Adjusted

########## Uderlying Stock: Hong Kong Exchange ##############
getSymbols("1800.HK", src="yahoo", from="2013-01-01")
CCCGY_HK <- as.xts(`1800.HK`)
sum(is.na(`1800.HK`))
head(CCCGY_HK)
CCCGY_HK.o=CCCGY_HK$`1800.HK.Open`
CCCGY_HK.c=CCCGY_HK$`1800.HK.Close`
CCCGY_HK.a=CCCGY_HK$`1800.HK.Adjusted`

# Hangseng Index Data
getSymbols("^HSI", from="2013-01-01")
sum(is.na(HSI))  # 24 na
HSI <-na.omit(HSI)
head(HSI)
HSI.o <- HSI$HSI.Open
HSI.c <- HSI$HSI.Close
HSI.o <- HSI$HSI.Open

# merging 
CCCGY_with_HSI <- merge(CCCGY,CCCGY_HK,HSI, all=FALSE)
dt=index(CCCGY_with_HSI)
tail(CCCGY_with_HSI)
sum(is.na(CCCGY_with_HSI))  # 0 na

##### log return ######
CCCGY_log.ret = Delt(CCCGY.o, type="log")[-1] 
CCCGY_HK_log.ret = Delt(CCCGY_HK.c, type="log")[-1] 
HSI_log.ret = Delt(HSI.c, type="log")[-1] 
plot(CCCGY_log.ret, col="blue")
points(CCCGY_HK_log.ret, col="yellow")
lines(HSI_log.ret, col="red")

# Time Series Plotting
library(ggplot2)
library(dygraphs)
library(grid)
library(gridExtra)

# dygraph() - log_ret
CCCGY_log.ret <- cbind(CCCGY_log.ret,CCCGY_HK_log.ret, HSI_log.ret)
dygraph(CCCGY_log.ret, ylab="Log", main="Log Return: CCCGY")
dySeries("..1", label="CCCGY_ADR") 
dySeries("..2", label="CCCGY_HK") 
dyOptions(colors = c("blue", "brown")) 
dyRangeSelector()

        
### merge two objects, CCCGY.o and CCCGY_HK.o
CCCGY_xts <- xts(CCCGY.o, order.by = index(CCCGY), frequency=365)
CCCGY_hk_xts <- xts(CCCGY_HK.o, order.by = index(CCCGY_HK), frequency=365)
HSI_xts <- xts(HSI.o, order.by = index(HSI), frequency=365)
CCCGY_open <- merge(CCCGY_xts, CCCGY_hk_xts )
head(CCCGY_open)
plot(CCCGY_open)
sum(is.na(CCCGY_open))  # ---- 103 NA

## correlation
## merging
z <- merge(as.zoo(CCCGY.o), as.zoo(CCCGY.c), as.zoo(CCCGY_HK.o), 
           as.zoo(CCCGY_HK.c), all = FALSE)  
head(z)
# set the column names
myColnames <- c("CCCGY.Open","CCCGY.Close",
                "CCCGY.HK.Open", "CCCGY.HK.Close")
colnames(z) <- myColnames
# cast back to an xts object
mktprices <- as.xts(z)
# calculate log returns
mktRtns <- diff(log(mktprices), lag = 1)
head(mktRtns)
mktRtns <- mktRtns[-1,] # remove NA in the 1st row
head(mktRtns)

require(gplots)
cor_heat_map <- function(correlationMatrix, title)
{
  
   heatmap.2(x = correlationMatrix,
             cellnote = correlationMatrix,
             main = title,
             symm = TRUE,
             dendrogram="none",
             Rowv = FALSE,
             trace = "none",
             density.info = "none",
             notecol = "black")
}
corr <- cor(mktRtns) * 100
# corr1 <- cor(mktRtns['2003-01/2003-12']) * 100
# corr2 <- cor(mktRtns['2015-01/2017-12']) * 100

par(oma=c(2,2,2,2))
cor_heat_map(corr, "Correlations of CCCGY_ADR and CCCGY_HK")
## cor(CCCGY&CCCGY_HK) = 0.3043
# cor_heat_map(corr1, "Correlations of CCCGY_ADR and CCCGY_HK, 2013")
# cor_heat_map(corr2, "Correlations of CCCGY_ADR and CCCGY_HK, 2017")
## cor(CCCGY&CCCGY_HK) = 0.35

# scatter plots
CCCGY <- merge(CCCGY, CCCGY_HK)
p1 <- ggplot(data=CCCGY, aes(x = CCCGY$X1800.HK.Close, y = CCCGY$CCCGY.Open)) +
        xlab("Close Price (HK)") +
        ylab("Open Price (USA)") +
        geom_point()
p2 <- ggplot(data=CCCGY, aes(x = CCCGY$X1800.HK.Adjusted, y = CCCGY$CCCGY.Adjusted)) +
        xlab("Close Price (HK)") +
        ylab("Open Price (USA)") +
        geom_point()

# Box plots
p3 <- ggplot(data=CCCGY, aes(x = CCCGY$X1800.HK.Close, y = CCCGY$CCCGY.Open)) +
        xlab("Close Price (HK)") +
        ylab("Open Price (USA)") +
        geom_boxplot()
p4 <- ggplot(data=CCCGY, aes(x = CCCGY$X1800.HK.Adjusted, y = CCCGY$CCCGY.Adjusted)) +
        xlab("Close Price (HK)") +
        ylab("Open Price (USA)") +
        geom_boxplot()
grid.arrange(p1, p2, p3, p4, ncol=2)

# scatter plot matrix
pairs(~CCCGY$CCCGY.Open+CCCGY$CCCGY.Close+CCCGY$CCCGY.Adjusted+X1800.HK.Open+ 
              X1800.HK.Close+X1800.HK.Adjusted, 
      data = CCCGY, 
      main = "Scatter Plot Matrix")


# Project Plan
# 6 Major steps for this Project

# No. 1 Deadline -> April 15th
Getting Proper Data (for 2013 January to 2018 January)

Current Status -> 
Completed individually (Need to combine all of them onto a single file - that's for later just before we prepare the project report.

# No. 2 Deadline -> April 15th
Statistical Analysis on the Data - Arriving at a suitable ARMA or GARCH Model paramets for each of the stocks picked (preferably the parameters should match for the same stock in ADR and HK Exchange)

Current Status -> 
All of us have to follow the steps in Kellen's file (which she presented to Yuri 2 weeks back) and analyze the following:

Find the log returns of the closing prices on ADR and HK. The check:
Stationarity - if not stationary move on!
Normality - if not normal, try with t-distribution - if nothing works, move on!
Autocorrelation - use acf function and see how many period lags; use pacf and see how many period lags;
Based on the results from acf and pacf assume a suitable p and q (nos.) and for the ARMA model with p and q as degrees.
We would prefer to have the same ARMA degrees for the same stock in HK and ADR. So trial and error if we get this.

For most of the stocks, this is where analysis ends. For a few stock, may be we may have to add GARCH model as well.


# No. 3 Deadline -> April 15th
Matching the time-series of HK (closing prices) and ADR (Opening prices)

Current Status ->
Refer the file named 'Code for Time-series Date Matching'. The time series matching is done for 1 stock. We need to do this for all our stocks.

#Important: Based on time-series matching, formulate the trading strategy - For Example - if HK prices went up on by 5% on a day, buy in ADR at the Opening price.

# No. 5 Deadline -> April 22nd
Back-Testing-Part 1
(Using our ARMA/ GARCH model to Forecast the HK and ADR prices for Feb 2018 and compare it with the actual Feb 2018 prices) 

Current Status ->
Not Started

# No. 5 Deadline -> April 22nd
Back-Testing-Part 2
Test the trading strategy on Feb 2018 data and calculate the profit margin) 

Current Status ->
Not Started

# No. 6 Deadline -> April 29th 
Final Report

Current Status ->
Not Started

# Successful project completion and Qi's treat to all of us!

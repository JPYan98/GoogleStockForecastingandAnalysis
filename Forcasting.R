#################################################################
######### This script shows different viz for time series #######
#################################################################
#downloading data set with prices:: 
prices <- read.csv("D:/daily_prices_stocks.csv")
prices$Date <- as.Date(prices$Date)

#downloading dataset with returns :: 
rets <- read.csv("D:/daily_returns_stocks.csv")
rets$Date <- as.Date(rets$Date)

# we will be using ggplot framework and create line charts for time series
#install.packages("ggplot2")
library(ggplot2)

#First looking at the shape of pricing time series? 
ggplot(data=prices)+
  geom_line(aes(x=Date, y=GOOG.Adjusted))

ggplot(data=prices)+
  geom_line(aes(x=Date, y=TLT.Adjusted))

ggplot(data=prices)+
  geom_line(aes(x=Date, y=VNQ.Adjusted))

#let's take a look at all securities together.....
compare_chart <- ggplot(data=prices)+
  geom_line(aes(x=Date, y=GOOG.Adjusted), color="blue")+
  geom_line(aes(x=Date, y=TLT.Adjusted), color="red")+
  geom_line(aes(x=Date, y=VNQ.Adjusted), color="green4")
print(compare_chart)

#we can also make this chart interactive by using plotly: 
#install.packages("plotly")
library(plotly)
ggplotly(compare_chart)
#but this interactive is still not very obvious 

#################################################################
######### Charting time series of returns (ROR) #################
#################################################################

#First looking at the shape of ROR time series? 
ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_GOOG))

ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_TLT))

ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_VNQ))


#let's take a look at all securities together.....
compare_ror <- ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_GOOG), color="blue")+
  geom_line(aes(x=Date, y=returns_TLT), color="red")+
  geom_line(aes(x=Date, y=returns_VNQ), color="green4")
print(compare_ror)


#################################################################
######### Using ADF to test for stationarity    #################
#################################################################

#lets run the ADF test on the prices for the 3 securities:
#install.packages("tseries")
library(tseries)
#ADF test for GOOG prices:
adf.test(prices$GOOG.Adjusted)
#Since the p-value above .05, we accept the null hypothesis.
#This means the time series is non-stationary. 
#In other words, it has some time-dependent structure and does not have constant variance over time.
adf.test(prices$TLT.Adjusted)
adf.test(prices$VNQ.Adjusted)
#all of the above have high p-values, which means they are non-stationary except for VNQ

#now lets do the same for ROR:
adf.test(rets$returns_GOOG) #for GOOG
#Since the p-value below .05, we reject the null hypothesis (H0) and accept alternative (H1).
#This means the time series is stationary. 
#In other words, it has constant variance over time.
adf.test(rets$returns_TLT)
adf.test(rets$returns_VNQ)
# all of the ROR time series are somehow stationary.

#################################################################
######### How do we analyze time series :: pACF test stats #######
#################################################################
# applying pACF to both, stationary and non-stationary to analyze
#pACF stands for partial AutoCorrelation Function
#if the bar goes beyond the blue line, we have autocorrelation for that lag
#if bar is within the blue ribbon, we don't have autocorrelation 
#pACF plots start at lag1. 
#pACF on pricing:
pacf(prices$GOOG.Adjusted)
#pACF on ROR
pacf(rets$returns_GOOG)
#how do these two differ - which one has autocorrelation, and which one doesn't

####################################################
#### Introduction to forecasting time series #######
####################################################
#let's take one more look at the prices for all 3 securities:
compare_chart <- ggplot(data=prices)+
  geom_line(aes(x=Date, y=GOOG.Adjusted), color="blue")+
  geom_line(aes(x=Date, y=TLT.Adjusted), color="red")+
  geom_line(aes(x=Date, y=VNQ.Adjusted), color="green4")
print(compare_chart)

#let's take a look at ROR all securities together.....
compare_ror <- ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_GOOG), color="blue")+
  geom_line(aes(x=Date, y=returns_TLT), color="red")+
  geom_line(aes(x=Date, y=returns_VNQ), color="green4")
print(compare_ror)
#we will be switching to daily ROR - we'll have more fun there - it's usually easier to forecast daily ROR
#which one is easier to forecast? prices (non-stationary) or returns (stationary)?
adf.test(prices$GOOG.Adjusted) #p-value high so it's non-stationary
adf.test(rets$returns_GOOG) #p-value low so it's stationary

###################################################################
#### For non-stationary data - we want to do a decomposition #######
#### To better understand all the behavior in the data ############
#### What are the trends, seasons, residuals? #####################
###################################################################

# we usually look at non-stationary prices for decomp
# we'll need to convert our data frame to a ts object first, ts=timeseries, frequency=250 since trading days in a year
GOOG_ts <- ts(prices[,c("Date", "GOOG.Adjusted")], frequency = 250, start=c(2007, 1, 3))
dec <- decompose(GOOG_ts)
plot(dec) 

TLT_ts <- ts(prices[,c("Date", "TLT.Adjusted")], frequency = 250, start=c(2007, 1, 3))
dec <- decompose(TLT_ts)
plot(dec)

VNQ_ts <- ts(prices[,c("Date", "VNQ.Adjusted")], frequency = 250, start=c(2007, 1, 3))
dec <- decompose(VNQ_ts)
plot(dec)

####################################################
#### ACF and pACF - daily time-series analysis #######
####################################################
#WE WILL USE DAILY RETURNS - ROR for this because it is stationary
#ACF - will show us the moving average (MA) component - TRENDING of errors
acf(rets$returns_GOOG) #GOOG - MA takes 1 day because day 2 does not meet the line
acf(rets$returns_TLT) #TLT - MA take 3 days because day 4 does not meet the line
acf(rets$returns_VNQ) #VNQ - MA takes 1 day because day 2 does not meet the line

#pACF - will show us the lag - "if today depends on yesterday" - autocorrelation - AR
pacf(rets$returns_GOOG) #GOOG - AR takes 1 day because day 2 does not meet the line
pacf(rets$returns_TLT) #TLT - AR takes 3 days because day 4 does not meet the line
pacf(rets$returns_VNQ) #VNQ - AR takes 1 day because day 2 does not meet the line
                      # Inmagine VNQ pACF test for 7 days

#How does this translate into an ARMA model? 

#######################################################################
#### Fitting ARMA on daily ROR stationary time series      ############
#### NOTE: if you wanted to forecast non-stationary, use ARIMA  #######
#######################################################################
#fitting an ARMA(1,1) on GOOG:
GOOG_arma <- arma(rets$returns_GOOG, order=c(1,1)) 
summary(GOOG_arma)
#however, to use the predict() function we need to use the arima function, ARIMA with i=0 us same as ARMA, semi-stationary i=1
#Predict function needs ARIMA model
GOOG_arima <- arima(rets$returns_GOOG, 
                   order=c(1,0,1)) 
predict(GOOG_arima, n.ahead =3) #want to get forecasted values for 3 days out
#   Day 1            Day2         Day 3
#-0.0004348268  0.0002189635  0.0005141405 rate of return
# 0.01878109    0.01879709     0.01880035  rate of uncertainty(error), (High risk), ROR +/- ROU = Forecast

#fitting an ARMA(3,3) on TLT:
TLT_arma <- arma(rets$returns_TLT, order=c(3,3)) 
summary(TLT_arma) #could we consider adding lags?
#Coefficient(s):
#            Estimate   Std. Error t value Pr(>|t|)   
#ar1        0.4217598   0.4305785    0.980 0.327324     Not Significant, not use
#ar2       -0.8130351   0.2424621   -3.353 0.000799 *** This is significant, use
#ar3       -0.0104096   0.4201336   -0.025 0.980233     Not Significant, not use
#ma1       -0.4571925   0.4397162   -1.040 0.298459     Not Significant, not use
#ma2        0.7916448   0.2516055    3.146 0.001653 **  This is significant, use
#ma3       -0.0379606   0.4236513   -0.090 0.928602     Not Significant, not use
TLT_arima <- arima(rets$returns_TLT, 
                    order=c(3,0,3)) 
predict(TLT_arima, n.ahead =3)
##   Day 1            Day2         Day 3
#-0.0005994251 -0.0004887077  0.0004883237 lose money first 2 days, gain 3rd day
# 0.009612969    0.009615835   0.009622307 error(risk) is a bit less than +/-1%
#as a result, Google forecast has about twice the range of TLT

#fitting an ARMA(7,1) on VNQ
VNQ_arma <- arma(rets$returns_VNQ, order=c(7,1)) 
summary(VNQ_arma) #model is quite garbage? 
#ar1,2,3,4,7 are all significant, 3 is not too bad because around 0.15
VNQ_arima <- arima(rets$returns_VNQ, 
                   order=c(7,0,1)) 
predict(VNQ_arima, n.ahead =3)
# 0.0013330391 0.0014270916 0.0001727919 Looks like high return
# 0.01940832   0.01970384   0.01970651  Risk is higher than others

#######################################################################
#### Fitting ARCH on daily ROR stationary time series      ############
#### NOTE : look into the GARCH model - it performs better ############
#######################################################################

# we use ARCH and GARCH when we think that volatility (sigma) differs over time
#do you see seasons with higher and lower volatility?
#let's take a look at the daily plot of GOOG returns: 

ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_GOOG))
#where can you see periods of increased volatility? 
# if we see these periods of high and low vol, we should use a conditional heteroskedasticity model

#we will use a simple GARCH model to estimate the GARCH and ARCH models:
#install.packages("rugarch")
library(rugarch)
model_param <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model= list(model="sGARCH", garchOrder=c(5,5)),
                          distribution.model="norm") #you can change the distribution model to t-student 
garch_model <- ugarchfit(data=rets$returns_GOOG,
                         spec=model_param, out.sample = 20)

print(garch_model)
#Individual Statistics:  These are our coefficients, but p value very bad
#mu     0.03440
#omega  0.10317
#alpha1 0.07432
#alpha2 0.17706
#alpha3 0.22023
#alpha4 0.17162
#alpha5 0.35055
#beta1  0.19074
#beta2  0.23229
#beta3  0.20298
#beta4  0.20924
#beta5  0.22146
model_param <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model= list(model="sGARCH", garchOrder=c(1,1)),
                          distribution.model="norm") #you can change the distribution model to t-student 
garch_model <- ugarchfit(data=rets$returns_GOOG,
                         spec=model_param, out.sample = 20)
print(garch_model) #GARCH does not print ***s, but p values look good, can play around the garchOrder
#what option traders do, different GARCH models

## forecasting a GARCH model is called Bootstrapping GARCH :) 
bootstrap <- ugarchboot(garch_model, method = c("Partial", "Full")[1],
                   n.ahead = 500, n.bootpred = 500)
print(bootstrap)# this output will give us both, forecasted returns 
#and will give us forecasted sigma (volatility), significantly higher than the fixed income TLT below
#variant is more precise than the other model

###############################################
##############################################
# doing the same GARCH for the next ticker:: 
################################################

garch_model <- ugarchfit(data=rets$returns_TLT,
                         spec=model_param, out.sample = 20)

print(garch_model) #high dependency on previous variant (beta is bigger)


## Forecasting a GARCH model is called Bootstrapping GARCH :) 
bootstrap <- ugarchboot(garch_model, method = c("Partial", "Full")[1],
                        n.ahead = 500, n.bootpred = 500) #2 years out
print(bootstrap)# this output will give us both, forecasted returns 
#and will give us forecasted sigma (volatility)
#sigma shows the volatility is less than 1% for the first ten days

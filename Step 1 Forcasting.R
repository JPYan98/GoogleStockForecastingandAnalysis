#################################################################
######### This script shows different viz for time series #######
#################################################################
#you will need the joined_monthly_returns data frame and other 
#data frame and UDFs from previous scripts
#if you don't have your objects in the environment, load it using source()
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
#is this non-stationary, semi-stationary, strong-stationary? 

ggplot(data=prices)+
  geom_line(aes(x=Date, y=TLT.Adjusted))
#is this non-stationary, semi-stationary, strong-stationary? 

ggplot(data=prices)+
  geom_line(aes(x=Date, y=VNQ.Adjusted))
#is this non-stationary, semi-stationary, strong-stationary? 

#let's take a look at all securities together.....
compare_chart <- ggplot(data=prices)+
  geom_line(aes(x=Date, y=GOOG.Adjusted), color="blue")+
  geom_line(aes(x=Date, y=TLT.Adjusted), color="red")+
  geom_line(aes(x=Date, y=VNQ.Adjusted), color="green4")
print(compare_chart)
#does this chart make any sense? Can we easily compare the securities? 

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
#is this non-stationary, semi-stationary, strong-stationary? 

ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_TLT))
#is this non-stationary, semi-stationary, strong-stationary? 

ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_VNQ))
#is this non-stationary, semi-stationary, strong-stationary? 


#let's take a look at all securities together.....
compare_ror <- ggplot(data=rets)+
  geom_line(aes(x=Date, y=returns_GOOG), color="blue")+
  geom_line(aes(x=Date, y=returns_TLT), color="red")+
  geom_line(aes(x=Date, y=returns_VNQ), color="green4")
print(compare_ror)
#does this chart make any sense? Can we easily compare the securities? Which time series have stronger stationarity


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
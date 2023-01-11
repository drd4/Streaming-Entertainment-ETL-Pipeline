### March 24 2021
### Frameworks II Assignment 4: Time Series


#################################################################

## Section 1 ##

#################################################################

# Plotting
library(ggplot2)
library(ggthemes)
library(gridExtra)

# XTS
library(quantmod)
library(xts)
library(zoo)

library(forecast)
# datasets
library(fpp)
library(fpp2)

# statistical tests
library(tseries)
library(tidyverse)



#Question 1: What type of data structure is goog?
goog = readRDS('goog.RDS')

class(goog)

#xts


#Question 2: What was googles stock price June 2010


goog["201006"]

#221.0374

#Question 3: Using the monthly stock price for google, what is the average stock price for the year 2010?

mean(goog["2010"])

#260.9768


#Question 4: how many months

#142 months
length(goog)

#Question 5:  What is the correlation between google stock price and one-month lagged stock price? You can use lag() to obtain a one-month lag for google stock price. When computing correlation with cor(), be sure to set use='complete.obs'.

goog_lag <- lag.xts(goog, k =1)

#cor between lag & stock price
cor(goog, goog_lag, use = 'complete.obs')

#0.9923893


#Question 6: In order to have access to a wider array of forecasting models, we will convert the data to a "ts" data type. Also, we will split the data into a train and test sample, using the train sample to estimate a model and the test sample to evaluate it. We will used data from Jan, 2007 to Dec, 2015 for the train sample and the rest for the test sample. The code below will convert goog to a “ts” object and split the data.


google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))

length(train)

#108


#Question 7: Autocorrelation examines correlation of a variable and its lagged values. Construct a plot of autocorrelations for train using ggAcf() from the forecast package. Which lag has the strongest autocorrelation?
ggAcf(google)

#1



#################################################################

## Section 2 ##

#################################################################


#Question 1:  Use the average to make a prediction for the stock price over the 34 months of the test sample. Let's call this average_model. What is the point forecast of the stock price for October 2018?

average_model = meanf(train, h = 34)
average_model

#355.776


#Question 2:  Let us examine the accuracy of the above prediction from average_model on the train sample. 
#Specifically, what is the RMSE of the prediction in the train sample? Hint: Use accuracy() from library(forecast)

accuracy(average_model)


#Question 3:  RSME test sample

accuracy(average_model, x  = test)

#588.3496

#Question 4:  Next, let us examine another simple prediction, one that assumes the future will be the same as the last observation. Let’s call this naive_model. Use naive_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?

naive_model = naive(train, h = 34)
naive_model
#758.88

#Question 5: What is the RMSE of the naive_model on the test sample?

accuracy(naive_model, x = test)
#230.50860


#################################################################

## Section 3 ##

#################################################################

#Question 1:  Let us fit an exponential smoothing model using the following function:
#Call this ets_model.
#errors of ets model are
ets_model = ets(train, model= 'AAA')
ets_model

#additive

#Question 2: trend for ets_model 
#additive


#Question 3:  What is AICc for ets_model?

summary(ets_model)

#1255.624

#Question 4:  Do the residuals look like white noise? To answer this question, examine an Acf() or ggAcf() plot and the result of the Ljung-Box test.
checkresiduals(ets_model) #not white noise

#Question 5: Use ets_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?

ets_model_forcast = forecast(ets_model, h = 34)
ets_model_forcast

#1028.4942

#Question 6: RMSE ets_model on test sample

accuracy(ets_model_forcast, x = test)

#102.20154




#################################################################

## Section 4 ##

#################################################################


#Question 1: How many ordinary autoregressive lag variables have been used in auto_arima_model?

auto_arima_model = auto.arima(train)
summary(auto_arima_model)



#0

#Question 2: ordinary differences
#1

#Question 3: moving averages
#0

#Question 4: Seasonal
#0

#Question 5: White noise? 
checkresiduals(auto_arima_model)

#Yes

#Question 6:   Use auto_arima_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?

auto_forcast = forecast(auto_arima_model, h = 34)
auto_forcast

#920.8568

#Question 7:  RMSE of auto_arima model
accuracy(auto_forcast, x = test)

#143.69172

#Question 8: BoxCox.lambda() is a handy function for identifying the optimal value of lambda to stabilize variance. What is the optimal value of lambda?

c = BoxCox.lambda(train)
c

#.4748787

#Question 9:  AICc for arima model 
arima_model = Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))
arima_model
#343.16

#Question 10: LJ BOX TEST

checkresiduals(arima_model, lag = 24)

#Yes

#Question 11:  Use arima model to forecast stock price, 34 months of test sample
#point forecast of stock price for 10/18

model2 = forecast(arima_model, h = 34)
model2
#1165.1741


#Question 12: RSME
accuracy(model2,x = test)

#56.19252
### Time Series Analysis ### 


library(ggplot2)
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast)
library(tseries) 
library(tidyverse)
library(forecast)

#getwd()
#setwd("/Users/danieldasgupta/Desktop/COLUMBIA SPRING 2021/frameworks/wd/btcproject")

### Percent Change Time Series 
btc_pct = read.csv("pct_change.csv")

btc_pct$date = as.Date(btc_pct$date, format = "%m/%d/%y")
str(btc_pct)

btc_pct1 = btc_pct %>%
  select(date, btc_pc)

# Convert DF to XTS, TS


btc_xts <- xts(btc_pct1[,'btc_pct'], btc_pct1[,'date'])
plot(btc_xts)

ggAcf(btc_xts)

#Convert xts to ts for forecasting 

library(TSstudio)
btc_ts <- xts_to_ts(btc_xts, start = as.Date("2017-08-02"), frequency = 365 )

ggAcf(btc_ts)
#Train and Test

library(TSstudio)
length(btc_ts)    #1304 days collected, test should be final 30%

.30 * 1304 #391

set.seed(1738)
split_btc <- ts_split(ts.obj = btc_ts, sample.out = 391)

train <- split_btc$train
test <- split_btc$test

length(btc_ts)

length(train)
length(test)

# Auto ARIMA Model

model_auto = auto.arima(y= train)
auto_forecast = forecast(model_auto, h = 391)
accuracy(auto_forecast, test)


# Average Model

average_model = meanf(train, h= 391)
average_model

accuracy(average_model, x= test)



# Naive Model
naive_model = naive(train, h = 391)
accuracy(naive_model, x = test)


# Drift Model

drift_model = rwf(train, h = 391, drift = T)
accuracy(drift_model, x = test)

# Simple Expoential Smoothing

ses_model = ses(train, h = 391)
accuracy(ses_model, x = test)


# Holts method

holt_model = holt(train, h = 391)
accuracy(holt_model, x = test)

# Holts Method with Damping

holt_damped_model = holt(train, h = 391, damped = T)
accuracy(holt_damped_model, x = test)




# Look at the RMSE of all the models used 
rbind(average_model = accuracy(average_model,x = test)[2,],
      naive_model = accuracy( naive_model,x = test)[2,],
      drift_model = accuracy( drift_model,x = test)[2,],
      ses_model = accuracy( ses_model,x = test)[2,],
      holt_model = accuracy( holt_model,x = test)[2,],
      holt_damped_model = accuracy(holt_damped_model,x = test)[2,],
      auto_arima = accuracy(auto_forecast,x=test)[2,]
)


# Graph Models  
autoplot(train, color='black')+
  autolayer(test,size=1.05,color='lightsalmon2')+
  autolayer(holt_model,series = 'Holt',PI=F)+
  autolayer(average_model,series = 'Average Model',PI=F)+
  autolayer(naive_model,series = 'Naive Model',PI=F)+
  autolayer(auto_forecast,series = 'Auto ARIMA',PI=F) +
  autolayer(ses_model, series = 'SES Model', PI = F) + 
  autolayer(holt_damped_model, series = 'Holt Damped Model', PI = F) +
  autolayer(drift_model, series = 'Drift Model', PI = F)





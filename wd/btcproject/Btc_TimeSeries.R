
#####################################################
#Data wrangling
library(tidyverse)

#Time-Series Data
library(xts)
library(zoo)
library(quantmod)

getwd()
setwd("/Users/danieldasgupta/Desktop/COLUMBIA SPRING 2021/frameworks/wd/btcproject")
data <- read.csv('rawbtc.csv', stringsAsFactors = FALSE)


data = na.omit(data)

#Extract only relevant data
btc_data_not_clean <- data %>% 
  select(date, PriceUSD)

btc_data_not_clean$date <- as.Date(btc_data_not_clean$date)

head(btc_data_not_clean)
#Subset to Feb 2017 (arbitrary)
btc_data = filter(btc_data_not_clean, between(date, as.Date("2017-02-02"), as.Date("2020-10-17")))

library(TSstudio)
btc_ts <- xts_to_ts(btc_xts, start = as.Date("2017-08-05"), frequency = 365 )

plot(btc_ts)


#Convert the date column into date data type
btc_data$date <- as.Date(btc_data$date)

str(btc_data)


btc_data
#Convert the data into time-series data d d
btc_xts <- xts(btc_data[,'PriceUSD'], btc_data[,'date'])

#First day of complete data 7/18/2011 - starting point
btc_ts <- as.ts(btc_xts)



btc_df <- as.data.frame(btc_ts_data)

## Top Autocorrelations 
library(forecast)
ggAcf(x = btc_ts)


library(TSstudio)
# Split into Train and Test
length(btc_ts)    #3511 days collected, test should be final 30%

.30 * 1354 #406

set.seed(1738)
split_btc <- ts_split(ts.obj = btc_ts, sample.out = 406)

train <- split_btc$train
test <- split_btc$test

length(btc_ts)

length(train)
length(test)

### AVERAGE MODEL

average_model = meanf(train,h = 406 )
average_model

average_model$mean

accuracy(average_model, x = btc_ts)
#RMSE on Test 9746


autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)



## Naive Model

naive_model = naive(train, h = 406)
accuracy(naive_model, x= btc_ts)
#RMSE 8828
autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(test)


## Seasonal Naive Method

seasonal_naive_model = snaive(train,h =406)
accuracy(seasonal_naive_model, x= btc_ts)
#RMSE 8828

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(test)



## Drift 
drift_model = rwf(train, h = 406, drift = T)
accuracy(drift_model, x= btc_ts)
#RMSE 7912
autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(drift_model,PI=F,size=1.1,series='Drift Model')+
  autolayer(test)

# Exponential Smoothing

#Simple Exp Smoothing

ses_model = ses(train, h = 406)
accuracy(ses_model, x= btc_ts)
#RMSE 8828
autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(test)


## holt model
holt_model = holt(train, h = 406)
accuracy(holt_model, x = btc_ts)
#RMSE 7984

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(test)


## holt model with damping

holt_damped_model = holt(train, h = 406, damped = T)
accuracy(holt_damped_model, x = btc_ts)
#RMSE 8845

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(holt_damped_model,series="Holt's Method with Damping",PI=F,size=1.1)+
  autolayer(test)



## ARIMA Model

model_auto= auto.arima(y = train, d=1, D=1, stepwise = F, approximation = F)
model_auto

model_auto_forecast = forecast(model_auto, h = 406)
accuracy(model_auto_forecast, x = btc_ts)
# RMSE 8840


## SUMMARY OF MODELS
rbind(average_model = accuracy(object = average_model,x = btc_ts)[2,],
      naive_model = accuracy(object = naive_model,x = btc_ts)[2,],
      seasonal_naive_model = accuracy(object = seasonal_naive_model,x = btc_ts)[2,],
      drift_model = accuracy(object = drift_model,x = btc_ts)[2,],
      ses_model = accuracy(object = ses_model,x = btc_ts)[2,],
      holt_model = accuracy(object = holt_model,x = btc_ts)[2,],
      holt_damped_model = accuracy(object = holt_damped_model,x = btc_ts)[2,],
      arima = accuracy(model_auto_forecast,x=btc_ts)[2,]
)


autoplot(train, color='sienna')+
  autolayer(test,size=1.05,color='seagreen2')+
  autolayer(average_model,series = 'Average Model',PI=F)+
  autolayer(naive_model,series = 'Naive Model',PI=F)+
  autolayer(seasonal_naive_model,series = 'Seasonal Naive Model',PI=F)+
  autolayer(drift_model,series = 'Seasonal Naive Model',PI=F)+
  autolayer(ses_model,series = 'Seasonal Naive Model',PI=F)+
  autolayer(holt_model,series = 'Holt',PI=F)+
  autolayer(model_auto_forecast,series = 'ARIMA',PI=F)

\\


###

data <- read.csv('rawbtc.csv', stringsAsFactors = FALSE)


data = na.omit(data)

#Extract only relevant data
btc_data_not_clean <- data %>% 
  select(date, PriceUSD)

btc_data_not_clean$date <- as.Date(btc_data_not_clean$date)


#Subset to Aug 8 2017 to Jan 5 2018 (arbitrary)
btc_data = filter(btc_data_not_clean, between(date, as.Date("2017-08-05"), as.Date("2018-01-05")))

head(btc_data)


#Convert the date column into date data type
btc_data$date <- as.Date(btc_data$date)


#Convert DF to xts 
btc_xts <- xts(btc_data[,'PriceUSD'], btc_data[,'date'])

plot(btc_xts)

#Convert xts to ts for forecasting 

library(TSstudio)
btc_ts <- xts_to_ts(btc_xts, start = as.Date("2017-08-05"), frequency = 365 )

plot(btc_ts)

length(btc_ts)

count_ma = ts(btc_ts, frequency = 365, start = c(2017, 08))
arima = auto.arima(count_ma)
arima <- forecast(arima, h = 154)
accuracy(arima)
plot(arima)
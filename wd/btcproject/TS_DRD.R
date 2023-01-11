#Data wrangling
library(tidyverse)
library(forecast)
#Time-Series Data
library(xts)
library(zoo)
library(quantmod)
library(ggplot2)


getwd()
setwd("/Users/danieldasgupta/Desktop/COLUMBIA SPRING 2021/frameworks/wd/btcproject")

### Percent Change 
btc_pct = read.csv("pct_change.csv")

btc_pct$date = as.Date(btc_pct$date, format = "%m/%d/%y")
str(btc_pct)

btc_pct1 = btc_pct %>%
  select(date, abs_btc_pc)

btc_pct1
plot(btc_pct1)

btc_pct1 %>%
  ggplot(aes(x = date, y = abs_btc_pc)) + geom_line()

head(tot)
tot = read.csv("totalbtc.csv")

btc_pct2 = tot %>%
  select(Date, Abs_Pct_Change)
str(btc_pct2)
btc_pct2$Date <- as.Date(btc_pct2$Date, format = "%m/%d/%y")
btc_pct2 %>%
  ggplot(aes(x = Date, y = Abs_Pct_Change)) + geom_line()


btc_pct3 = tot %>%
  select(Date, Price)
str(btc_pct3)
btc_pct3$Date <- as.Date(btc_pct3$Date, format = "%m/%d/%y")

price_chart <- btc_pct3 %>%
              ggplot(aes(x = Date, y = Price)) + geom_line(color = "blue") + 
                geom_vline(xintercept = as.numeric(as.Date("2018-08-05")), 
                    color = "red", 
                     lwd = 1,
                      linetype = "dashed")  + 
             geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), 
             color = "red", 
             lwd = 1,
             linetype = "dashed") + 
  ggtitle("Bitcoin Price Date Selection") +
  xlab("Date (Year)") + ylab("Price (USD)") + ylim(0,45000)
price_chart

price_chart_a <- price_chart + geom_vline(xintercept = as.numeric(as.Date("2018-08-05")), 
                         color = "red", 
                         lwd = 1,
                         linetype = "dashed") 

price_chart_b <- price_chart_a + geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), 
                    color = "red", 
                    lwd = 1,
                    linetype = "dashed") + ggtitle("Bitcoin Price Date Selection") + xlab("Date (Year)")

price_chart_b






btc_pct4 = tot %>%
  select(Date, Pct_Change)
str(btc_pct4)
btc_pct4$Date <- as.Date(btc_pct4$Date, format = "%m/%d/%y")
btc_pct4 %>%
  ggplot(aes(x = Date, y = Pct_Change)) + geom_line()





price_chart <- btc_pct4 %>%
  ggplot(aes(x = Date, y = Pct_Change)) + geom_line(color = "dodgerblue") + 
  geom_vline(xintercept = as.numeric(as.Date("2018-08-05")), 
             color = "red", 
             lwd = 1,
             linetype = "dashed")  + 
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), 
             color = "red", 
             lwd = 1,
             linetype = "dashed") + 
  ggtitle("Bitcoin Price Date Selection") +
  xlab("Date (Year)") + ylab("Price Change (%)") 
price_chart
# Convert DF to XTS, TS


btc_xts <- xts(btc_pct1[,'abs_btc_pc'], btc_pct1[,'date'])
plot(btc_xts)

  
#Convert xts to ts for forecasting 

library(TSstudio)
btc_ts <- xts_to_ts(btc_xts, start = as.Date("2017-08-02"), frequency = 365 )
plot(btc_ts)

quantile()


#LAG
library(forecast)
Acf(x = btc_xts, plot = F)


# Split into Train and Test

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

model_auto = auto.arima(y= train)
auto_forecast = forecast(model_auto, h = 391)
accuracy(auto_forecast, test)


autoplot(train)+
  autolayer(auto_forecast,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)


# AVERAGE MODEL 

average_model = meanf(train, h= 391)
average_model

accuracy(average_model, x= test)


autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)

# Naive Model
naive_model = naive(train, h = 391)
accuracy(naive_model, x = test)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(test)


# Drift Model

drift_model = rwf(train, h = 391, drift = T)
accuracy(drift_model, x = test)


autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(drift_model,PI=F,size=1.1,series='Drift Model')+
  autolayer(test)


# Exponential Smoothing Models

# Simple Expoential Smoothing

ses_model = ses(train, h = 391)
accuracy(ses_model, x = test)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(test)

# Holts method

holt_model = holt(train, h = 391)
accuracy(holt_model, x = test)

# Holts Method with Damping

holt_damped_model = holt(train, h = 391, damped = T)
accuracy(holt_damped_model, x = test)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.5)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(holt_damped_model,series="Holt's Method with Damping",PI=F,size=1.1)+
  autolayer(test)

#ETS - having problems with frequency... 

#### Testing with Mean Price Quarter

data = read.csv("btc_meanprice_quarter.csv")
str(data)
q = data %>%
  select(Qtr1, Qtr2, Qtr3, Qtr4)

ts = ts(q, start = 2011, frequency = 4)
ts
length(ts)    #1304 days collected, test should be final 30%

y <- ts(c(9.069318,  3.275742, 5.388313, 5.365187,10.143223,12.204860,
        33.359630 , 117.010124,   104.457589,   496.442899,
          691.819724  ,521.897972,   533.694225,   356.869368,
        251.998410,  236.917987  , 255.401496,   346.357733,
           409.898158,  513.258261,   616.123532,   731.193224,
     1035.135931, 1907.794882  ,3488.793833,  9459.216570,
        10460.901180 ,7744.418058  ,6797.769327,  5139.993993,
           3750.219822, 7277.417945 ,10366.434970,  7987.214848,
           8269.379789, 8662.615609 ,10630.716020, 16855.867740,
          39942.219390), start = c(2011, 3), frequency = 4)

y

class(y)

autoplot(y)




set.seed(1738)
split_btc <- ts_split(ts.obj = btc_ts, sample.out = 391)

train <- split_btc$train
test <- split_btc$test

length(btc_ts)

length(train)
length(test)

autoplot(y)
# ARIMA 
model_auto = auto.arima(y = train, d = 1, D = 1, stepwise = F, approximation = F)
model_auto

accuracy(model_auto, test)


rbind(average_model = accuracy(f = average_model,x = test)[2,],
      naive_model = accuracy(f = naive_model,x = test)[2,],
      drift_model = accuracy(f = drift_model,x = test)[2,],
      ses_model = accuracy(f = ses_model,x = test)[2,],
      holt_model = accuracy(f = holt_model,x = test)[2,],
      holt_damped_model = accuracy(f = holt_damped_model,x = test)[2,],
      arima = accuracy(model1_forecast,x=test)[2,]
)


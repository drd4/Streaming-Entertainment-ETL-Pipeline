#Section 1

#load data
goog = readRDS('goog.RDS')

#data strcut
class(goog)

#Google stock June 2010
goog["201006"]

#mean of 2010 stock
mean(goog["2010"])

#how many months of data
length(goog)

#One month lag
goog_lag <- lag.xts(goog, k =1)

#cor between lag & stock price
cor(goog, goog_lag, use = 'complete.obs')

#data split
google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))

#train length
length(train)

#autocorrelation plot
ggAcf(google)


###################################################
#Section 2

average_model <- meanf(train, h = length(test))

#accuracy
accuracy(average_model) 
accuracy(average_model, x = test)

#naive mdodel
naive_model <- naive(train, h = length(test))
naive_model
#accuracy
accuracy(naive_model, x=test)


##############################
#Section 3
ets_model <- ets(train,model = 'AAA')

#erros are additive, trend is additive
summary(ets_model)

#check residuals
checkresiduals(ets_model) #not white noise

ets_model_forecast <- forecast(ets_model, h = length(test))
ets_model_forecast

accuracy(ets_model_forecast, x = test)

##############################
#Section 4

# 0,1,0,0

#auto
auto_arima_model <- auto.arima(train)
summary(auto_arima_model)
checkresiduals(auto_arima_model)

#forecast using auto
model1_forecast = forecast(auto_arima_model, h=length(test))
accuracy(model1_forecast,x = test)

#optimal lambda
c <- BoxCox.lambda(train)

#manual creation
arima_model <- Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))
checkresiduals(arima_model, lag = 24)

# forcast of manual
model2_forecast <- forecast(arima_model, h=length(test))
accuracy(model2_for2cast,x = test)

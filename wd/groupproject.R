install.packages("crypto")
devtools::install_github("jessevent/crypto")
library(crypto)
show_me_everyone_getting_rich <- crypto_global_market()


crypto_global_market(market = 'total')

# cluster with traditional financail metrics, 

#coin market cap API - market cap, transaction volume

#features based on instrinic properties of the coin,
# so purpose, 
# 5 classes
# btc is cpyto
# eth is virtual machine
# buzz on twitter - text mining 

# use clusters to see what are the best performing types of groups of coins

## which coins are 

#predicitng price 


# not just a prediction of price,  want to see what characteristics predict higher growth 

#past three months, BTC has gone up x %, trying to predict next growth % 
# focus on a specific coin or two

# not interested in straight prediction, what is actually driving the change of bitcoin

# clustering is often driving by some latent characteristics that come from theory

# features, some theory that there are some latent groups that need to be identify 

# what really is the end goal- is it we want to know the price of btc tomorrow or what is driving the price of btc tomorrow

# or what makes a solid cyprto, doesnt matter price, growth is a better thing 


# what method or methods are best predictors of x coin 
#or pick one coin,  what method is best at predicting price, moving average, deep learning model

# basically build a tool that actually predicts price of bitcoin, using linear regresioon to extract what is driving growth 
# deep learning 

# jsut for the proposal, dataset with price of bitcoin over past two years, data fromt witter and do cleaning 
#the data
# code to clean 


install.packages('dygraphs')

library(shiny)
library(coindeskr)
library(dygraphs)


last31 <- get_last31days_price()


ui <- 
  fluidPage(
    titlePanel('Bitcoin USD Price for Last 31 days'),
    mainPanel(
      h3('Minimum'),
      h3(htmlOutput('minprice')),
      h3('Maximum'),
      h3(htmlOutput('maxprice')),
      dygraphOutput("btcprice")
    )
  )

server <- function(input,output){
  
  output$minprice <- renderText(
    paste('Price : $', min(last31), '<br>Date :', rownames(last31)[which.min(last31$Price)] )
  )
  
  
  output$maxprice <- renderText(
    paste('Price : $', max(last31), '<br>Date :', rownames(last31)[which.max(last31$Price)] )
  )
  output$btcprice <- renderDygraph(
    dygraph(data = last31, main = "Bitcoin USD Price for Last 31 days") %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyRangeSelector()
  )
}
shinyApp(ui, server)




library(crypto)
library(tidyverse)

list_coins<-crypto_list()
# Print 10 of them
list_coins%>%head(10)%>%select(symbol, name)


btc<-crypto_history(coin = 'BTC', start_date = "20210223")
eth<-crypto_history(coin = 'ETH', start_date = "20210101")
ltc<-crypto_history(coin = 'LTC', start_date = "20210101")
df<-rbind(btc,eth,ltc)
df%>%head()







install.packages("coinmarketcapr")
library(coinmarketcapr)
get_api_info()
listing <- get_crypto_listings(currency = "USD", latest = TRUE)

setup(api_key = "142f3ef6-9039-4730-8d75-d7ac7f240eba")

?coinmarketcapr


#############3
install.packages("tidyquant")
library(tidyquant)

getSymbols("GME", from = '2020-01-01', 
           to = '2021-02-20', warnings = FALSE,
           auto.assign = TRUE)
head(GME)




###########################################################################3

setwd("/Users/danieldasgupta/Desktop/COLUMBIA SPRING 2021/frameworks/wd/btcproject")

#Read in Raw CSV 

rawdata = read.csv("rawbtc.csv")

str(rawdata)

btc_df <- na.omit(btc_df)


#Subset data into new df, 'cleandata'
library(tidyverse)
btc_df = rawdata %>%
  select(date,AdrActCnt, BlkCnt, BlkSizeByte, BlkSizeMeanByte, CapMrktCurUSD, CapRealUSD, DiffMean,
         FeeMeanUSD, FeeMedUSD, FeeTotUSD, HashRate, IssContPctAnn, IssContUSD, IssTotUSD, PriceUSD,
         ROI1yr, ROI30d, SplyCur, SplyFF, TxCnt, TxTfrCnt, TxTfrValAdjUSD, TxTfrValMeanUSD, TxTfrValMedUSD,
         TxTfrValUSD, VtyDayRet180d, VtyDayRet30d, VtyDayRet60d)

df = rawdata %>%
  select(date,AdrActCnt, BlkCnt, BlkSizeByte, BlkSizeMeanByte, CapMrktCurUSD, CapRealUSD, DiffMean,
         FeeMeanUSD, FeeMedUSD, FeeTotUSD, HashRate, IssContPctAnn, IssContUSD, IssTotUSD, PriceUSD,
         ROI1yr, ROI30d, SplyCur, SplyFF, TxCnt, TxTfrCnt, TxTfrValAdjUSD, TxTfrValMeanUSD, TxTfrValMedUSD,
         TxTfrValUSD, VtyDayRet180d, VtyDayRet30d, VtyDayRet60d)

#Remove NA/Empty Rows
df <- na.omit(df)


#Test 
is.na(cleandata)

#Export CSV
write.csv(cleandata, "cleandata.csv")


## Visualize some data
head(rawdata)

library(ggplot2)
ggplot(data=rawdata,aes(x=date))+
  geom_histogram(binwidth=5000,fill='cadetblue')

library(knitr)

#Identify 

missing.values <- df %>%
  group_by(year = year(date)) %>%
  summarise(avg = mean(PriceUSD))


missing.values <- df %>%
  group_by(year = year(date)) %>%
  summarise(c = colSums(is.na(.)))


%>%
  summarise(colSums(is.na(df)))
           
           
           
           , PriceUSD) %>%
  summarise(count = colSums(is.na(df))) 



  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values  %>% kable()

plot(missing.values)

ggplot(missing.values, aes(x = key)) +
  geom_

df$date <- as.Date(df$date, "%d/%m%Y")
na.omit(rawdata )
df %>%
  mutate(year= year(date)) %>%
  group_by(year) %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val))



%>%
  group_by(month, year) %>%
  mutate(is.missing = is.na(df[1:3])) %>%
  group_by(is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

df

df <- na.omit(df) 

ggplot(df, aes(x=date, y = PriceUSD)) +
  geom_line()

df

   summarise(num.missing = sum(value))

plot(df$date, df$PriceUSD)

##
library(lubridate)
library(gridExtra)
library(scales)


str(btc_df)
rawdata = read.csv("rawbtc.csv")

btc_df$date = as.Date(btc_df$date)


qplot(x = date, y = PriceUSD,
      data = btc_df)


install.packages("VIM")
library(imputeTS)

ggplot_na_distribution(rawdata)



set.seed(2)
mnar_data <- data.frame(x = rnorm(100), y = rnorm(100)) %>%
  mutate(y_miss = ifelse(y > 1, y, NA),
         y = ifelse(is.na(y_miss), y, NA),
         x_miss = ifelse(is.na(y), x, NA))

library(VIM)
aggr(rawdata, numbers= TRUE)

t = rawdata
apply(t, 2, function(x) any(is.na(x)))
indx <- apply(t, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(indx)
indx


month(df$date)

head(df)
df %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(PriceUSD))

df %>%
  seq(as.Date("2009-01-03"), as.Date("2021-02-23"), by = "quarter")




df %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"))



#######





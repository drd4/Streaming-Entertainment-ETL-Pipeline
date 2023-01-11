#Data wrangling
library(tidyverse)
library(forecast)
#Time-Series Data
library(xts)
library(zoo)
library(quantmod)
library(ggplot2)

## Graphs

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





tot = read.csv("totalbtc.csv")

btc_pct2 = tot %>%
  select(Date, Abs_Pct_Change)
str(btc_pct2)
btc_pct2$Date <- as.Date(btc_pct2$Date, format = "%m/%d/%y")
btc_pct2 %>%
  ggplot(aes(x = Date, y = Abs_Pct_Change)) + geom_line(color = "gold3")



btc_pct3 = tot %>%
  select(Date, Price)
str(btc_pct3)
btc_pct3$Date <- as.Date(btc_pct3$Date, format = "%m/%d/%y")
btc_pct3 %>%
  ggplot(aes(x = Date, y = Price)) + geom_line()



btc_pct4 = tot %>%
  select(Date, Pct_Change)
str(btc_pct4)
btc_pct4$Date <- as.Date(btc_pct4$Date, format = "%m/%d/%y")
btc_pct4 %>%
  ggplot(aes(x = Date, y = Pct_Change)) + geom_line()


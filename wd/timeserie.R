##TIME SERIES ##

library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling


grid.arrange(autoplot(EuStockMarkets[,'FTSE'])+xlab('')+ylab('FTSE Index'),
             autoplot(JohnsonJohnson)+xlab('')+ylab('J&J Sales'),
             autoplot(maxtemp)+xlab('')+ylab('Max temp'),
             autoplot(Seatbelts[,'DriversKilled'])+xlab('')+ylab('Seatbelt Casualties'))


#UNIVARIATE TIME SERIES


class(AirPassengers)

AirPassengers
autoplot(AirPassengers)
60*12


getwd()
setwd("/Users/danieldasgupta/Desktop/COLUMBIA SPRING 2021/frameworks/wd/exam")


#### Section 1 Cluster Analysis #######
####################

#Fast food survey

#### Question 1: ####

#clustering algos can be broadly categorized into distance based methods and model based methods.  
# describe distance based methods

# A - 

#Distance based methods such as k-means and Hierachical clustering compute the similarity between observations.
#These methods finds groups while maximizing the distance from members of other groups.  


### Question 2: ### read data into-  are there missing values?  why is important to impute missing values before cluster analysis

fastfood = read.csv("fastfood_survey_exam_prep.csv")

head(fastfood)

sum(is.na(fastfood))

#There are no missing values.  Since clustering algorithms use data on all variables, a missing observation on one variable will cause the entire row of data to be ignored for analysis.  



#Question 3: in the space below, enter 4 code used to answer the previous questoin

fastfood = read.csv("fastfood_survey_exam_prep.csv")

head(fastfood)

sum(is.na(fastfood))

#Question 4:  For distanced based clustering methods, the scale of the variable affects the weight
# assigned to the variable.  Therefore, it is vest to standardize variables.
#there are many ways to standardize a variable.  standardize each variable in the dataset by
#subtracting the mean and dividing by the SD. call the new data fastfood_std

#what is the standardized value of "quality of fries"  from the first observation 

fastfood_std = scale(fastfood)
fastfood_std
fastfood_std
#0.1412111
#Question 5: post code used

d = read.csv("hw.csv")

#Question 6:  calc euclidean distance between all observations in standardized dataset
# conduction hierachial cluster analysis using ward.d2, and the compute the eucliean distance
#plot dendrogram
d_scale = scale(d)

distances = dist(fastfood_std, method = "euclidean")
clusters = hclust(d = distances, method = 'ward.D2')
plot(clusters)

plot(cut(as.dendrogram(clusters),h=10)$upper)

#How many clusters emerged from the data and justify your conclusion

#Two clusters emerged from the data.  After cutting the dendrogram to show only the trees above 10 to visualize the number of clusters better. By looking at the differences in height which indicates dissimplarity between elements, two clusters emerge.

#####Question 7: enter r code

#### Question 8: ####
#cacl cophenetic correlation for the model from previous question 

cor(cophenetic(clusters), distances)
####Question 9:#### enter r code



#### Question 10:####  
#if one used a three cluster solution, how many observations are in the smallest cluster

h_clusters = cutree(clusters, k = 3)
table(h_clusters)


#### Question 11: #### code

### Question 12: ####
#examine the profile of the resulting clustings/segments in terms of 
#clustering variables.  You can do so by combining the original dataset with the 
#cluster memberships from the three-cluster solution.  You can use cbind() to combine
##Compeare the largest cluster, call it segment 1, to the other two clusters in 
#terms of the clustering variables.  For which of the following variables is the score
#of segment 1 the highest


data2 = cbind(fastfood, h_clusters)

library(dplyr)
data2 %>%
  select(everything())%>%
  group_by(h_clusters)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(ggplot2)
library(tidyr)
data2 %>% 
  select(speed_of_service:taste_burgers, h_clusters) %>% 
  group_by(h_clusters) %>% 
  summarize_all(function(x) round(mean(x,na.rm=T),2)) %>%
  gather(key = var, value = value, speed_of_service:taste_burgers) %>%
  ggplot(aes(x = var, y = value, fill = factor(h_clusters))) + 
  geom_col(position = 'dodge') + 
  coord_flip()


data2=cbind(data,h_clusters)
data2 %>%
  select(everything())%>%
  group_by(h_clusters)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()
#all but drive_through
#### Question 13 ####: 
#enter code
###########



##### Section 2: Recommender Systems ####:

# use MoveLense dataset from recommberlab package

library(recommenderlab)
data(MovieLense)
MovieLense


### Question 14: ####
#briefly outline the process underlying collaborative filtering

# Collaborative filtering process requires one to load data, create a user-item matrix and standardize ratings.
# Then, one can calculate similartiy matrix, for example Euclidean Distance, and then predict unknown ratgins based on n nearest neighbors, from which a score is computed weighted by similarity to target user.  Finally can then suggest top k items.


### Question 15: ###
#how many movies did user 115 rate

nratings(MovieLense['115',])

### Question 16: ###
# code


####Question 17: ###
#What was the mean rating of movies by user 115
mean(getRatings(MovieLense['115',]))

#Questoin 18: code


### Questoin 19: ###
#set seed to 1706, split dataset, 80% in train using the sample() function
#how many rows in train


set.seed(1706)
split = sample(nrow(MovieLense),size = 0.8*nrow(MovieLense))
train = MovieLense[split,]
test = MovieLense[-split,]

dim(train)


str(train)
### Question 20 ###
#code

### Question 21 ###:

#construct a user based collaborative filtering recommender system using the train data
# set the parameter nn to 10, method to cosine, normalize to center
#call this recom_ubcf - to predict the top three recommended movies for each user in the test data

#what are the top three recommeded movies for user 115

recom_ubcf = Recommender(train, 
                         method='UBCF', 
                         parameter=list(method='cosine',nn=10, normalize='center'))


pred_ubcf_topN=predict(recom_ubcf,newdata=test,method='topNList',n=3)
getList(pred_ubcf_topN)['115']
# Question22 code:

#### Question 23: ####
#Based on the user-based collaborative filtering recommender, recom_ubcf
#that was constructed in the previous question, what is the predicted rating for the top reommended movie 
#for user 115
getRatings(pred_ubcf_topN)['115']

#Q24 Code

#pred_ubsf=predict(recom_ubcf,newdata=test,type="ratings")
pred_ratings =predict(recom_ubcf,newdata=test,method='ratings',n=3)
getRatings(pred_ratings)['115']


#### Question 25: ####
#One of the oldest approaches to recommendations invloved recommending the
# most popular item.  We still this in the form of Top 50 music hits, billboard top hits etc/

#the underlying assumption is that if most people like a movie, book or song, you will like it too

#Create a non-personalized recommendations with the POPULAR method using the train data.
#Set the normalize = cetner, (althought center is default).  Call this recommender recom_popular
#Use recom popular to predict the top 3 recommended movies for each user in the test data.  
#Based on the results, what are the top 3 recomended movies for user 115?

recom_popular = Recommender(train,
                            method = 'POPULAR',
                            parameter = list(normalize = 'center'))

pred_popular_topN = predict(recom_popular,newdata=test,type='topNList',n=3)
getList(pred_popular_topN)['115']


## Q26- code

###########



##### Section 3: Time Series  ####:

# use ausbeer.RDS - 
library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling

### Question 27: ####
#Briefly discuss the differences between Simple Exponential smoothing, holts method, and holts seasonal method

#Simple Exponential Smoothing forecasts are calculcated using weighted averages, but is only suitable for forecasting data with no clear trend or seasonal pattern.
#Holts method extends SES to allow the forecsating of data with a trend.
#holts Seasonal method - extends holts method to capture additive and multiplicative variations in trends.  


### Question 28: ####

#for the remaining question in this section that invovled analysis, use the dataset containing
#total quarterly beer production in Australia(in megaliters) from 1956 Q1 - 2010 Q2
#the data is in ausbeer RDS, and can be read using readRDS()- call it ausbeer

#The goal is to forecast beer consumption starting in 2005 using all historical data up
#until the end of 2004 in the ausbeer data.  To do this, split ausbeer data into train and test
#such that the train data ends on Q4 2004 and test begins on Q1 2005.  

#how many quarters are in the test data?


ausbeer=readRDS('ausbeer_test.RDS')
class(ausbeer)

train = window(ausbeer,start=c(1956,01),end=c(2004,04))
test = window(ausbeer,start=c(2005,01),end=c(2010,02))
length(test)


### Question 29: ####

#code

### Question 30: ####
#use the average to make a prediction of beer consumption over the test data.
#Call this average_model, what is the point forecast of beer consumption for Q1 2008?

average_model = meanf(train,h = 22) 
average_model

a = meanf(train, h = length(test))
a
### Question 31: ####

#code


### Question 32: ####
# Examine the accruacy of the above precdiction from average_model, on the train sample
#what is the RMSE of the prediction in the train sample

accuracy(average_model,x=ausbeer)


### Question 33: ####
# code

### Question 34: ####
# RMSE of average model on test sammple



### Question 35: ####

# r code

### Question 36: ####
# Use ARIMA model.  Since there are a large number of parameters with which to define ARIMA model, 
#lets use auto.arima to automatically determine the best paraemters.  Set it up to do an exhaustive 
#search by setting stepwise to F, and approximation to F. 
#Call this audio_arima, how many oridinary moving average lag variales have been used??

auto_arima_model=auto.arima(train, stepwise = F, approximation = F)
auto_arima_model 

### Question 37: ####

#code

#ARIMA(0,1,2)(0,1,1)[4]
#autoregressive
#degree of differencing
#number of moving avg
#4 - number of periods in each seasonal
#second set of numbers of seasonal time series
### Question 38: ####

#How many seasonal autoregressive lag variables have been used in the auto_arima model?

### Question 39: ####
# use the auto_arima model to construct a forecast of beer consumption over the quarters in the test dataset.
#What is the point forecasrt of the beer consumption for Q1 2008?
auto_arima_model_forecast = forecast(auto_arima_model,h=length(test))
auto_arima_model_forecast
length(test)

f= forecast(auto_arima_model, h = 22)
f
### Question 40: ####

# r code

### Question 41: ####

#What is the RMSE of the auto_arima model in the test datase?
accuracy(auto_arima_model_forecast,x = ausbeer)

### Question 42: ####

#r code



##### Section 4: Neural Networks ####:

### Question 43: ####

#discuss the purpose of the loss function in an artificial neural network model?
#The loss function is used to evaluate how well algorithms model the data. 
The loss function is used to calculate the prediction error. 


Specifically, The loss function is used to optimze the parameter values in a neural network model.  
Specifically, loss functions calculates stochastic gradient decent
### Question 44: ####
# i class you have explored a popular deeping learning Package called h20. review the following code 
# used to build a neural network model
library(h2o)
model1 = h2o.deeplearning(x  = 1:999,
                          y = 1000,
                          training_frame = train_h2o,
                          hidden = c(25,25,25),
                          seed = 10)

#how many hidden layers have been used in model1?
# 3 hidden layers 

### Question 45: ####

#based on the code to genrate model 1 described in the previous question, what is the total
#number of hidden units used in model1

#hidden units = 25

### Question 46: ####

#the h2o package allows for the use of random search to automatically tune hyperparameters.
#the following code has been applied to provide search criteria to tune hyperparameters

search_critera = list(strategy = "RandomDiscrete",
                      max_runtime_secs = 500,
                      max_models = 70,
                      seed = 10,
                      stopping_rounds = 7,
                      stopping_tolerance = 1e-2,
                      stopping_metric= 'logloss')

#based on search_criteria, the search will stop when logloss does not improve by a certain %.
#what is this percentage?
search_critera
#1

### Question 47: ####

#based on search_critera, the search will stop when logloss does not improve by more than 
#a certain number of scoring events.  What is the number of scoring events?


#7




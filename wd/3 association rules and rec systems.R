### March 19 2021
### Frameworks II Assignment 3: Association Rules and Recommender Systems


#################################################################

## Section 1 ##

#################################################################

library(arules)
library(ggplot2)
library(tidyverse)
library(arulesViz)
data(Groceries)
?Groceries


# Question 1: Load the dataset called Groceries that accompanies the arules package by executing data(Groceries). How many transactions does the transactions dataset Groceries contain?

# When you click the data in the environment, you can see S4 [9835 x 169]
# 9835 transactions


# Question 2: Which are the top 5 frequently occuring items in the dataset?

summary(Groceries)

par(mfrow=c(1,2))

itemFrequencyPlot(Groceries,
                  type="relative",
                  topN=5, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')

#yogurt
#soda
#rolls/buns
#other vegs
#whole milks
itemFrequencyPlot(Groceries, support = 0.04, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue", las = 1,topN=5,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))


#Question 3: Run a market basket analysis to generate a set of rules:
# support = 0.01, confidence= 0.01,  how many rules were generated?

rules_all = apriori(Groceries, parameter = list(support = 0.01, confidence = 0.01))
summary(rules_all)

#610 rules 


#Question 4:  Repeat with support and confidence of 0.001

rulesrules_all2 = apriori(Groceries, parameter = list(support = 0.001, confidence = 0.001))
summary(rulesrules_all2)
#41100 rules 

#Question 5: looking at first market basket, among the rules generated, what had the larget lift?

inspect(sort(rules_all, by = 'lift', decreasing = T)[1:5])

#3.372304


#Question 6: #Generate list of rules with only 2 items, support/confidence 0.01, how many 2 items rules were created

rules_2 = apriori(Groceries, parameter = list(support = 0.01, confidence = 0.01,
                                              minlen = 2,
                                              maxlen = 2))
summary(rules_2)

#426 rules

#Question 7: Which of the following rules havr the highest confidence 
inspect(sort(rules_2, by = 'confidence', decreasing = T)[1:10])

#butter whole milk 


#Question 8:  what is support for the rule, soda = whole milk
q = inspect(sort(rules_2, by = 'confidence', decreasing = T)[1:426])

#0.04006101

#Question 9:
#association is weak, honeslty guessed- just looked at the numbers and thought they were low


#Question 10:
#yogurt- milk
#basically i pulled up 'q' and just treated it like an excel file

#################################################################

## Section 2 ##

#################################################################



#Question 1 What format is the data in?
data = read.csv("product_ratings_data.csv")

#long

#Question 2: create realRatingMatrix, how many ratings does it contain
library(recommenderlab)

ratings_matrix = as(data, Class = 'realRatingMatrix')
#as(ratings_matrix, 'matrix')

ratings_matrix

#362105

#Question 3: What ratings did u10023 give prod_14

as(ratings_matrix, 'matrix')['u10023', c('prod_14')]
#4

#Question 4: Split data , sample(), seed 1031, train 90%, how many rows in train
set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]

nrow(train)

#4500

#Question 5: How many products did user 20150 rate? in train

train['u20150']
#44
nratings(train['u20150',])

#Question 6: how many ratings did product 25 recieve

nratings(train[,'prod_25'])
#3745

#Question 7: What is the most common rating in the train sample?


q = as(train, 'matrix')

sort(table(q))
#3

#Question 8: what is the avg rating for product 100 in train
mean(getRatings(train[,'prod_100']))
#2.824492

#Question 9:  Normalize user ratings, what is average rating for prod 100

pp = train
ww = normalize(train, method = 'center', row = TRUE )
mean(getRatings(ww[,'prod_100']))
#0.0843864

#Question 10:  Using the normalized ratings, assess the cosine similarty between
#the first five users, which of the pairs are most similar?

similarity(normalize(train)[1:6,], method = 'cosine')
#u895, u8926


#################################################################

## Section 3 ##

#################################################################


#Question 1: Construct a user-based collaborative filtering recommender using the train data. Use defaults for the parameters in the Recommender function. Based on this recommender, which of the following are in the list of top 5 recommended products for u10139? (Note: u10139 is in the test data).


recom_ubcf = Recommender(train, 
                         method='UBCF', 
                         parameter=list(method='cosine',nn=25, normalize='center'))
pred_ubcf_topN = predict(recom_ubcf,newdata=test,method='topNList',n=5)

getList(pred_ubcf_topN)['u10139']

#prod_72, prod_83


#Question 2: Based on the recommender created above, what is the predicted rating of product 1 (prod_1) by user 10139 (u10139)?
pred = predict(recom_ubcf, test, type = 'ratingMatrix')
getRatings(pred)

as(pred, 'matrix')

#3.0851606

#Question 3:Construct an item-based collaborative filtering recommender using train data. Use defaults for the parameters in the Recommender function. Based on this recommender, which of the following are in the list of top 5 recommended products for u10139?
recommenderRegistry$get_entry("IBCF", type ="realRatingMatrix")

recom_ibcf = Recommender(train, method='IBCF', 
                         parameter=list(k=30, method='cosine',
                                        normalize='center'))

pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_topN)['u10139']

#prod_3, prod_51


#Question 4: Based on the recommender created in the previous question, what is the predicted rating of product 1 (prod_1) by user 10139 (u10139)?

pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')
as(pred_ibcf, 'matrix')

#3.3115044


#Question 5: The recommenderlab library offers a useful framework to cross-validate and evaluate recommenders. Here, we are going to create an evaluation scheme using ratings_matrix, the realRatingMatrix that we had before splitting into a train and test dataset. The evaluationScheme() function below will do a 80:20 split on the data, placing 80% of the data in the train sample. And, we will give the recommender algorithm 30 items from the test set and hold out the other items for computing the error. As with any random sampling operation, it is important to set the seed right before creating the evaluationScheme as done below.

set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))

#1.307363

#Question 6:  Do the same for user based collaborative filtering 

set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))

#1.2279925

#Question 7:  user based with nn 100
recommenderRegistry$get_entries()$UBCF_realRatingMatrix


recom_ubcf = Recommender(train, 
                         method='UBCF', 
                         parameter=list(method='cosine',nn=100, normalize='center'))
pred_ubcf = predict(recom_ubcf,newdata=getData(es,'known'), type='ratings')




set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
getData(es, 'train')
getData(es, 'known')
getData(es, 'unknown')


nratings(ratings_matrix) == 
  nratings(getData(es,'train')) + 
  nratings(getData(es,'known')) + 
  nratings(getData(es,'unknown')) 


recom_ubcf = Recommender(data = getData(es,'train'),
                         method='UBCF',
                         parameter = list(method='cosine',nn=100,normalize='center'))
pred_ubcf = predict(recom_ubcf,newdata=getData(es,'known'), type='ratings')
calcPredictionAccuracy(pred_ubcf,data = getData(es,'unknown'))

#1.1589691

#Question 8:Finally, as a baseline for evaluating personalized recommenders, let us use a non-personalized recommender that relies only on popularity not on similarity. To do so, modify the code for the item-based recommender, replacing 'IBCF' by 'POPULAR'. Note, there is no need to recreate the evaluation scheme.

#What is the RMSE for this non-personalized recommender?




set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom = Recommender(getData(es,'train'),method='POPULAR')
pred_pop = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_pop = calcPredictionAccuracy(x = pred_pop,data = getData(es,'unknown'))
#1.1692988





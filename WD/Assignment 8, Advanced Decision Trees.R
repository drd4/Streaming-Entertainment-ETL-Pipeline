

## Assignment 8, Section 1: Advanced Decision Trees
library(ISLR)
data(OJ)

head(OJ)



#Question 1:  Split dataset, 70% train, how many rws are in the train sample

library(caTools)
set.seed(1234)

split = sample.split(OJ$Purchase, SplitRatio = 0.7)
train = OJ[split,]
test = OJ[!split,]

nrow(train)
nrow(test)

#749


#Question 2: In the train sample, how many Minute maid purchases were made?

head(train)
library(tidyverse)

MM <- train %>% filter(train$Purchase == "MM")
nrow(MM)

#292

#Question 3: What is the average price for MM in the train sample? 
head(train)
mean(train$PriceMM)

#2.087223

#Question 4: What is the avg discount for MM in the train sample?
mean(train$DiscMM)
#0.1237116


#Question 5:   How many purchases of MM were made in week 275?

## if in the train set

w275t <- train %>% filter(train$WeekofPurchase == "275")
MM275t <- w275t %>% filter(w275t$Purchase == 'MM')
nrow(MM275t)
#17



###### Section 2: 

#Question 1: Classification tree to predict Purchase using the following variables, price, discount, special, percent discount, loyalty for citrus hill, and diff in sales price
#What is AUC for the test sample?

library(rpart)
head(train)
classtree = rpart(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH, data = train, method = 'class')
summary(classtree)

library(rpart.plot)
rpart.plot(classtree)
library(ROCR)
pred = predict(classtree,newdata=test, type = "prob")
ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,0.4),
     text.adj=c(-0.3,2),
     xlab="1 - Specificity",
     ylab="Sensitivity") 

as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#0.8628776 --- rounds to 0.8629

#Question 2:  Tune the model to optimize complexity, use 10 fold cross validation and test cp values rannge from 0 to 0.1, in steps of 0.001, what is the optimal cp
#set seed to 100 before running the train function
install.packages('e1071', dependencies=TRUE)
library(e1071)
library(caret)
trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(.cp = seq(from = 0, to = 0.1, by = 0.001))
set.seed(100)
cvModel = train(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH,
                data = train,
                method = "rpart",
                trControl=trControl,
                tuneGrid = tuneGrid)
summary(cvModel)
cvModel$results
cvModel$bestTune$cp  ## 0.005




#Question 3:  Rerunn the tree model with the optimal cp value- what is the AUC for this model on the test sample?
cvTree = rpart(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH,
               data = train, cp = cvModel$bestTune$cp)
pred = predict(cvTree, newdata= test)
ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,0.4),
     text.adj=c(-0.3,2),
     xlab="1 - Specificity",
     ylab="Sensitivity") 

as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#0.8629

# The predict function when applied to a classification tree (rather than a regression tree) produces a matrix of probabilities of 0 and probabilities of 1. When entering the predictions into the prediction function of the ROCR package, you need to use pred[,2] and not pred. As an e.g., 
#pred = predict(treeModel,newdata=test)
#ROCRpred = prediction(pred[,2],test$dependentVariable)

## Section 3

#Question 1: Use same variables, but construct bag model, use 1000 trees,  need to set mtry 
#set seed to 617 before running the bag model

library(randomForest)
set.seed(617)
bag = randomForest(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH, data = train,
                   mtry = 9, ntree=1000) #mtry is set to 9, which is 10 prdictor variables - 1
pred = predict(bag, newdata= test, type = "prob")
ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,0.4),
     text.adj=c(-0.3,2),
     xlab="1 - Specificity",
     ylab="Sensitivity") 

A <- as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

round(A, 2)
#0.87


#Question 2:  USe a random forest model, use 1000 trees-  do not set mtry, set seed to 617
library(randomForest)
set.seed(617)
forest = randomForest(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH, data=train, ntree= 1000)
pred = predict(forest, newdata = test, type = "prob")
pred
ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#0.88


#Question 3:  to run a boosting model for a two-level classification model, dependent variable can only take values of 0 and 1, so need to create a new variable, Purchase2
#use new variable purchase 2, run a gradient boosting model (gbm) with 1000 trees, set distribution to bernoulli, interaction depth to 1, shrinkage to 0.004
train$Purchase2 = as.numeric(train$Purchase)-1

test$Purchase2 = as.numeric(test$Purchase)-1
library(gbm)
set.seed(617)
boost = gbm(Purchase2 ~ PriceCH + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + PriceDiff + PctDiscMM + PctDiscCH,
            data = train,
            distribution = "bernoulli",
            n.trees = 1000,
            interaction.depth = 1,
            shrinkage = 0.04)
summary(boost)

pred = predict(boost, n.trees = 100, type = "response", newdata = test)
ROCRpred = prediction(pred,test$Purchase2)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#0.88

#Assignment 5, Section 1: Logistic Regression

data <- read.csv("ebay.csv", stringsAsFactors = T)
head(data)


#Question 1:  How many rows are in the dataset
nrow(data)
#1861

#Question 2:  How many iPads are black?

head(data)
library(tidyverse)
unique(data$productline)
subset(data, data$color == "Black" & data$productline == c("iPad 2", "iPad 4", "iPad mini 2", "iPad 3", "iPad 1", "iPad mini", "iPad Air 1/2", "iPad mini Retina")) %>%
  count(color)

subset(data, data$color=="Black") %>%
count(color)


#a <- subset(data, data$color == "Black" | data$productline == "iPad 2" | data$productline == "iPad 4")

#425 black ipads



#Question 3:  Which of the following ipads dos this dataset contain
levels(data$productline)
#ipad1, ipad2, ipad3, ipad4, ipad air 1/2 , ipad mini, ipad mini2, ipad mini retina, ipad mini3, unkwown
#ALL BUT IPAD 5, IPAD 4 Mini

#Question 4 What is the uniqueID of the ipad with the highest startprice?
data[data$startprice == max(data$startprice), ]
#Unique ID = 11397

##################### 
# Assignment 5, Section 2: Logisitic Regression
#Question 1: Split data in train and test, 80% in train.  Use sample.split.  How many rows in train sample


data <- read.csv("ebay.csv", stringsAsFactors = T)

library(caTools)
head(data)
set.seed(196)
split = sample.split(Y = data$sold, SplitRatio = 0.8)
table(split)
train = data[split,]
test = data[!split,]
nrow(train)
#1489

#questoin 2: what is the median startprice of ipads that sold?
head(train)
sold <- subset(train, sold == '1')
median(sold$startprice)

#99

#question 3: what is the median start price of ipads that did not sell?
nosell <- subset(train, sold == '0')
median(nosell$startprice)

#249.99


#Question 4: Now, let us run a model to predict the variables that influence whether an iPad will be sold or not.  
#Since the variable to be predicted only takes on two values, we will use a logistic regression model. Use the 'glm' function to build a model,
#setting family to binomial with sold as the dependent variable and the following independent variables:

model1 <- glm(sold ~ biddable + startprice +  condition+ cellular+ carrier+ color+ storage+ productline+ noDescription+ charCountDescription+ upperCaseDescription+ startprice_99end, data = train, family = 'binomial')
summary(model1)
# 1454.5 
#but when you just do 
model1
#AIC is 1454


#Question 5 Now let us examine individual variables.  Which of the following variables has a singificant influence on whether an ipad is sold or not
summary(model1)
#Think we are just looking at the variables that have the *, so it is stat significant
#biddable, startprice, cellularunkwon, productline


#Question 6 Based on the results of the model, does 99 ending for startprice singifncatly increase the change of an iPad being sold?
summary(model1)
#statitically, no

# Question 7 Based on the model, does color of the upad have a statitically signifncat impact on wheter an ipad is sold? 
#no



##########
#Assignment 5, Section 3
data <- read.csv("ebay.csv", stringsAsFactors = T)

library(caTools)
head(data)
set.seed(196)
split = sample.split(Y = data$sold, SplitRatio = 0.8)
table(split)
train = data[split,]
test = data[!split,]
nrow(train)

#question 1: Simpler models are generally preferred to more complex models because they are less likely to overfit the data. So, let us drop out non-signficant variables from model1 above but keep variables that previous research or experience indicates should have an effect. So, estimate model2 with the following variables:

#biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end
#What is the AIC for model2?

model2 <- glm(sold ~ biddable+ startprice + condition + storage + productline + upperCaseDescription + startprice_99end, data = train, family = 'binomial')
summary(model2)
#1448.5

#Question 2:  based on the coeffiecnet of upperCaseDescription, what advice would you give someone selling ipads on ebay?
#coefficent is negative, so i would suggest use fewer upper case letters
summary(model2)
levels(train$productline)

#Question 3  You will note that the data contains a number of factor variables. In order to model factor variables, they have to be dummy coded. 
#Fortunately, glm and lm functions automatically dummy code factor variables and then run the dummy variables in the model. The first level is usually selected to be the baseline or reference variable to which each of the other levels is compared.
#Review the results of model2 and the coefficients of the variables. After controlling for the effects of all other variables in the model, what sells better iPad3 or iPad 1?
summary(model2)
100*(exp(summary(model2)$coef[10])-1) #ipad 3
100*(exp(summary(model2)$coef[17])-1) #unknown (ipad 1)
#since ipad 3 is greater, it sells better

#Question 4 If startprice goes up by $1, what will be the % reduction in the chance of selling an iPad. To interpret coefficients in logistic regression, you have to exponentiate the coefficient. E.g., exp(coefficient)

100*(exp(summary(model2)$coef[3])-1) #looking at third coefficent (start price)

# -.8965784
#closest to 1% 


#Question 5: Based on model2 (and controlling for the effects of all other variables), how much more (or less) likely is an iPad Air 1/2 to sell compared to an iPad 1?
100*(exp(summary(model2)$coef[12])-1) #ipad air 1/2
100*(exp(summary(model2)$coef[17])-1) #unknown (ipad 1)

#Question 6 Now, let us run one more model called model_productline. For this model, predict the variable 'sold' using only 'productline'. Is the sign of the coefficient for iPad Air1/2 in this model the same as that in model2? 

model_productline <- glm(sold ~ productline, data = train, family = 'binomial')
summary(model_productline)
#in this model, ipad air 1/2 sign is negative
summary(model2)
#in model 2, ipad air 1/2 sign is positive 


#Assignment 5: Section 4

#Question 1: Use model2 to generate predictions for 'sold' in the test set. What is the probability of sale for an iPad with UniqueID 10940?

pred_test <- predict(model2, newdata = test, type = 'response')
df <- data.frame(ID = test$UniqueID, prediction = pred_test)
df %>%
  filter(ID == 10940)
##0.02824507
#Question 2 #what is the accuracy of model 2 on the test set?  use threshold of 0.5
pred = predict(model2, newdata = test, type = 'response')
ct = table(sold = test$sold,
           predictions = as.integer(pred>0.5)); ct
accuracy = sum(ct[1,1], ct[2,2])/nrow(test); accuracy
#0.8064516

#Question 3:In order to evaluate a model, one has to have a point of reference or a baseline. A suitable baseline the accuracy of the test sample using the majority class in the train sample. In other words, what is the accuracy in the test sample, if we assumed the majority class of the train sample for all observations?

baseline = table(test$sold)[1]/nrow(test); baseline
#0.5376
#compared to question 2 accuracy, model 2 is performing better

#Question 4: Is model2 performing better than the baseline?
#since baseline is above 0.5, then model2 is performing better

#Question 5 The accuracy measure depends on the cut-value (or threshold) used. Hence a more popular measure is area under the curve (or AUC). AUC is computed by finding the area under the curve of a plot of Senstivity vs. 1-Specificity. AUC is a model performance measure that is independent of any particular threshold. What is the AUC for model2 on the test sample?

library(ROCR)
ROCRpred = prediction(pred, test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.868968
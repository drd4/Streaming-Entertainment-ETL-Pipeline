# Assignment 6, Section 1: Feature Selection

data <- read.csv("houses.csv")

str(data)
library(caret)
set.seed(1031)
split = createDataPartition(y=data$price,p = 0.7,list = F,groups = 100)
train = data[split,]
test = data[-split,]

# Question 1: What is the average house price in the train sample

mean(train$price)

#540165.7


#Question 2 Now, examine bivariate correlations with price to identify variables that are weakly related to (or not relevant) for predicting price. Which of the following variables has the weakest relationship with price?

cor(train[,-16])
round(cor(train[,-16]), 2)*100

#Condition as its the lowest 

#Question 3 Next, examine a correlation matrix to look evaluate bivariate correlations. Which of the following pairs have the highest bivariate correlation?
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(train[,c(3:7, 10:13,16)]),type = 'lower',show.diag = F,colors = c('red','white','darkgreen'))

#Sqft_living and bathrooms, darkest 

#Question 4: Theory or domain knowledge can help identify sources of multicollinearity. The area of a house (sqft_living) is the sum of area above the basement (sqft_above) and the basement (sqft_basement). This is useful to know because multicollinearity can arise not only from associations between a pair of predictors but also between a linear combination of predictors. But, first let’s verify this by computing the correlation between sqft_living and the sum of sqft_above and sqft_basement. What is the correlation?

sum <- train$sqft_above + train$sqft_basement

cor(sum, train$sqft_living)

#1 

#Question 5 bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age
#Call this model1. Now, use vif() from library(car) to assess VIF for each variable. Which predictor has the highest VIF?
  
model1 <- lm(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data  = train)  
summary(model1)

library(car)
vif(model1)

#sqft_living highest VIF

############################
#Assignment 6, Section 2: Feature Selection

#Question 1:   Let’s examine some algorithm-driven ways of feature selection predictors for price. We are going to select from the following predictors:
#bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age

#Evaluate all possible subsets to identify the best-six predictor model. Which of the following variables are included in the best six-predictor model? Select all that apply.

library(leaps)
library(tidyverse)
train_sub <- train %>% select(price,bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age)
subsets = regsubsets(price~., data = train_sub, nvmax = 6)
summary(subsets)
names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures
library(ggplot2)
library(tidyr)
subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  ggplot(aes(x=model,y=value))+
  geom_line()+
  geom_point()+
  facet_grid(type~.,scales='free_y')

#this tells us which model is the best
sel <- which.min(summary(subsets)$cp)
sel

#this tells us which coefficents are in the best model
coef(subsets, sel)

#the following have stars in the 6th regression:  bbedrooms, sqft_living, waterfront, view, grade, age
mod <- lm(price~bedrooms + sqft_living + waterfront + view + grade + age, data = train_sub)
summary(mod)

length(coef(subsets, sel))
# this says 0.642 - lets try this answer

#Question 2: What is the r2 for the best model?

model2 <- lm(price~bedrooms + sqft_living + waterfront + view + grade + age, data = train_sub)
summary(model2)

#0.642

#Question 3:  forward stepwise regression using same predictors. which of following variables were included in the best model

start_mod = lm(price~1, data = train_sub)
empty_mod = lm(price~1, data = train_sub)
full_mod  = lm(price~., data  = train_sub)
forwardStepwise = step(empty_mod,
                       scope = list(upper = full_mod, lower = empty_mod),
                       direction = 'forward')
summary(forwardStepwise)
summary(forwardStepwise)$r.squared
sort(names(coef(forwardStepwise)))

# we look at the AIC of each model and see that the final model, that includes sqft_lving, grade, age, waterfornt, view, vedrooms, bathroms ,sqft_lot, floors + condiotn 

#Question 4:  Now run backward stepwise


backwardStepwise = step(full_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)

#Question 5:
#Use hybrid

empty_mod = lm(price~1,data=train_sub)
full_mod = lm(price~.,data=train_sub)
hybridStepwise = step(empty_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')


#Question 6 Now, use a Lasso model to select from the following predictors: Set the seed to 1031. Use cv.glmnet from library(glmnet) which finds the best lambda through 10-fold cross-validation. Which of the following variables were included in the best model? Select all that apply.

# X / design matrix
library(glmnet)

set.seed(1031)
x = model.matrix(price~.-1, data = train_sub) # the -1 eliminates the intercept
y = train$price

lassoModel <- glmnet(x,y, alpha = 1,
                     standardize = TRUE)


lmModel <- glmnet(x, y, alpha = 1, lambda = 0,
                  standardize = TRUE)  #lambda = 0 no penalities

par(mfrow=c(1,2))
plot(lassoModel,xvar='lambda',label=TRUE, main = "LASSO regression")

# use cross validation to select best lambda
set.seed(1031)
cv.lasso = cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)

#compare coefficients
coef(cv.lasso)

# from here - we can see that only bathrooms, sqft_living, waterfront, view, grade and age are included in the model 


#Question 7 
#Now, use only the variables selected by Lasso model above to predict price using a linear regression. What is the R2 for the model?
q7mod<- lm(price~bathrooms + sqft_living + waterfront + view + grade + age, data = train_sub)
summary(q)

#0.6418

#Question 8: use dimension reduction to reduce the number of variables into a set of components
#instead of selecting individual variables, we capture the essence in a few components so as to retain at least 90% of the inforamtion.
# run the follopwing code to reduce the predictors to a few components

library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

#explore the intermediate objects and results of the above code to find how many components were used to capture the information in the predictors

str(trainComponents)

head(trainComponents)
#7 components

#Question 9 

train_model = lm(price~., trainComponents)
summary(train_model)

#.5465

#Question 10
testcomponents = predict(x,newdata=testPredictors)
testcomponents$price = test$price
pred = predict(train_model,newdata=testcomponents)
sse = sum((pred-testcomponents$price)^2)
sst = sum((mean(trainComponents$price) - testcomponents$price)^2)
r2_test = 1 - sse/sst
r2_test

#.551636

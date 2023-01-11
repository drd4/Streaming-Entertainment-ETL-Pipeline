#Assignment 4, Section 3

#Question 7:  Based on model4, if a person decided to add another bathroom, what would the increase in expected price, holding all other predictors constant?

house = read.csv("house.csv")

library(caret)
set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]
nrow(train)
nrow(test)
nrow(house)

model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model4)

#The coefficent for bathrooms is 53366.25732, the assignment rounds up to 53370

#Question 8  Which of the predictors in model4 exerts the strongest influence on price?

#We are looking for the largest coefficent (absolute value) - based on summary(model4) the following are the coefficents
#Bedrooms -41551.50
#Bathroom 33886.06
#sqft_living 164.64
#age 3495.38
#grade 130266.64

#Grade is the largest but that is incorrect, bedrooms is the second largest 

#Question 9 Finally, let us apply model4 estimated on train data to test data.  What is the R2 for the test?

model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = test)
summary(model4)

#R2 = 0.6609 

#Question 10: What is the rsme for model 4 on the test sample?

pred = predict(model4)
data.frame(price = test$price[100:109], prediction = pred[100:109])

sse = sum((pred - test$price)^2)
sst = sum((mean(test$price)-test$price)^2)
model4_r2 = 1 - sse/sst; model4_r2

sse1 = sum((pred-test$price)^2); sse1
rmse4 = sqrt(mean((pred-test$price)^2)); rmse4

#212514.9 
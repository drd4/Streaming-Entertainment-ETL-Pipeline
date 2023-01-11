# Assignment 7, Section 1: Decision Trees

wages = read.csv("assignment7_wages.csv",stringsAsFactors = T)
str(wages)

#Question 1: Which of the values are non-metric?

str(wages)  #sex, race



# Question 2: What fraction of the respondents are female?
wages = wages[wages$earn>0,]
library(tidyverse)
a <- wages %>% filter(sex == "female")
859/1368
#0.627924


#question 3  Which race on average earns the least?
black <- wages %>% filter(race == "black")
mean(black$earn) #28830
hispanic <- wages %>% filter(race == "hispanic")
mean(hispanic$earn) #25821
other <- wages %>% filter(race == "other")
mean(other$earn) #33431
white <- wages %>% filter(race == "white")
mean(white$earn) # 33570

#hipanic earns least

#Question 4: Split the data into a train and test sample such that 75% of the data is in the train sample. Use simple random sampling by employing the sample() function. Use a seed of 1731. Call the train data, train and test data, test.

#What is the average earn of respondents in the train sample?


## 75% of the sample size
smp_size <- floor(0.75 * nrow(wages))

## set the seed to make your partition reproducible
set.seed(1731)
smp_size <- floor(0.75 * nrow(wages))

train_ind <- sample(seq_len(nrow(wages)), size = smp_size)

train <- wages[train_ind, ]
test <- wages[-train_ind, ]

mean(train$earn)  #31760.4


# Question 5 What is the average height of respondents in the train sample?

mean(train$height)  #66.47727



###### 
# Assignment 7, Section 2: Decision Trees

# Question 1: Now, construct a linear regression model to predict earn using all other variables. Call this model1.  Which variables are stat sig?

model1 <- lm(earn~., data = train)
summary(model1)
#height, sex, ed, age


#Question 2: We will be comparing linear regression to a regression tree model later. Since tree models don’t generate an R2, let us compute the root mean squared error (RMSE).

#What is the RMSE for model1 on the train sample?

pred = predict(model1)

sse = sum((pred - train$earn)^2)
sst = sum((mean(train$earn)-train$earn)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$earn)^2); sse1
rmse1 = sqrt(mean((pred-train$earn)^2)); rmse1

#26957.41


#Question 3: Review the charts. What is the approximate difference in earn between 12 years and 16 years of education for Males?


library(ggplot2)
ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ 
  geom_bar(stat="summary",fun="mean",position="dodge")

ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))

#20000

#Question 4: Based on the charts drawn for the previous question, what is the approximate difference in earn between 12 years and 16 years of education for Females?

#15000

#Question 5:  Which of the following variables are significant? Select all that apply

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train) 
summary(model_sex_ed)
## ed, sexmale:ed

#Question 6:  Based on the model, "sex and ed do not interact in influencing earn"
#FALSE


#Question 7:  Contrust model that incorporated all variables and interaction bc sex and ed. what is the RSME?
names(train)
model2 <- lm(earn~ height + sex + race + ed + age + sex*ed, data = train)
summary(model2)
pred = predict(model2)
rmse2 = sqrt(mean((pred-train$earn)^2)); rmse2

#26908.29

#Question 8:  is the RSME from model 2 lower than model1?
#TRUE


#Question 9 : add sex and age to to model2

model3 <- lm(earn ~ height+ sex + race + ed + age + sex*ed + sex*age, data= train)
summary(model3)
pred = predict(model3)
rmse3 = sqrt(mean((pred-train$earn)^2)); rmse3

#26860.93

#Question 10: add age*ed to model 3
model4 <- lm(earn ~ height + sex + race + ed + age + sex*ed + sex*age + age*ed, data = train)
summary(model4)
pred = predict(model4)
rmse4 = sqrt(mean((pred-train$earn)^2)); rmse4
#26847.67


#Question 11: RSME for model
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
summary(model5)
pred = predict(model5)
rmse5 = sqrt(mean((pred-train$earn)^2)); rmse5
#26519.18

#Question 12: what variables are stat sig in model 1 and model 5
# none of the above



##########
#Assignment 7, Section 3

#Question 1:  construct regression tree, which is the first variable to be used for the split?
library(rpart)
library(rpart.plot)
tree1 <- rpart(earn~., data = train)
prp(tree1,digits=5)   # tree plot method 1
rpart.plot(tree1, digits=5) # tree plot method 2

# sex is the first split

#Question 2 what is true of people that earn the most?
#over 18 years of education


#Question 3: Which is true about people that earn the leasT?
#age less than 29

#Question 4how many levels does the tree have?
# 11

# Question 5 Compute RSME for tree1
pred = predict(tree1)
rmse = sqrt(mean((pred-train$earn)^2)); rmse
#25143.97

#Question 6: how many leaves
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
summary(treeSimp1)
prp(treeSimp1,digits=5)

#9

#Question 7: RSME
pred = predict(treeSimp1)
rmse2 = sqrt(mean((pred-train$earn)^2)); rmse2
#25683.12

#Question 8: tree with minibucket of 50
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
prp(treeSimp2)

#6 leaves

#Question 9 : RSME
pred = predict(treeSimp2)
rmse3 = sqrt(mean((pred-train$earn)^2)); rmse3
#26728.12


#Question 10 : minibucket of 5, rsme?
treeComplex1 <- rpart(earn~., data = train, control = rpart.control(minbucket = 5))
pred = predict(treeComplex1)
rmse4 = sqrt(mean((pred-train$earn)^2)); rmse4
#25143.97

#Question 11: treeComplex 2, mini 1, rsme?
treeComplex2 <- rpart(earn~., data = train, control = rpart.control(minbucket = 1))
pred = predict(treeComplex2)
rmse5 = sqrt(mean((pred-train$earn)^2)); rmse5
#22725.7




#####
#Assignment 7, Section 4

#Question 1:  What is the test RSME for the linear regression model with the lowest train RSME?
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
summary(model5)
pred = predict(model5, newdata = test)
rmse5 = sqrt(mean((pred-test$earn)^2)); rmse5
#27154.5

#question 2:  what is rsme for tree1 with test data
tree1 <- rpart(earn~., data = train)
pred = predict(tree1, newdata = test)
rmse6 = sqrt(mean((pred-test$earn)^2)); rmse6
#27059.98

#Question 3: RSME for treesimp2 with test
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
pred = predict(treeSimp2, newdata = test)
rmse7 = sqrt(mean((pred-test$earn)^2)); rmse7
#27990.53


#Question 4:  What is the test set RMSE for treecomplex2
treeComplex2 <- rpart(earn~., data = train, control = rpart.control(minbucket = 1))
pred = predict(treeComplex2, newdata = test)
rmse6 = sqrt(mean((pred-test$earn)^2)); rmse6
#30141.22

# Question 5 Which model had lowest RSME?
#tree1

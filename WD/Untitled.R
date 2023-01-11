## KAGGLE EXAMPLE Random forest
scoringData = read.csv('scoringData.csv')
data =read.csv("analysisData.csv")

scoringData$zipcode <- as.character(scoringData$zipcode)

library(dplyr)

## combining data to jointly deal with factor labels and missing data
combinedData <- bind_rows(data, scoringData)

combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
## set missing / malformed zipcodes to NA
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA
combinedData$zipcode <- as.factor(combinedData$zipcode)
combinedData$zipcode <- forcats::fct_lump_n(combinedData$zipcode, 40) # combine factor levels for zipcodes less than 40

table(combinedData$zipcode)
#select numeric variables
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')

combinedData[,numeric_predictors] <- predict(imp_model_med, newdata=combinedData[,numeric_predictors])

names(combinedData)
## split back into training data
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]

library(ranger)
library(caret)

formula <- "price ~ zipcode + minimum_nights + review_scores_accuracy + calculated_host_listings_count_shared_rooms + review_scores_location + minimum_minimum_nights"

trControl <- trainControl(method = "cv", number = 5)
tuneGrid = expand.grid(mtry = c(2,4,6),
                       splitrule = "extratrees",
                       min.node.size = c(10,50,100))
# training 
install.packages("doParallel")
library(doParallel)
##this takes a minute 
cl <- makePSOCKcluster(2)
registerDoParallel(cl)


set.seed(617)

## if there are missing values in zipcode or whatever
table(combinedData$zipcode)
sort(table(combinedData$zipcode))

combinedData$zipcode[is.na(combinedData$zipcode)] <- "Other"


### try more values for the random forest 
## do more variable exploration  # decision tree 


## KAGGLE EXAMPLE Random forest
scoringData = read.csv('scoringData.csv')
data =read.csv("analysisData.csv")

scoringData$zipcode <- as.character(scoringData$zipcode)

library(dplyr)

## combining data to jointly deal with factor labels and missing data
total <- bind_rows(data, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
## set missing / malformed zipcodes to NA
total$zipcode[nchar(total$zipcode)<5] <- NA
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40
table(train$zipcode)
table(test$zipcode)
table(total$zipcode)
library(stringr)
library(qdapTools)

total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))

## Check if it works
head(total$amenities, 3)

names(total)

train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]

mod <- lm(price ~ bedrooms + bathrooms + monthly_price + Pool + zipcode +  property_type + cleaning_fee + accommodates + review_scores_rating, data = train)
summary(mod)
library(caret)
library(car)
vif(mod)
pred = predict(mod,newdata=test)
str(test$zipcode)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)


##################

#######################
####################################################
library(tidyverse)
library(randomForest)
library(stringr)
library(qdapTools)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1

total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign
total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)



total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
#total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
#total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))
### Fix property types ###
# remove properties with less than 5 examples #
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]

str(test)

test$property_type[test$property_type == "Castle"] <- "Other"
test$property_type[test$property_type == "Dome house"] <- "Other"
test$property_type[test$property_type == "Tent"] <- "Other"
test$zipcode[test$zipcode == "11103"] <- "Other"
test$zipcode[is.na(test$zipcode)] <- "Other"

# 
# k <- lm(price ~ zipcode + bedrooms +  Pool + bathrooms + property_type + cleaning_fee + Shampoo + review_scores_rating + accommodates + TV + Gym  + Dryer +  Elevator + minimum_nights + Airconditioning  + Essentials + Petsallowed + `Selfcheck-in` + `Family/kidfriendly` + Breakfast, train)
# summary(k)
# 
# pred = predict(k, newdata = test)
# 
# submissionFile = data.frame(id = test$id, price = pred)
# write.csv(submissionFile, 'sample_submission.csv',row.names = F)
# table(test$property_type)
# table(train$property_type)
# 
# table(train$zipcode)
# 
# 
# oo <- lm(price ~ zipcode + bedrooms + bathrooms + Pool + cleaning_fee + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials, train)
# summary(oo)
# pred = predict(oo,  test)
# sub = data.frame(id = test$id, price = pred)
# sub
# 
# ## to fix the NA of the test, you need to fill in missing values
# ok <- lm(price ~ zipcode + bedrooms + bathrooms + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials + square_feet + Petsallowed + `Selfcheck-in` + `Family/kidfriendly` + Breakfast, train)
# pred1 = predict(ok, test)
# summary(ok)
# s = data.frame(id = test$id, price = pred1)
# write.csv(s, 'sample_submission.csv',row.names = F)
# 
# # random forest
# 
# library(randomForest)
# set.seed(617)
# forest = randomForest(price ~ zipcode + bedrooms + bathrooms + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials + square_feet + Petsallowed +   Breakfast, data = train, ntree = 100, na.action = na.exclude)
# summary(forest)
# predfor = predict(forest, newdata = test)
# plot(forest)
# q = data.frame(id= test$id, price = predfor)
# q
# write.csv(q, 'q.csv', row.names = F)  #61

forestmoretrees = randomForest(price ~ zipcode + bedrooms + bathrooms + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials + square_feet + Petsallowed +   Breakfast, data = train, ntree = 250, na.action = na.exclude)
plot(forestmoretrees, ymax = 80)
?plot()

plot(forestmoretrees)

#400 trees
forestmoretrees2 = randomForest(price ~ zipcode + bedrooms + bathrooms + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials + square_feet + Petsallowed +   Breakfast, data = train, ntree = 400, na.action = na.exclude)

plot(forestmoretrees2)
predforr = predict(forestmoretrees2, newdata =  test)
qq = data.frame(id = test$id, price = predforr)
write.csv(qq, 'q.csv', row.names = F)   


#################
###############

#######################
####################################################
library(tidyverse)
library(randomForest)
library(stringr)
library(ngram)
library(qdapTools)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1

total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign


colnames(total)[which(colnames(total) == '24-hourcheck-in')] = 'check_in_24'
colnames(total)[which(colnames(total) == 'Cat(s)')] = 'cat'
colnames(total)[which(colnames(total) == 'Dog(s)')] = 'dog'
colnames(total)[which(colnames(total) == 'Otherpet(s)')] = 'otherpet'
colnames(total)[which(colnames(total) == 'Washer/Dryer')] = 'Washer_and_Dryer'
colnames(total)[which(colnames(total) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(total)[which(colnames(total) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'





total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)





# First, remove other signs in the column like [, ], . 
total$host_verifications = gsub("\\[", "", total$host_verifications) ## remove [
total$host_verifications = gsub("\\]", "", total$host_verifications) ## remove ]
total$host_verifications = gsub("\\'", "", total$host_verifications) ## remove '
total$host_verifications = total$host_verifications %>% stringr::str_replace_all("\\s", "") ## remove the space
total$host_verifications = noquote(total$host_verifications) ##  remove quotation sign

head(total$host_verifications)
## Create dummy variables
total = cbind(total, mtabulate(strsplit(as.character(total$host_verifications), split = ','))) 

## calculate the wordcount for the text column chosen ###
total$host_identity_verified = as.character(total$host_identity_verified)
total = total %>%
  rowwise() %>%
  mutate(verification_count = wordcount(host_identity_verified))



total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))
### Fix property types ###
# remove properties with less than 5 examples #
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total

# filter variables with (near) zero variance
library(caret)
# zero_var_table <- nearZeroVar(total, saveMetrics= TRUE)
# total <- total[, !zero_var_table$nzv]
# cor(train$price, train$host_response_time)

str(train$price)
str(train$host_response_rate)
## split data
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]

str(test)

test$property_type[test$property_type == "Castle"] <- "Other"
test$property_type[test$property_type == "Dome house"] <- "Other"
test$property_type[test$property_type == "Tent"] <- "Other"
test$zipcode[test$zipcode == "11103"] <- "Other"
test$zipcode[is.na(test$zipcode)] <- "Other"

 sqft <- lm(price ~ zipcode + bedrooms   + square_feet + Pool + bathrooms + property_type + cleaning_fee + Shampoo + review_scores_rating + accommodates + TV + Gym  + Dryer +  Elevator + minimum_nights + Airconditioning  + Essentials  + `Selfcheck-in` + `Family/kidfriendly` + Breakfast, train)
 summary(sqft)
# pred = predict(sqft, newdata= test)
# qqq = data.frame(id = test$id, price = pred)
# qqq
# 
# rmset = sqrt(mean((pred-test$price)^2)); rmset
# 


## lets do some variable selection
# library(caret)
# zero_var_table <- nearZeroVar(total, saveMetrics= TRUE)
# total <- total[, !zero_var_table$nzv]
# 
# names(total)

## split train and test data based on weekly price


train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]

# # train split
# train1 <- train[is.na(train$weekly_price), ]
# train2 <- train[!is.na(train$weekly_price),]
# 
# #test
# test1 <- test[is.na(test$weekly_price),]
# test2 <- test[!is.na(test$weekly_price),]





forest = lm(price ~ zipcode + bedrooms + bathrooms + property_type + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + Pool + minimum_nights   + Essentials + square_feet +  host_listings_count + number_of_reviews + availability_30 + Breakfast + security_deposit, data = train)
summary(forest)
pred = predict(forest, newdata= test)
qqq = data.frame(id = test$id, price = pred)


########
library(tidyverse)
library(randomForest)
library(stringr)
library(ngram)
library(qdapTools)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA


total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40


## split data
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]


total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1

total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))
#missing variable fixes

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1
total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)
total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$review_scores_rating[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm = TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total

## split data
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]


table(total$minimum_nights) ---### should fix this and clean up outliers
table(total$Essentials)

mod <- lm(price ~ zipcode + bedrooms + bathrooms + cleaning_fee + square_feet + accommodates + property_type )


write.csv(total, "t.csv")


cor(train$price, train$Elevator)  #0.16 / 29310
sum(train$Elevator)/nrow(train)

a <- train %>% filter(train$Elevator == '1')

cor(train$price, train$CableTV) #.18

cor(train$price, train$Airconditioning)  # .16

cor(train$price, train$Buildingstaff) #.12

cor(train$price, train$Dishwasher) #.19

cor(train$price, train$Doorman) #.11

cor(train$price, train$Dryer)  #.20

cor(train$price, train$`Family/kidfriendly`)  #.19

cor(train$price, train$Gym) #.13

cor(train$price, train$Hairdryer) #.12

cor(train$price, train$Indoorfireplace) #.11

cor(train$price, train$Internet)

cor(train$price, train$Iron) #.11

cor(train$price, train$Pool)  #0.49

cor(train$price, train$TV)  #.22

cor(train$price, train$Washer)  #.20

cor(train$price, train$review_scores_location) #.14
cor(train$price, train$review_scores_rating)  #0.06


cor(train$price, train$minimum_minimum_nights) 


mod <- lm(price ~ zipcode + bedrooms +  + bathrooms + cleaning_fee + minimum_nights +square_feet + CableTV + Airconditioning + Dishwasher  + `Family/kidfriendly` + TV + Washer + review_scores_location + property_type, data = train )
summary(mod)
vif(mod)
pred = predict(mod, newdata= test)
qqq = data.frame(id = test$id, price = pred)
qqq



set.seed(617)
#forest with 500 trees
forest = randomForest(price~ zipcode + bedrooms +  + bathrooms + cleaning_fee + minimum_nights +square_feet + CableTV + Airconditioning + Dishwasher + TV + Washer + review_scores_location + property_type, data = train, ntree = 500, na.action = na.exclude)
pred = predict(forest, newdata= test)
qqq = data.frame(id = test$id, price = pred)
plot(forest) 
qqq$price[is.na(qqq$price)] <- median(qqq$price, na.rm=TRUE)
qqq$price[qqq$price <0 ] <- median(qqq$price, na.rm = TRUE)
write.csv(qqq, 'forest1113.csv', row.names = F)   #73



# boosting with cross validation 
forestmoretrees = randomForest(price ~ zipcode + bedrooms + bathrooms + cleaning_fee  + Shampoo + review_scores_rating + accommodates + TV + Gym + Dryer + Elevator + minimum_nights + Airconditioning + Essentials + square_feet + Petsallowed +   Breakfast, data = train, ntree = 250, na.action = na.exclude)


library(caret)
set.seed(617)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 100,
                       interaction.depth = c(1,2,3), 
                       shrinkage = (1:100)*0.001, # or shrinkage = seq(1e-3, 1, by = .1)
                       n.minobsinnode=c(5,10,15))

garbage = capture.output(cvModel <- train(price ~ zipcode + bedrooms + bathrooms + cleaning_fee + minimum_nights +square_feet + CableTV + Airconditioning + Dishwasher + TV + Washer + review_scores_location + property_type, data=train,
                                          method="gbm", trControl=trControl, tuneGrid=tuneGrid, na.action = na.exclude))

cvBoost = gbm(earn~., data=train,
              distribution="gaussian", n.trees=cvModel$bestTune$n.trees, interaction.depth=cvModel$bestTune$interaction.depth, shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)
pred = predict(cvBoost,test,n.trees=100)
rmse_cv_boost = sqrt(mean((pred-test$earn)^2)); rmse_cv_boost  


## cart model

library(rpart); library(rpaaert.plot)
tree = rpart(price ~  zipcode + bedrooms +  + bathrooms + cleaning_fee + minimum_nights +square_feet + CableTV + Airconditioning + Dishwasher + TV + Washer + review_scores_location + property_type, data=train )
pred = predict(tree, newdata= test)
 
aa = data.frame(id = test$id, price = pred)
write.csv(aa, 'cartmodel.csv', row.names = F)

## max tree
maximaltree = rpart(price ~ zipcode + bedrooms +  + bathrooms + cleaning_fee + minimum_nights +square_feet + CableTV + Airconditioning + Dishwasher + TV + Washer + review_scores_location + property_type, data=train, control = rpart.control(cp = 0) )
pred = predict(maximaltree, newdata= test)
aaa = data.frame(id= test$id, price = pred)
write.csv(aaa, 'garbages.csv', row.names = F) #74




############


########
library(tidyverse)
library(randomForest)
library(stringr)
library(car)
library(caret)
library(ngram)
library(qdapTools)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA


total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode[total$zipcode==""] <- "Other"

total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1



total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1
total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)
total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$review_scores_rating[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm = TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total
total$host_response_time <- as.factor(total$host_response_time)
total$host_is_superhost <- as.factor(total$host_is_superhost)
total$host_has_profile_pic <- as.factor(total$host_has_profile_pic)
total$host_identity_verified <- as.factor(total$host_identity_verified)

#total$host_response_time[is.na(total$host_response_time)] <- "Other"

#total$host_response_time[total$host_response_time == "within an hour"] <- "OK"

## split data
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]



m <- lm(price ~  host_total_listings_count  + zipcode  + property_type + room_type + accommodates + bathrooms + bedrooms + beds +  cleaning_fee + guests_included + extra_people + minimum_nights + availability_30 +availability_365 + number_of_reviews + review_scores_rating +calculated_host_listings_count+reviews_per_month, train)
summary(m)
vif(m)
m$xlevels$host_response_time <- union(m$xlevels$host_response_time, levels(test$host_response_time))
m$xlevels$host_is_superhost <- union(m$xlevels$host_is_superhost, levels(test$host_is_superhost))
m$xlevels$host_has_profile_pic <- union(m$xlevels$host_has_profile_pic, levels(test$host_has_profile_pic))
m$xlevels$host_identity_verified <- union(m$xlevels$host_identity_verified, levels(test$host_identity_verified))

pred = predict(m, newdata = test)

submissionFile = data.frame(id = test$id, price = pred)
write.csv(submissionFile, 'forth_submission.csv', row.names = F)

################### going to look at the best model and add a couple more variables to see what the deal is

########
library(tidyverse)
library(randomForest)
library(stringr)
library(car)
library(caret)
library(ngram)
library(qdapTools)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA


total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode[total$zipcode==""] <- "Other"

total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1
total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)
total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$review_scores_rating[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm = TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total



total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))

#total$host_response_time[is.na(total$host_response_time)] <- "Other"

#total$host_response_time[total$host_response_time == "within an hour"] <- "OK"

## split data
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]

#linear model first


mod <- lm(price ~ zipcode + bedrooms + bathrooms + cleaning_fee + review_scores_rating + accommodates + minimum_nights + square_feet + property_type + room_type + guests_included + extra_people + availability_30 + availability_365 + number_of_reviews + calculated_host_listings_count + reviews_per_month+ Shampoo + TV + Gym + Dryer + Elevator + Airconditioning + Essentials + Petsallowed, train)
summary(mod)
vif(mod)
pred = predict(mod, newdata = test)
sub = data.frame(id = test$id, price = pred)


##


library(gbm)
set.seed(617)
trControl_boost6 = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boost6 = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  
## after trying different values of parameters, I found these values are the best according to the cross-validation results

cvBoost6 = train(price ~ zipcode + bedrooms + bathrooms + cleaning_fee + review_scores_rating + accommodates + minimum_nights + square_feet + property_type + room_type + guests_included + extra_people + availability_30 + availability_365 + number_of_reviews
                 + calculated_host_listings_count + reviews_per_month+ Shampoo + TV + Gym + Dryer + Elevator + Airconditioning + Essentials + Petsallowed
                 ,data = train, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6, na.action = na.exclude)
cvBoost6

train$property_type <- as.factor(train$property_type)
train$room_type <- as.factor(train$room_type)
### start here*********
## to save time, I only use 2,000 trees here, the best model used 30,000 trees
boostCV6 = gbm(price ~  zipcode + bedrooms + bathrooms + cleaning_fee + review_scores_rating + accommodates + minimum_nights + square_feet + property_type + room_type + guests_included + extra_people + availability_30 + availability_365 + number_of_reviews
               + calculated_host_listings_count + reviews_per_month+ Shampoo + TV + Gym + Dryer + Elevator + Airconditioning + Essentials + Petsallowed
               ,data = train, distribution = "gaussian", 
               n.trees = 200,   
               interaction.depth = 5,
               shrinkage = 0.010,
               n.minobsinnode = 5)

summary(boostCV6)

## predict scoring dataset
pred_boostscore = predict(boostCV6, n.trees = 200, newdata = scoring)

submissionFile = data.frame(id = scoring$id, price = pred_boostscore)
write.csv(submissionFile, 'myboost.csv',row.names = F)  #74

#### my boost
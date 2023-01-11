#######
library(tidyverse)
library(randomForest)
library(stringr)
library(car)
library(caret)
library(ngram)
library(qdapTools)
data = read.csv('analysisData.csv')
scoring = read.csv('scoringData.csv')

data$amenities = as.character(data$amenities)

scoring$amenities = as.character(scoring$amenities)



library(ngram)
library(dplyr)



library(stringr)
library(qdapTools)

## First, remove the irrelevant sign in the column
data$amenities = gsub("\\.", "", data$amenities)  ## remove the dot sign 
data$amenities = data$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
data$amenities = noquote(data$amenities)   ##  remove quotation sign

## Second split the column and create dummy variables
data = cbind(data,mtabulate(strsplit(as.character(data$amenities), ',')))

## Check if it works
head(data$amenities, 3)



## Create dummy variables
data = cbind(data, mtabulate(strsplit(as.character(data$host_verifications), split = ','))) 


## First, the amenities column
scoring$amenities = gsub("\\.", "", scoring$amenities)  
scoring$amenities = scoring$amenities %>% stringr::str_replace_all("\\s", "")   
scoring$amenities = noquote(scoring$amenities)  
scoring = cbind(scoring,mtabulate(strsplit(as.character(scoring$amenities), ',')))



colnames(data)[which(colnames(data) == '24-hourcheck-in')] = 'check_in_24'
colnames(data)[which(colnames(data) == 'Washer/Dryer')] = 'Washer_and_Dryer'
colnames(data)[which(colnames(data) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(data)[which(colnames(data) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'

colnames(scoring)[which(colnames(scoring) == '24-hourcheck-in')] = 'check_in_24'
colnames(scoring)[which(colnames(scoring) == 'Washer/Dryer')] = 'Washer_and_Dryer'
colnames(scoring)[which(colnames(scoring) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(scoring)[which(colnames(scoring) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'


data$bedrooms[is.na(data$bedrooms)] <- 1
data$bathrooms[is.na(data$bathrooms)] <- 1
data$cleaning_fee[is.na(data$cleaning_fee)] <- median(data$cleaning_fee, na.rm=TRUE)
data$square_feet[is.na(data$square_feet)] <- median(data$square_feet, na.rm=TRUE)
data$review_scores_rating[is.na(data$review_scores_rating)] <- median(data$review_scores_rating, na.rm = TRUE)
data$accommodates[is.na(data$accommodates)] <- median(data$accommodates, na.rm=TRUE)
data$accommodates[is.na(data$review_scores_rating)] <- median(data$review_scores_rating, na.rm=TRUE)
data %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> data




scoring$bedrooms[is.na(scoring$bedrooms)] <- 1
scoring$bathrooms[is.na(scoring$bathrooms)] <- 1
scoring$cleaning_fee[is.na(scoring$cleaning_fee)] <- median(scoring$cleaning_fee, na.rm=TRUE)
scoring$square_feet[is.na(scoring$square_feet)] <- median(scoring$square_feet, na.rm=TRUE)
scoring$review_scores_rating[is.na(scoring$review_scores_rating)] <- median(scoring$review_scores_rating, na.rm = TRUE)
scoring$accommodates[is.na(scoring$accommodates)] <- median(scoring$accommodates, na.rm=TRUE)
scoring %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> scoring

### data
## create a data frame to use the neighbourhood_cleansed
data1 = data %>%
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(record_count_c = n(),     ## count every level's record
            price_mean_c = mean(price)) %>%  ## calculate each group's mean price
  arrange(desc(record_count_c))


## merge the data frame and get the mean price and record counts
data = merge(data, data1, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

class(data$neighbourhood_cleansed)

## select the top 50 levels
data$neighbourhood_cleansed = as.character(data$neighbourhood_cleansed) ## we need to convert it into character first in order to change the values
data = data %>%
  mutate(new_neigh_c = ifelse(record_count_c > 142, neighbourhood_cleansed, "other"))  ## find the top 49 levels and put others into "other" group
data$neighbourhood_cleansed = as.factor(data$neighbourhood_cleansed)
data$new_neigh_c = as.factor(data$new_neigh_c)

### scoring
## create a data frame to use the neighbourhood_cleansed
scoring1 = scoring %>%
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(record_count_c = n()) %>%
  arrange(desc(record_count_c))



## merge the data frame and get the mean price and record counts
scoring = merge(scoring, scoring1, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

scoring$neighbourhood_cleansed = as.character(scoring$neighbourhood_cleansed)
scoring = scoring %>%
  mutate(new_neigh_c = ifelse(record_count_c > 32, neighbourhood_cleansed, "other"))
scoring$new_neigh_c = as.factor(scoring$new_neigh_c)

## create price_mean_c for score_data 
data2 = data.frame(neighbourhood_cleansed = data1$neighbourhood_cleansed,
                   price_mean_c = data1$price_mean_c)

scoring = merge(scoring, data2, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"), all.x = TRUE) 

sum(is.na(scoring$price_mean_c))
scoring[is.na(scoring$price_mean_c),]$price_mean_c = mean(scoring$price_mean_c, na.rm = TRUE)


### data 
## create a data frame to use the mean price of neighbourhood_group_cleansed
data3 = data %>%
  group_by(neighbourhood_group_cleansed = neighbourhood_group_cleansed) %>%
  summarize(price_mean_gc = mean(price))

## merge the data frame and get the mean price
data = merge(data, data3, by = c("neighbourhood_group_cleansed", "neighbourhood_group_cleansed"))


### scoring 
## merge the data frame and get the mean price and record counts
scoring = merge(scoring, data3, by = c("neighbourhood_group_cleansed", "neighbourhood_group_cleansed"), all.x = TRUE)

## check missing value
sum(is.na(scoring$price_mean_gc)) ## there is no NA




library(caret)
set.seed(617)
split = createDataPartition(y = data$price,
                            p = 0.7,
                            list = F,
                            groups = 100)
train = data[split,]
test = data[-split,]
nrow(train) + nrow(test) == nrow(data)




library(gbm)
set.seed(617)
trControl_boost6 = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boost6 = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  
## after trying different values of parameters, I found these values are the best according to the cross-validation results

cvBoost66 = train(price ~ price_mean_c + bedrooms + cleaning_fee + room_type + minimum_nights 
                 + accommodates + property_type + bathrooms + beds + neighbourhood_cleansed + host_is_superhost
                 + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
                 + reviews_per_month + cancellation_policy
                 + accommodates + availability_90
                 + Airconditioning + Dryer + Elevator + Family_and_kidfriendly +
                 + review_scores_cleanliness + Hairdryer + Refrigerator + Shampoo + number_of_reviews 
                + Selfcheck_in  + TV + maximum_nights + host_listings_count + cleaning_fee 
                 + monthly_price + review_scores_accuracy + square_feet + property_type
                 ,data = train, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6, na.action = na.exclude)

cvBoost66

### start here*********
## to save time, I only use 2,000 trees here, the best model used 30,000 trees
train$room_type <- as.factor(train$room_type)
train$property_type <- as.factor(train$property_type)
train$host_is_superhost <- as.factor(train$host_is_superhost)
train$cancellation_policy <- as.factor(train$cancellation_policy)
boostCV66 = gbm(price ~ price_mean_c + bedrooms + cleaning_fee + room_type + minimum_nights 
                + accommodates + property_type + bathrooms + beds + neighbourhood_cleansed + host_is_superhost
                + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
                + reviews_per_month + cancellation_policy
                + accommodates + availability_90
                + Airconditioning + Dryer + Elevator + Family_and_kidfriendly +
                  + review_scores_cleanliness + Hairdryer + Refrigerator + Shampoo + number_of_reviews 
                + Selfcheck_in  + TV + maximum_nights + host_listings_count + cleaning_fee 
                + monthly_price + review_scores_accuracy + square_feet + property_type
               ,data = train, distribution = "gaussian", 
               n.trees = 200,   
               interaction.depth = 4,
               shrinkage = 0.01,
               n.minobsinnode = 5)

summary(boostCV6)


## predict scoring dataset
pred_boostscore66 = predict(boostCV66, n.trees = 200, newdata = scoring)

submissionFile = data.frame(id = scoring$id, price = pred_boostscore66)
write.csv(submissionFile, 'boost66.csv',row.names = F)  #


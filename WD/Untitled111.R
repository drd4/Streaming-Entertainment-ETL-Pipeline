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


## First, the amenities column
scoring$amenities = gsub("\\.", "", scoring$amenities)  
scoring$amenities = scoring$amenities %>% stringr::str_replace_all("\\s", "")   
scoring$amenities = noquote(scoring$amenities)  
scoring = cbind(scoring,mtabulate(strsplit(as.character(scoring$amenities), ',')))



#

# data$zipcode <- substr(data$zipcode, 1, 5)
# data$zipcode[nchar(data$zipcode)<5] <- NA_character_
# data$zipcode <- as.factor(data$zipcode)
# data$zipcode <- forcats::fct_lump_n(data$zipcode, 40) # combine factor levels for zipcodes less than 40

library(tidyverse)



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
scoring$accommodates[is.na(scoring$review_scores_rating)] <- median(scoring$review_scores_rating, na.rm=TRUE)
scoring %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> scoring
### data
## create a data frame to use the neighbourhood_cleansed
data1 = data %>%
  group_by(zipcode = zipcode) %>%
  summarize(record_count_c = n(),     ## count every level's record
            price_mean_c = mean(price)) %>%  ## calculate each group's mean price
  arrange(desc(record_count_c))


## merge the data frame and get the mean price and record counts
data = merge(data, data1, by = c("zipcode", "zipcode"))

library(data.table)
data.table(data)

# 
# scoring$zipcode <- substr(scoring$zipcode, 1, 5)
# scoring$zipcode[nchar(scoring$zipcode)<5] <- NA_character_
# scoring$zipcode <- as.factor(scoring$zipcode)
# scoring$zipcode <- forcats::fct_lump_n(scoring$zipcode, 40) # combine factor levels for zipcodes less than 40

### scoring
## create a data frame to use the neighbourhood_cleansed
scoring1 = scoring %>%
  group_by(zipcode = zipcode) %>%
  summarize(record_count_c = n()) %>%
  arrange(desc(record_count_c))



## merge the data frame and get the mean price and record counts
scoring = merge(scoring, scoring1, by = c("zipcode", "zipcode"))



## create price_mean_c for score_data 
data2 = data.frame(zipcode = data1$zipcode,
                   price_mean_c = data1$price_mean_c)

scoring = merge(scoring, data2, by = c("zipcode", "zipcode"), all.x = TRUE) 

sum(is.na(scoring$price_mean_c))
scoring[is.na(scoring$price_mean_c),]$price_mean_c = mean(scoring$price_mean_c, na.rm = TRUE)

names(data)

colnames(data)[which(colnames(data) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(data)[which(colnames(data) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'

colnames(scoring)[which(colnames(scoring) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(scoring)[which(colnames(scoring) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA


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
trControl_boostt = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boostt = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  
## after trying different values of parameters, I found these values are the best according to the cross-validation results
a <- lm(price ~ price_mean_c +  bedrooms + cleaning_fee + room_type + minimum_nights 
        + accommodates + bathrooms + beds +zipcode+ 
          + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
        + reviews_per_month + cancellation_policy
        + extra_people + guests_included + availability_90
        + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking 
        + review_scores_cleanliness + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
        + Selfcheck_in  + TV + maximum_nights + host_listings_count 
        + monthly_price + review_scores_accuracy + square_feet +  accommodates 
        ,data = train)
summary(a)
preda = predict(a, newdata = test)

cvBoost6 = train(price ~  price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
                 + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed + host_is_superhost
                 + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
                 + transit_wc + summary_wc+ description_wc+ host_since_days + reviews_per_month + cancellation_policy
                 + last_review_days + first_review_days + extra_people + guests_included + availability_90
                 + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking
                 + review_scores_cleanliness + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
                 + host_about_wc + Selfcheck_in  + TV + selfie + kba + jumio + maximum_nights + host_listings_count 
                 + neighborhood_overview_wc + host_has_profile_pic + monthly_price + review_scores_accuracy
                 ,data = train, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6)


cvBoost6
names(train)
### start here*********
## to save time, I only use 2,000 trees here, the best model used 30,000 trees
boostCV6 = gbm(price ~ price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
               + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed + host_is_superhost
               + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
               + transit_wc + summary_wc+ description_wc+ host_since_days + reviews_per_month + cancellation_policy
               + last_review_days + first_review_days + extra_people + guests_included + availability_90
               + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking
               + review_scores_cleanliness + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
               + host_about_wc + Selfcheck_in  + TV + selfie + kba + jumio + maximum_nights + host_listings_count 
               + neighborhood_overview_wc + host_has_profile_pic + monthly_price + review_scores_accuracy
               ,data = train, distribution = "gaussian", 
               n.trees = 2000,   
               interaction.depth = 5,
               shrinkage = 0.005,
               n.minobsinnode = 5)

summary(boostCV6)


## predict train dataset
predBoostCV6 = predict(boostCV6, n.trees = 2000)
RMSE(predBoostCV6, train$price)

## predict test dataset
predBoostCV6_test = predict(boostCV6, test, n.trees = 2000)
RMSE(predBoostCV6, test$price)

## predict scoring dataset
pred_boostscore6 = predict(boostCV6, n.trees = 2000, newdata = scoring)

submissionFile = data.frame(id = scoring$id, price = pred_boostscore6)
write.csv(submissionFile, 'boost6.csv',row.names = F)  #56


#### my boost ###################################


library(gbm)
set.seed(617)
trControl_boost6 = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boost6 = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  
cvBoost1 = train(price ~ price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
                 + accommodates + property_type + bathrooms + beds  + host_is_superhost
                 + security_deposit + availability_30  + review_scores_rating + availability_60
                 +  reviews_per_month + cancellation_policy
                 + extra_people + guests_included + availability_90
                 + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking 
                 + review_scores_cleanliness + Hairdryer + Iron + Shampoo + number_of_reviews 
                 + Selfcheck_in  + TV + host_listings_count + minimum_nights + square_feet 
                 + host_has_profile_pic + monthly_price
                 ,data = train, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6)



### start here*********
## to save time, I only use 2,000 trees here, the best model used 30,000 trees
boostCV1 = gbm(price ~ price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
               + accommodates + property_type + bathrooms + beds  + host_is_superhost
               + security_deposit + availability_30  + review_scores_rating + availability_60
               +  reviews_per_month + cancellation_policy
               + extra_people + guests_included + availability_90
               + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking 
               + review_scores_cleanliness + Hairdryer + Iron + Shampoo + number_of_reviews 
               + Selfcheck_in  + TV + host_listings_count + minimum_nights + square_feet 
               + host_has_profile_pic + monthly_price
               ,data = train, distribution = "gaussian", 
               n.trees = 200,   
               interaction.depth = 5,
               shrinkage = 0.01,
               n.minobsinnode = 5)

pred_boostscore = predict(boostCV1, n.trees = 200, newdata = scoring)

submissionFile1 = data.frame(id = scoring$id, price = pred_boostscore)
write.csv(submissionFile1, 'boost1.csv',row.names = F)  #66


###

#look at price mean





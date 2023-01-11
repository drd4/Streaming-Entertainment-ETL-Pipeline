data = read.csv('analysisData.csv')
scoring = read.csv('scoringData.csv')

library(ngram)
library(dplyr)
library(stringr)
library(qdapTools)
library(tidyverse)
library(data.table)
library(caret)
library(gbm)
#rm(scoring1)
# rm(data)
# rm(data1)
# rm(data111)
# rm(data2)
# rm(data4)
# rm(test)
# rm(train)
# rm(split)
scoring$zipcode <- as.character(scoring$zipcode)
total <- bind_rows(data, scoring)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode[is.na(total$zipcode)] <- "Other"
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40


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
data111 <- total[!is.na(total$price),]
scoring111 <- total[is.na(total$price),]


## create a data frame to use the neighbourhood_cleansed
data1 = data111 %>%
  group_by(zipcode = zipcode) %>%
  summarize(record_count_c = n(),     ## count every level's record
            price_mean_c = mean(price)) %>%  ## calculate each group's mean price
  arrange(desc(record_count_c))


## merge the data frame and get the mean price and record counts
data4 = merge(data111, data1, by = c("zipcode", "zipcode"))

library(data.table)
data.table(data4)

# 
# scoring$zipcode <- substr(scoring$zipcode, 1, 5)
# scoring$zipcode[nchar(scoring$zipcode)<5] <- NA_character_
# scoring$zipcode <- as.factor(scoring$zipcode)
# scoring$zipcode <- forcats::fct_lump_n(scoring$zipcode, 40) # combine factor levels for zipcodes less than 40

### scoring
## create a data frame to use the neighbourhood_cleansed
scoring1 = scoring111 %>%
  group_by(zipcode = zipcode) %>%
  summarize(record_count_c = n()) %>%
  arrange(desc(record_count_c))



## merge the data frame and get the mean price and record counts
scoring4 = merge(scoring111, scoring1, by = c("zipcode", "zipcode"))



## create price_mean_c for score_data 
data2 = data.frame(zipcode = data1$zipcode,
                   price_mean_c = data1$price_mean_c)

scoring = merge(scoring4, data2, by = c("zipcode", "zipcode"), all.x = TRUE) 

sum(is.na(scoring$price_mean_c))
#scoring[is.na(scoring$price_mean_c),]$price_mean_c = mean(scoring$price_mean_c, na.rm = TRUE)

str(scoring$zipcode)
str(data4$zipcode)
### final is scoring, data4

table(scoring$zipcode)
table(data4$zipcode)
library(caret)
library(car)

library(gbm)
set.seed(617)
trControl_boostt = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boostt = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  


b <- lm(price ~ price_mean_c + bedrooms+bathrooms + accommodates  + TV + Elevator + minimum_nights + Shampoo + review_scores_rating + square_feet
        + availability_30 + property_type + guests_included + Hairdryer + Freestreetparking  , data = data4 )

vif(b)
alias(b)
summary(b)
predd = predict(b, newdata=scoring)
su = data.frame(id = scoring$id, price = predd)
su
write.csv(su, 'lm2.csv',row.names = F) #71


library(gbm)
set.seed(617)
trControl_boostt = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boostt = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnod

cvBoostme = train(price ~ price_mean_c + bedrooms+bathrooms + accommodates  + TV + Elevator + minimum_nights + Shampoo + review_scores_rating + square_feet + availability_30 + property_type + guests_included + Hairdryer + Freestreetparking,data = data4, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6)


### doing thissss
data4$property_type <- as.factor(data4$property_type)
### start here*********
## to save time, I only use 2,000 trees here, the best model used 30,000 trees
boostCVme = gbm(price ~ price_mean_c + bedrooms+bathrooms + accommodates  + TV + Elevator + minimum_nights + Shampoo + review_scores_rating 
              + square_feet + availability_30 + property_type + guests_included + Hairdryer + Freestreetparking,data = data4, distribution = "gaussian", 
               n.trees = 200,   
               interaction.depth = 5,
               shrinkage = 0.005,
               n.minobsinnode = 5)

summary(boostCVme)



## predict scoring dataset
pred_boostscoremw = predict(boostCVme, n.trees = 200, newdata = scoring)

submissionFile = data.frame(id = scoring$id, price = pred_boostscoremw)
write.csv(submissionFile, 'boostme.csv',row.names = F)  ## need to try this 

#different try

cvBoostme2 = train(price ~ price_mean_c + bedrooms+bathrooms + accommodates  + TV + Elevator + minimum_nights + Shampoo + review_scores_rating+ square_feet + availability_30 + availability_60 + property_type + guests_included + Dryer + Freestreetparking + Elevator + Pool,data = data4, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6)
summary(cvBoostme2)
boostCVme2 = gbm(price ~ price_mean_c + bedrooms+bathrooms + accommodates  + TV + Elevator + minimum_nights + Shampoo + review_scores_rating+ square_feet + availability_30 + availability_60 + property_type + guests_included + Dryer + Freestreetparking + Elevator + Pool,data = data4, distribution = "gaussian", 
                 n.trees = 200,   
                 interaction.depth = 5,
                 shrinkage = 0.005,
                 n.minobsinnode = 5)

pred_boostscoremw2 = predict(boostCVme2, n.trees = 200, newdata = scoring)

submissionFile2 = data.frame(id = scoring$id, price = pred_boostscoremw2)
write.csv(submissionFile2, 'boostme2.csv',row.names = F)  ## need to try this 


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






#Daniel Dasgupta
#Kaggle Project R Code
# 11/30/2020

# read in the data 
data = read.csv('analysisData.csv')
scoring = read.csv('scoringData.csv')


########################## 
library(ngram)
library(dplyr)
library(stringr)
library(qdapTools)
library(tidyverse)
library(data.table)
library(caret)
library(gbm)
library(corrplot)


#Before cleaning the data, I evaluated the differences between the two datasets to determine what needed to be done in my analysis:
# The analysis dataset (which I named 'data'), has 39,527 observations with 91 variables. The scoring dataset, had 9,882 observations with 90 variables: the difference between the two datasets is the inclusion of price in the analysis dataset.
#I wanted to take a closer look at the contents of the dataset which variables had missing values or outliers.  


# Instead of cleaning both datasets seperately, I combinded them to create uniformity, but I first needed to explore the data structures

str(data)
str(scoring)

#in the scoring dataset, zipcode is a integer, and an character in the analysis dataset, therefore, I needed to convert zipcode to a character to combine the two datasets
scoring$zipcode <- as.character(scoring$zipcode) 

#I then combined both data sets into total
total <- bind_rows(data, scoring)

# After combining the data, I checked which variables have missing data, so I can later on replace missing values

missing_col_total = colSums(is.na(total)); missing_col_total[missing_col_total > 0] #Of the variables of interest, square feet, beds, cleaning fee all had a lot of N/A values

#I then created a correlation matrix of numeric variables to visualize which variables are highly correlated to price

numericVars <- which(sapply(total, is.numeric)) #index numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on

total_numVar <- total[, numericVars]
total_numVar$id <- NULL   #remove id
cor_numVar <- cor(total_numVar, use="pairwise.complete.obs") #correlations of total numeric variables
#sort on decreasing correlations with price
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="blue", tl.pos = "lt")


#I then decided to replace N/A values of bedrooms and bathrooms - as it can be assumed that an Airbnb will have at least one bathroom and one bedroom - most likely the owner of the Airbnb did not properly fill out the listing

total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1

#For cleaning fee, square feet, review scores rating and accomodates- I decided to replace N/A values with the median of the sample


total$cleaning_fee[is.na(total$cleaning_fee)] <- median(total$cleaning_fee, na.rm=TRUE)
total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$review_scores_rating[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm = TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)


# I also identified a lot of different property types
table(total$property_type)
# I wanted to remove outliers, such as 'Dome House' that only had one listing. Overall, I removed property types with less than 5 listings, and replaced the value with 'Other'

total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total

table(total$property_type)

# from here, it was time to clean out the amenities variable, and create dummy variables based on the different amenities listed

#remove . 
total$amenities = gsub("\\.", "", total$amenities)  
#remove spaces
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   

#remove quoation sign 
total$amenities = noquote(total$amenities)  

## Second split the column and create dummy variables
total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))

#This resulted in creating 141 new dummy variables to account for ammendity offerings such as Elevator, Dryer, Airconditioning


# It then occurred to me that Airbnb prices are highly related to their locations- just as in NYC, certain neighbourhoods are more expensive than others
# From here, I thought it would make sense to create a new variable, neighbourhood mean price, to provide my model with further information to price the prices of the scoring dataset.  I grouped the neighborhoods, and calculated the mean price by neighbourhod
## but - before I could calculate the mean, I need to split the data back into the scoring and the analysis data


data1 <- total[!is.na(total$price),]
scoring1 <- total[is.na(total$price),]

# after splitting the data back to the orginals, I can then create the group neighbourhood price mean - while doing so, I need to reduce the number of levels- just like I did for property type

data1 %>% 
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in% names(which(table(neighbourhood_cleansed) <= 5)) ~ "Other",
    TRUE ~ neighbourhood_cleansed
  )) -> data1

table(data1$neighbourhood_cleansed)

#now create the price mean variable
data2 = data1 %>% 
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(price_mean = mean(price))

head(data2)

#then merge the data back together
data1 = merge(data1, data2, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

## from here- I can then use the mean price from data1, and put it into scoring1

scoring1 %>% 
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in% names(which(table(neighbourhood_cleansed) <= 5)) ~ "Other",
    TRUE ~ neighbourhood_cleansed
  )) -> scoring1

scoring2 = data.frame(neighbourhood_cleansed = data1$neighbourhood_cleansed,
                      price_mean = data1$price_mean)
scoring3 = unique(scoring2) # remove duplicate values

scoring4 = merge(scoring1, scoring3, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))

## after this computation, our two datasets are scoring4 and data1 



#### First model, linear regression


set.seed(617)
mod1 <- lm(price ~ price_mean  + bedrooms + cleaning_fee + room_type + minimum_nights 
           + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed
           + security_deposit + availability_30 + review_scores_rating 
           + reviews_per_month +  guests_included + Refrigerator + 
           + Airconditioning + Dryer + Elevator + Freestreetparking
           + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
           + TV + maximum_nights + host_listings_count + monthly_price + review_scores_accuracy, data = data1)

summary(mod1)
predmod1 = predict(mod1, newdata= scoring4)


submissionFile1 = data.frame(id = scoring4$id, price = predmod1)
write.csv(submissionFile1, '11-9(2).csv',row.names = F)
## RMSE: 65.993


#### Second model, random forest

library(randomForest)

set.seed(617)
forest = randomForest(price ~price_mean  + bedrooms + cleaning_fee + room_type + minimum_nights 
                      + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed
                      + security_deposit + availability_30 + review_scores_rating 
                      + reviews_per_month +  guests_included + Refrigerator + 
                        + Airconditioning + Dryer + Elevator + Freestreetparking
                      + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
                      + TV + maximum_nights + host_listings_count + monthly_price + review_scores_accuracy, data = data1, ntree = 400)
predforest = predict(forest, newdata = scoring1)

submissionFile2 = data.frame(id = scoring4$id, price = predforest)
write.csv(submissionFile2, 'q.csv',row.names = F)
## RMSE: 61.629


#### Third model, boosting 
set.seed(617)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 2000,
                       interaction.depth = c(1,2,3), 
                       shrinkage = (1:100)*0.001, 
                       n.minobsinnode=c(5,10,15))

garbage = capture.output(cvModel <- data1(price ~ price_mean  + bedrooms + cleaning_fee + room_type + minimum_nights 
                                          + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed
                                          + security_deposit + availability_30 + review_scores_rating 
                                          + reviews_per_month +  guests_included + Refrigerator + 
                                            + Airconditioning + Dryer + Elevator + Freestreetparking
                                          + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
                                          + TV + maximum_nights + host_listings_count + monthly_price + review_scores_accuracy, data=data1,
                                          method="gbm", trControl=trControl, tuneGrid=tuneGrid))


boost = gbm(price ~ price_mean  + bedrooms + cleaning_fee + room_type + minimum_nights 
            + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed
            + security_deposit + availability_30 + review_scores_rating 
            + reviews_per_month +  guests_included + Refrigerator + 
              + Airconditioning + Dryer + Elevator + Freestreetparking
            + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
            + TV + maximum_nights + host_listings_count + monthly_price + review_scores_accuracy, data = train, distribution = "gaussian", 
            n.trees=cvModel$bestTune$n.trees,
            interaction.depth=cvModel$bestTune$interaction.depth, 
            shrinkage=cvModel$bestTune$shrinkage,
            n.minobsinnode = cvModel$bestTune$n.minobsinnode)

summary(boostCV6)
predboost = predict(boost, newdata= scoring1)
submissionFile3 = data.frame(id = scoring4$id, price = predboost)
write.csv(submissionFile3, "boost.csv", row.names = F)

##RMSE: 56.476












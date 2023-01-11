data = read.csv('analysisData.csv')
scoring = read.csv('scoringData.csv')




for (i in 1:ncol(data)) {
  if (is.numeric(data[,i])) {
    data[is.na(data[,i]), i] = mean(data[,i], na.rm = TRUE)
  }
}
missing_col_data = colSums(is.na(data)); missing_col_data[missing_col_data > 0]

for (i in 1:ncol(scoring)) {
  if (is.numeric(scoring[,i])) {
    scoring[is.na(scoring[,i]), i] = mean(scoring[,i], na.rm = TRUE)
  }
}
missing_col_score = colSums(is.na(scoring)); missing_col_score[missing_col_score > 0]


data[apply(data, 2, function(x) x=="")] = NA
scoring[apply(scoring, 2, function(x) x=="")] = NA

for (i in 1:ncol(data)) {
  if (is.factor(data[,i])) {
    data[,i] = addNA(data[,i])
  }
}
sum(is.na(data$summary))


for (i in 1:ncol(scoring)) {
  if (is.factor(scoring[,i])) {
    scoring[,i] = addNA(scoring[,i])
  }
}
sum(is.na(scoring$summary))

data$host_since = as.Date(data$host_since)
data$first_review = as.Date(data$first_review)
data$last_review = as.Date(data$last_review)

scoring$host_since = as.Date(scoring$host_since)
scoring$first_review = as.Date(scoring$first_review)
scoring$last_review = as.Date(scoring$last_review)
data$summary = as.character(data$summary)
data$description = as.character(data$description)
data$transit = as.character(data$transit)
data$host_about = as.character(data$host_about)
data$amenities = as.character(data$amenities)
data$host_verifications = as.character(data$host_verifications)
data$space = as.character(data$space)
data$neighborhood_overview = as.character(data$neighborhood_overview)

scoring$summary = as.character(scoring$summary)
scoring$description = as.character(scoring$description)
scoring$transit = as.character(scoring$transit)
scoring$host_about = as.character(scoring$host_about)
scoring$amenities = as.character(scoring$amenities)
scoring$host_verifications = as.character(scoring$host_verifications)
scoring$space = as.character(scoring$space)
scoring$neighborhood_overview = as.character(scoring$neighborhood_overview)


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


colnames(data)[which(colnames(data) == '24-hourcheck-in')] = 'check_in_24'
colnames(data)[which(colnames(data) == 'Cat(s)')] = 'cat'
colnames(data)[which(colnames(data) == 'Dog(s)')] = 'dog'
colnames(data)[which(colnames(data) == 'Otherpet(s)')] = 'otherpet'
colnames(data)[which(colnames(data) == 'Washer/Dryer')] = 'Washer_and_Dryer'
colnames(data)[which(colnames(data) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(data)[which(colnames(data) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'

colnames(scoring)[which(colnames(scoring) == '24-hourcheck-in')] = 'check_in_24'
colnames(scoring)[which(colnames(scoring) == 'Cat(s)')] = 'cat'
colnames(scoring)[which(colnames(scoring) == 'Dog(s)')] = 'dog'
colnames(scoring)[which(colnames(scoring) == 'Otherpet(s)')] = 'otherpet'
colnames(scoring)[which(colnames(scoring) == 'Washer/Dryer')] = 'Washer_and_Dryer'
colnames(scoring)[which(colnames(scoring) == 'Selfcheck-in')] = 'Selfcheck_in'
colnames(scoring)[which(colnames(scoring) == 'Family/kidfriendly')] = 'Family_and_kidfriendly'

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
### remember to use all.x = TRUE here to keep scoring as a whole

## check the missing value
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
data$new_neigh_c = as.factor(data$new_neigh_c)


library(gbm)
set.seed(617)
trControl_boost6 = trainControl(method = 'cv', number = 5)  ## use 5-fold cross validation
tuneGrid_boost6 = expand.grid(n.trees = 200, interaction.depth = c(3,4,5),
                              shrinkage = c(0.005, 0.001, 0.01), n.minobsinnode = c(5,10,15))  
## after trying different values of parameters, I found these values are the best according to the cross-validation results

cvBoost6 = train(price ~ price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
                 + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed + host_is_superhost
                 + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
                 + reviews_per_month + cancellation_policy
                  + extra_people + guests_included + availability_90
                 + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking 
                 + review_scores_cleanliness + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
                 +  Selfcheck_in  + TV + maximum_nights + host_listings_count 
                 + host_has_profile_pic + monthly_price + review_scores_accuracy
                 ,data = train, method = 'gbm', trControl = trControl_boost6, tuneGrid = tuneGrid_boost6, na.action = na.omit)

##### THIS IS WHERE YOU SAY SOMETHING WENT WRONG, THAT YOU DIDNT HAVE THIS WERID ERROR OF NA.FAIL.DEFAULT, MISSING VALUES IN OBJECT, AND THATS WHAT EFFECTED YOUR SCORE


cvBoost6

train$room_type <- as.factor(train$room_type)
train$property_type <- as.factor(train$property_type)
train$neighbourhood_group_cleansed <- as.factor(train$neighbourhood_group_cleansed)
train$cancellation_policy <- as.factor(train$cancellation_policy)
train$new_neigh_c <- as.factor(train$new_neigh_c)
train$host_is_superhost <- as.factor(train$host_is_superhost)
train$host_has_profile_pic <- as.factor(train$host_has_profile_pic)

boostCV6 = gbm(price ~ price_mean_c + new_neigh_c + price_mean_gc + bedrooms + cleaning_fee + room_type + minimum_nights 
               + accommodates + property_type + bathrooms + beds + neighbourhood_group_cleansed + host_is_superhost
               + security_deposit + availability_30 + availability_365 + review_scores_rating + availability_60
               + reviews_per_month + cancellation_policy
               + extra_people + guests_included + availability_90
               + Airconditioning + Dryer + Elevator + Family_and_kidfriendly + Freestreetparking 
               + review_scores_cleanliness + Hairdryer + Iron + Oven + Refrigerator + Shampoo + number_of_reviews 
               +  Selfcheck_in  + TV + maximum_nights + host_listings_count 
               + host_has_profile_pic + monthly_price + review_scores_accuracy
               ,data = train, distribution = "gaussian", 
               n.trees = 200,   
               interaction.depth = 5,
               shrinkage = 0.01,
               n.minobsinnode = 10)

summary(boostCV6)


pred_boostscore6 = predict(boostCV6, n.trees = 200, newdata = scoring)   # in predict.gbm() - NAs introduced by coersion


sub = data.frame(id = scoring$id, price = pred_boostscore6)
write.csv(sub, 'rep.csv',row.names = F)  #79

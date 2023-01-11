#Kaggle 

scoringData = read.csv('scoringData.csv', stringsAsFactors = T)
analysis <- read.csv("analysisData.csv", stringsAsFactors = T)
#sample_sub <- read.csv("sample_submission.csv")

# Read data and construct a simple model
data = read.csv('analysisData.csv', stringsAsFactors = T)
model = lm(price~minimum_nights+review_scores_accuracy,data)

# Read scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)

# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)


#First lets look at the distribution of important variables in the analysis 

head(analysis)
list(analysis)
str(analysis)

library(tidyverse)
#lets first select the important variablaes
data_sub <- analysis %>% select(host_since, host_location, host_response_time, host_response_rate, host_acceptance_rate, host_neighbourhood, host_has_profile_pic, host_identity_verified)

mod1 <- lm(price~bedrooms, data = analysisData)
pred = predict(mod1, newdata= scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'first_submission.csv',row.names = F)

attach(analysis)
library(modelr)
summary(model1)
model1 <- lm(price ~  bathrooms+ bedrooms+ beds + neighbourhood, data = analysis)
pred = predict(model1,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'first_submission.csv',row.names = F)

model2 <- lm(price~ review_scores_rating, data = analysis)
pred = predict(model2, newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'second_submission.csv', row.names = F)

model3 <- lm(price ~bathrooms + bedrooms + beds, + square_feet, data = analysis)
pred = predict(model3, newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'third_submission.csv', row.names = F)


model4 <- lm(price ~weekly_price + review_scores_rating  + bedrooms, data = analysis)
summary(model4)
pred = predict(model4, newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'forth_submission.csv', row.names = F)


################################### Live Demo of Decision Tree

#Decision Tree Example

data = read.csv("analysisData.csv")
dim(data)

library(rpart)

tree <- rpart(price ~ minimum_nights + review_scores_accuracy +  review_scores_cleanliness + calendar_updated, 
                cp = 0.01,
                method = "anova",
                data= data)

library(rpart.plot)

rpart.plot(tree)

#try different complexibilities
tree <- rpart(price ~ minimum_nights + review_scores_accuracy +  review_scores_cleanliness + weekly_price, 
              cp = 0.001,
              method = "anova",
              data= data)
rpart.plot(tree)

scoringData = read.csv("scoringData.csv")
pred = predict(tree, newdata = scoringData)

head(pred)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'demo_submission.csv', row.names = F)


#tuning the complexity parameter
#feature selection 

#concate both data sets, fit the model togethre, so the factor models have the same levels, remove the merged data as it doesnt have price and then go from there


# relace N/A with means 
#replace the scoring data, replace it with the global mean of the analysis data 


#collapse in category 


#variable selection in 


#lets try refactoring 
total <- rbind(analysis, scoringData)
model1 <- lm(price ~  bathrooms+ bedrooms+ beds + neighbourhood, data = analysis)
pred = predict(model1,newdata=scoringData)


submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'first_submission.csv',row.names = F)


library(tidyverse)


a <- select(analysis, -price)
total <- rbind(a, scoringData)


scoringData = read.csv('scoringData.csv', stringsAsFactors = T)
analysis <- read.csv("analysisData.csv")

analysis$neighbourhood = as.character(analysis$neighbourhood)
scoringData$neighbourhood = as.character(scoringData$neighbourhood)
model1 <- lm(price ~  bathrooms+ bedrooms+ beds + zipcode + host_is_superhost, data = analysis)
pred = predict(model1,newdata=scoringData)
summary(model1)

model1 <- lm(price~neighbourhood, data = analysis)
pred = predict(model1,newdata=scoringData)

analysis$zipcode = as.numeric(analysis$zipcode)
scoringData$zipcode = as.numeric(scoringData$zipcode)

cor.test(analysis$price, analysis$zipcode)
mod <- lm(price~host_name, data = analysis)
summary(mod)
anova(analysis$price, analysis$host_is_superhost)

model2 <- glm(price~host_is_superhost, data = analysis)
summary(model2)
# how do pick a number for set seed?

#factor variables


levels(scoringData$neighbourhood)
levels(analysis$neighbourhood)

summary(analysis$neighbourhood)

library(tidyverse)
train_sub <- analysis  %>% select(price, id, zipcode, city, smart_location, property_type, room_type, accommodates, bathrooms, bedrooms, beds, cleaning_fee, minimum_nights, maximum_nights, number_of_reviews, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value)
library(leaps)
subsets= regsubsets(price~., data=train_sub, nvmax = 22)

str(analysis)

cor.test(analysis$price, analysis$number_of_reviews)
cor.test(analysis$price, analysis$review_scores_rating)
cor.test(analysis$price, analysis$review_scores_accuracy)
cor.test(analysis$price, analysis$review_scores_cleanliness)
cor.test(analysis$price, analysis$review_scores_location)
cor.test(analysis$price, analysis$room_type)

a <- lm(price~room_type, data = analysis)

summary(a)


t_sub <- analysis %>% select(price, id,  review_scores_rating, room_type, review_scores_location, bedrooms, bathrooms, zipcode, city, property_type, square_feet)
subsets <- regsubsets(price~. , data=t_sub, nvmax=8)
## which variables are included in the model
summary(subsets)

#check model performance
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic,
                              adjr2=summary(subsets)$adjr2)




a <- lm(price~., data = t_sub)
summary(a)
vif(a)
pred = predict(a,newdata=scoringData)



## Best subset selection
library(leaps)

subsets <- regsubsets(quality~. , data=train, nvmax=11)
## which variables are included in the model
summary(subsets)

#check model performance
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic,
                              adjr2=summary(subsets)$adjr2)

## Viz performance
library(ggplot2)
library(tidyr)
subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  ggplot(aes(x=model,y=value))+
  geom_line()+
  geom_point()+
  facet_grid(type~.,scales='free_y')

## best model by Cp
sel <- which.min(summary(subsets)$cp)
## coefficients of the best model
coef(subsets, sel)


# 10/30 
library(tidyverse)
t_sub <- analysis %>% select(price, id,  review_scores_rating, review_scores_location, bedrooms, bathrooms, zipcode, property_type, square_feet)
t_sub$zipcode = as.numeric(t_sub$zipcode)
scoringData$zipcode = as.numeric(scoringData$zipcode)
t_sub$property_type = as.character(t_sub$property_type)
scoringData$property_type = as.character(scoringData$property_type)
mod <- lm(price~ + review_scores_rating + review_scores_location + bedrooms + bathrooms + zipcode + property_type + square_feet, data = t_sub)
summary(mod)
pred = predict(mod,newdata=scoringData)
str(t_sub$zipcode)
str(scoringData$zipcode)
str(scoringData$property_type)
str(t_sub$property_type)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, '10-30.csv',row.names = F)

scoringData$property_type <- factor(scoringData$property_type, levels=levels(t_sub$property_type))
scoringData$zipcode <- factor(scoringData$zipcode, levels = levels(t_sub$zipcode))



mod <- lm(price~bedrooms + bathrooms+ property_type, data = analysis)
summary(mod)
pred = predict(mod,newdata=scoringData)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, '10-30.csv',row.names = F)

### Office hours



library(tidyverse)
train_sub <- analysis  %>% select(price, id, zipcode, city, smart_location, property_type, room_type, accommodates, bathrooms, bedrooms, beds, cleaning_fee, minimum_nights, maximum_nights, number_of_reviews, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value)

library(leaps)
subsets= regsubsets(price~., data=train_sub, nvmax = 22)



### Office hours data cleaning 

data <- analysis

str(data)

# one big difference between the scoring and analysis is zipcode 
is.numeric(scoringData$zipcode) #TRUE
is.numeric(analysis$zipcode)  #FALSE

# in this case, they really are categorical data

scoringData$zipcode <- as.character(scoringData$zipcode)
analysis$zipcode <- as.character(analysis$zipcode)

## might want to combine the analysis and scoring data into a single combinded dataset

# combining data to joinly deal with factor labels and missing data

combindedData <- bind_rows(analysis, scoringData)  

combindedData$price
tail(combindedData$price) # combined the data, but price now has NA

table(combindedData$zipcode)  # from here, we can see empty strings, some zipcodes have the - that arent specific enough



# modify zipcode

combindedData$zipcode <- substr(combindedData$zipcode, 1, 5)
combindedData$zipcode[nchar(combindedData$zipcode)<5] <- NA #filter out zipcodes less than five
combindedData$zipcode <- as.factor(combindedData$zipcode)




levels(combindedData$zipcode)

# city 
table(combindedData$city)
?gsub #global substitution of various pattern
combindedData$city <- gsub("-|\\.", " ", combindedData$city)

gsub("-|/|\\n|nn)|//(|\\.|\\,", " ", combindedData$city)  # the | is or, so can include multiple things

combindedData$city <- gsub("\\s+", " ", combindedData$city)  #collapse multiple spaces

library(stringr)
combindedData$city <- str_trim(combindedData$city)

# convert to lower
combindedData$city <- tolower(combindedData$city)


# property types
# lets say remove property types with less than 10 and convert them to NA
table(combindedData$property_type)
which(table(combindedData$property_type) <= 10)
names(which(table(combindedData$property_type) <= 10))


combindedData$property_type <- as.character(combindedData$property_type)
combindedData %>%
  mutate(property_type = case_when(
    property_type %in% names(which(table(combindedData$property_type) <= 10)) ~ NA_character_,
    TRUE ~property_type
  )) -> combindedData

#converting characters to NA's

char2na <- function(x){
  case_when(
    x == "" ~NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  )
}

combindedData %>%
  mutate_if(is.character, char2na) -> combindedData

table(combindedData$transit)

# Convert Character types to factors

colnames(combindedData)
head(combindedData$description)  # the description is just a large amount of data that is individualistic, want to simplfy things as much as possible

sum(table(combindedData$description) == 1) #find all values equal to 1
nrow(combindedData)

sum(table(combindedData$description) == 1) # like an id value, how is it helpful really tho


## create a loop   ## shit is wrong tho
char_ind <- which(sapply(combindedData, class) == "character")
char_ind_rm <- c()
for (i in char_ind) {
  ntypes <- sum(table(combindedData[,i]) == 1)
  if (ntypes/nrow(combindedData) > .3 | ntypes == 1){
    char_ind_rm <- c(char_ind_rm, i)
  }
}

char_ind_rm
length(char_ind_rm)

#convert chars to factors

combindedData <- mutate_if(combindedData, is.character, as.factor)


## Caret package for data pre


# use stepwise selection, 
# might use shrinkage, or lasso to try for example 
#glm net might work


# decison trees are probably the best

#######################################################
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)


combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA_character_
combinedData$zipcode <- as.factor(combinedData$zipcode)

combinedData$city <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$city))
combinedData$city <- stringr::str_trim(gsub("\\s+", " ", combinedData$city))

combinedData %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 10)) ~ NA_character_,
    TRUE ~ property_type
  )) -> combinedData

char2na <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  )
}
combinedData %>%
  mutate_if(is.character, char2na) -> combinedData

char_ind <-  which(sapply(combinedData, class) == "character")
# flag variables for removal if there are too many cats or only 1 cat
char_ind_rm <- c()
for (i in char_ind) {
  ntypes <- sum(table(combinedData[,i])==1)
  if (ntypes/nrow(combinedData) > .3 | ntypes == 1 ) {
    char_ind_rm <- c(char_ind_rm, i)
  }
}
combinedData <- combinedData[,-char_ind_rm]

combinedData <- mutate_if(combinedData, is.character, as.factor)

boxplot(combinedData$square_feet)

sqft_99 <- quantile(combinedData$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(combinedData$price) ~ TRUE,
  is.na(combinedData$square_feet) ~ TRUE,
  combinedData$square_feet > sqft_99 ~ FALSE,
  combinedData$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

combinedData <- combinedData[keep_index,]


library(caret)
zero_var_table <- nearZeroVar(combinedData, saveMetrics= TRUE)
combinedData <- combinedData[, !zero_var_table$nzv]

combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)

numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))
imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')


set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])


train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]

head(train)

mod1 <- lm(price~zipcode, data = train)
summary(mod1)
pred = predict(mod1,newdata=test)


# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

####################################################
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)


combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA_character_
combinedData$zipcode <- as.factor(combinedData$zipcode)


sqft_99 <- quantile(combinedData$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(combinedData$price) ~ TRUE,
  is.na(combinedData$square_feet) ~ TRUE,
  combinedData$square_feet > sqft_99 ~ FALSE,
  combinedData$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

combinedData <- combinedData[keep_index,]



### Imputation ###
# impute missing values

# median replacement: by hand
combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)

## select numeric variables for imputation models
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])


## CITY ##
## remove token separators
combinedData$neigh <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$city))
## trim whitespace
combinedData$city <- stringr::str_trim(gsub("\\s+", " ", combinedData$city))

combinedData %>%
  mutate(room_type = case_when(
    room_type %in%  names(which(table(room_type) <= 10)) ~ NA_character_,
    TRUE ~ room_type
  )) -> combinedData

combinedData %>%
  mutate(host_verifications = case_when(
    host_verifications %in%  names(which(table(host_verifications) <= 10)) ~ NA_character_,
    TRUE ~ host_verifications
  )) -> combinedData

combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]


test$zipcode[test$zipcode == "10020"] <- NA
test$zipcode[test$zipcode == "11581"] <- NA
test$property_type[test$property_type == "Castle"] <- NA
test$property_type[test$property_type == "Dome house"] <- NA
head(train)
mod1 <- lm(price~zipcode + bedrooms + bathrooms + accommodates   + square_feet + review_scores_rating + property_type + room_type + monthly_price + instant_bookable, data = train)
summary(mod1)
pred = predict(mod1,newdata=test)

submissionFile = data.frame(id = test$id, price = pred)
submissionFile$price[is.na(submissionFile$price)] <- median(submissionFile$price, na.rm=TRUE)

write.csv(submissionFile, '11-5.csv',row.names = F)  #score of 67.29
names(test)




mod2 <- lm(price~zipcode + bedrooms + bathrooms + accommodates +   + square_feet + review_scores_rating + property_type + room_type + monthly_price + instant_bookable, data = train)


scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)


#select numeric variables for imputation models
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])


library(caret)
zero_var_table <- nearZeroVar(combinedData, saveMetrics= TRUE)
combinedData <- combinedData[, !zero_var_table$nzv]

names(combinedData)

test$zipcode[test$zipcode == "10020"] <- NA
test$zipcode[test$zipcode == "11581"] <- NA
test$neighbourhood_cleansed[test$neighbourhood_cleansed == "Breezy Point"] <- NA
test$neighbourhood_cleansed[test$neighbourhood_cleansed == "Lighthouse Hill"] <- NA
test$property_type[test$property_type == "Castle"] <- NA
test$property_type[test$property_type == "Dome house"] <- NA
## split back into training data
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]
names(train)
mod2 <- lm(price~ zipcode +  bedrooms + neighbourhood_cleansed + host_is_superhost + bathrooms + security_deposit + accommodates + maximum_nights + number_of_reviews +  review_scores_value + review_scores_rating + property_type + room_type + instant_bookable, data = train)
summary(mod2)
pred = predict(mod2,newdata=test)
submissionFile = data.frame(id = test$id, price = pred)
submissionFile$price[is.na(submissionFile$price)] <- median(submissionFile$price, na.rm=TRUE)

write.csv(submissionFile, '11-5par1.csv',row.names = F)   # 109

str(lassodata)
####
lassodata <- total %>%
  select(id, price, property_type, cleaning_fee, review_scores_rating, review_scores_location, room_type, guests_included, number_of_reviews, zipcode, bathrooms, bedrooms, minimum_nights, beds, security_deposit, instant_bookable, neighbourhood)

lassodata <- train %>% select(price, zipcode, bedrooms, bathrooms, property_type,  accommodates , review_scores_rating , room_type)
library(glmnet)
x = model.matrix(price~.-1, data = lassodata)
y = train$price
ridgeModel = glmnet(x,y,alpha=0, standardize = TRUE)

cv.ridge = cv.glmnet(x,y,alpha=0)

lassoModel = glmnet(x,y, alpha=1,standardize = TRUE)
set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1)
min(cv.lasso$cvm)


cbind(LASSO=coef(cv.lasso)[,1], ridge=coef(cv.ridge)[,1])


##

scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)
combindedData$zipcode <- substr(combindedData$zipcode, 1, 5)
combindedData$zipcode[nchar(combindedData$zipcode)<5] <- NA #filter out zipcodes less than five
combindedData$zipcode <- as.factor(combindedData$zipcode)

train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]

a <- lm(price~., data= train)


#######################
####################################################
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

table(total$zipcode)

## remove token separators
total$neighbourhood <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(total$neighbourhood))
## trim whitespace
total$neighbourhood <- stringr::str_trim(gsub("\\s+", " ", total$neighbourhood))

total %>%
  mutate(neighbourhood = case_when(
    neighbourhood %in%  names(which(table(neighbourhood) <= 5)) ~ NA_character_,
    TRUE ~ neighbourhood
  )) -> total

#remove bedrooms with greater than 8 
total$bedrooms[total$bedrooms > 7] <- NA 



sqft_99 <- quantile(total$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(total$price) ~ TRUE,
  is.na(total$square_feet) ~ TRUE,
  total$square_feet > sqft_99 ~ FALSE,
  total$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

total <- total[keep_index,]

total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total$accommodates[is.na(total$monthly_price)] <- median(total$monthly_price, na.rm=TRUE)

total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 10)) ~ NA_character_,
    TRUE ~ property_type
  )) -> total

### Set missing character types to NA
char2na <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  )
}
total %>%
  mutate_if(is.character, char2na) -> total


# ## Filter & convert character columns
# # which columns are character types
# char_ind <-  which(sapply(total, class) == "character")
# # flag variables for removal if there are too many cats or only 1 cat
# char_ind_rm <- c()
# for (i in char_ind) {
#   ntypes <- sum(table(total[,i])==1)
#   if (ntypes/nrow(total) > .3 | ntypes == 1 ) {
#     char_ind_rm <- c(char_ind_rm, i)
#   }
# }
# total <- total[,-char_ind_rm]

## collect character types and convert to factors
total <- mutate_if(total, is.character, as.factor)
library(caret)

zero_var_table <- nearZeroVar(total, saveMetrics= TRUE)
total <- total[, !zero_var_table$nzv]

mod1 <- lm(price~zipcode + bedrooms + bathrooms + accommodates   + square_feet + review_scores_rating + property_type + room_type + monthly_price + instant_bookable, data = train)

names(total)

#select the variables we are going to take a closer look at 
subtotal <- total %>%
  select(id, price, property_type, cleaning_fee, review_scores_rating, review_scores_location, room_type, guests_included, number_of_reviews, zipcode, bathrooms, bedrooms, minimum_nights, beds, security_deposit, instant_bookable, neighbourhood)

train <- subtotal[!is.na(subtotal$price),]
test <- subtotal[is.na(subtotal$price),]
#lasso

library(glmnet)
complete.cases(train)
set.seed(1031)
x = model.matrix(price~.-1, data = train) # the -1 eliminates the intercept

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

table(test$price)



# help ?
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

total <- bind_rows(analysis, scoringData)

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]
l <- lm(price ~zipcode , data = train)
summary(l)

pred = predict(l, newdata = test)


## remove token separators
total$neighbourhood <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(total$neighbourhood))
## trim whitespace
total$neighbourhood <- stringr::str_trim(gsub("\\s+", " ", total$neighbourhood))

total %>%
  mutate(neighbourhood = case_when(
    neighbourhood %in%  names(which(table(neighbourhood) <= 5)) ~ NA_character_,
    TRUE ~ neighbourhood
  )) -> total

#remove bedrooms with greater than 8 
total$bedrooms[total$bedrooms > 7] <- NA 

names(combindedData) 


sqft_99 <- quantile(total$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(total$price) ~ TRUE,
  is.na(total$square_feet) ~ TRUE,
  total$square_feet > sqft_99 ~ FALSE,
  total$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

total <- total[keep_index,]

total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total$accommodates[is.na(total$monthly_price)] <- median(total$monthly_price, na.rm=TRUE)

total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 10)) ~ NA_character_,
    TRUE ~ property_type
  )) -> total

### Set missing character types to NA
char2na <- function(x) {
  case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  )
}
total %>%
  mutate_if(is.character, char2na) -> total


# ## Filter & convert character columns
# # which columns are character types
# char_ind <-  which(sapply(total, class) == "character")
# # flag variables for removal if there are too many cats or only 1 cat
# char_ind_rm <- c()
# for (i in char_ind) {
#   ntypes <- sum(table(total[,i])==1)
#   if (ntypes/nrow(total) > .3 | ntypes == 1 ) {
#     char_ind_rm <- c(char_ind_rm, i)
#   }
# }
# total <- total[,-char_ind_rm]

## collect character types and convert to factors
total <- mutate_if(total, is.character, as.factor)

subtotal <- total %>%
  select(id, price, property_type, cleaning_fee, review_scores_rating, review_scores_location, room_type,  number_of_reviews, zipcode, bathrooms, bedrooms, minimum_nights, beds, security_deposit, instant_bookable, neighbourhood)
names(subtotal)
train <- subtotal[!is.na(subtotal$price),]
test <- subtotal[is.na(subtotal$price),]

total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)

table(total$zipcode)

## remove token separators
total$neighbourhood <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(total$neighbourhood))
## trim whitespace
total$neighbourhood <- stringr::str_trim(gsub("\\s+", " ", total$neighbourhood))

total %>%
  mutate(neighbourhood = case_when(
    neighbourhood %in%  names(which(table(neighbourhood) <= 5)) ~ NA_character_,
    TRUE ~ neighbourhood
  )) -> total

total$square_feet[is.na(total$square_feet)] <- median(total$square_feet, na.rm=TRUE)
total$accommodates[is.na(total$accommodates)] <- median(total$accommodates, na.rm=TRUE)
total$accommodates[is.na(total$review_scores_rating)] <- median(total$review_scores_rating, na.rm=TRUE)
total$accommodates[is.na(total$monthly_price)] <- median(total$monthly_price, na.rm=TRUE)

total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 10)) ~ NA_character_,
    TRUE ~ property_type
  )) -> total
test$property_type[test$property_type == "Castle"] <- NA
test$property_type[test$property_type == "Dome house"] <- NA
test$property_type[test$property_type == "Houseboat"] <- NA
test$property_type[test$property_type == "Tent"] <- NA
test$room_type[test$room_type == "Hotel room"] <- NA
test$zipcode[test$zipcode == "11362"] <- NA
test$zipcode[test$zipcode == "11427"] <- NA
test$zipcode[test$zipcode == "11020"] <- NA
test$zipcode[test$zipcode == "11363"] <- NA

test$zipcode[test$zipcode == "10020"] <- NA
test$zipcode[test$zipcode == "11581"] <- NA
test$neighbourhood[test$neighbourhood == "Annadale"] <-NA
test$neighbourhood[test$neighbourhood == "Bull's Head"] <-NA
test$neighbourhood[test$neighbourhood == "Dongan Hills"] <-NA
test$neighbourhood[test$neighbourhood == "Lighthouse HIll"] <-NA
test$neighbourhood[test$neighbourhood == "Woodlawn"] <-NA
test$neighbourhood[test$neighbourhood == "Oakwood"] <-NA
test$neighbourhood[test$neighbourhood == "Bay Terrace"] <-NA

#remove bedrooms with greater than 8 
total$bedrooms[total$bedrooms > 7] <- NA 
mod1 <- lm(price~., data = train)
summary(mod1)
library(car)
vif(mod1)
names(train)
pred = predict(mod1,newdata=test)
str(train)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

str(train)

mod2 <- lm(price~ property_type + cleaning_fee + review_scores_rating  + room_type  + zipcode + bathrooms + bedrooms + minimum_nights + beds  + neighbourhood, data = train )
summary(mod2)
vif(mod2)
pred = predict(mod2,newdata=test)


mod1

names(train)



mod3 <- lm(price ~property_type + zipcode + room_type + bathrooms + bedrooms + beds + neighbourhood, data = train)
summary(mod3)
pred = predict(mod3, newdata = test)








#########


####################################################
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)


combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA_character_
combinedData$zipcode <- as.factor(combinedData$zipcode)


sqft_99 <- quantile(combinedData$square_feet, .99, na.rm=TRUE)
case_when(
  is.na(combinedData$price) ~ TRUE,
  is.na(combinedData$square_feet) ~ TRUE,
  combinedData$square_feet > sqft_99 ~ FALSE,
  combinedData$square_feet==0 ~ FALSE,
  TRUE ~ TRUE) -> keep_index

combinedData <- combinedData[keep_index,]



### Imputation ###
# impute missing values

# median replacement: by hand
combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)



## CITY ##
## remove token separators
combinedData$neighbourhood <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$neighbourhood))
## trim whitespace
combinedData$neighbourhood  <- stringr::str_trim(gsub("\\s+", " ", combinedData$neighbourhood ))

combinedData %>%
  mutate(room_type = case_when(
    room_type %in%  names(which(table(room_type) <= 10)) ~ NA_character_,
    TRUE ~ room_type
  )) -> combinedData

combinedData %>%
  mutate(host_verifications = case_when(
    host_verifications %in%  names(which(table(host_verifications) <= 10)) ~ NA_character_,
    TRUE ~ host_verifications
  )) -> combinedData

combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)

combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]


test$zipcode[test$zipcode == "10020"] <- NA
test$zipcode[test$zipcode == "11581"] <- NA
test$property_type[test$property_type == "Castle"] <- NA
test$property_type[test$property_type == "Dome house"] <- NA
names(train)
mod1 <- lm(price~zipcode + bedrooms + bathrooms + accommodates + reviews_per_month  + square_feet + review_scores_rating + property_type + room_type + monthly_price + bed_type  + cleaning_fee  + beds + host_is_superhost, data = train)
summary(mod1)

pred = predict(mod1,newdata=test)

submissionFile = data.frame(id = test$id, price = pred)
submissionFile$price[is.na(submissionFile$price)] <- median(submissionFile$price, na.rm=TRUE)
submissionFile$price[submissionFile$price <0 ] <- median(submissionFile$price, na.rm = TRUE)
write.csv(submissionFile, '11-9.csv',row.names = F)  #score of 66.21969
names(test)

alias(mod1)
vif(mod1)

alias(mod1)

names(train)

str(train)
mod2 <- lm(price~zipcode + bedrooms + bathrooms + accommodates + host_acceptance_rate +  reviews_per_month  + square_feet + review_scores_rating + property_type + room_type + monthly_price + bed_type  + cleaning_fee  + beds  + weekly_price + minimum_nights + license + number_of_reviews , data = train)
summary(mod2)  ## 0.6007
pred = predict(mod2,newdata=test)
submissionFile = data.frame(id = test$id, price = pred)
submissionFile$price[is.na(submissionFile$price)] <- median(submissionFile$price, na.rm=TRUE)
submissionFile$price[submissionFile$price <0 ] <- median(submissionFile$price, na.rm = TRUE)
write.csv(submissionFile, '11-9(2).csv',row.names = F)  #score of 65.99

alias(mod2)


library(tidyverse)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)
names(train)
tr <- analysis %>% 
  select(id, price, bedrooms, bathrooms, accommodates, reviews_per_month, review_scores_accuracy, bed_type)
str(tr)
library(glmnet)
library(glmnet)

set.seed(1031)
x <- model.matrix(price~., tr) 
y = tr$price

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

library(corrplot)


###


library(tidyverse)
scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")
str(analysis$zipcode)
str(scoringData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA

combinedData <- bind_rows(analysis, scoringData)

table(combinedData$zipcode)
### ZIP CODES ###
## modify zip+4 codes
combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
## set missing / malformed zipcodes to NA
combinedData$zipcode[nchar(combinedData$zipcode)<5] <- NA_character_


combinedData %>%
  mutate(zipcode = case_when(
    zipcode %in%  names(which(table(zipcode) <= 5)) ~ NA_character_,
    TRUE ~ zipcode
  )) -> combinedData

combinedData$zipcode <- as.factor(combinedData$zipcode)


train <- combinedData[!is.na(combinedData$price),] 
test <- combinedData[is.na(combinedData$price),]

table(train$zipcode)

modd <- lm(price ~ bathrooms + bedrooms + square_feet + reviews_per_month + zipcode + guests_included + review_scores_rating + number_of_reviews + room_type + beds + property_type, train)
summary(modd)
library(car)
alias(modd)
vif(modd)
test$zipcode[test$zipcode == "10004"] <- NA
test$zipcode[test$zipcode == "10007"] <- NA
test$zipcode[test$zipcode == "10020"] <- NA
test$zipcode[test$zipcode == "10038"] <- NA
test$zipcode[test$zipcode == "10039"] <- NA
test$zipcode[test$zipcode == "10044"] <- NA
test$zipcode[test$zipcode == "10065"] <- NA
test$zipcode[test$zipcode == "10069"] <- NA
test$zipcode[test$zipcode == "10129"] <- NA
test$zipcode[test$zipcode == "10270"] <- NA
test$zipcode[test$zipcode == "10280"] <- NA
test$zipcode[test$zipcode == "10282"] <- NA
test$zipcode[test$zipcode == "10302"] <- NA
test$zipcode[test$zipcode == "10303"] <- NA
test$zipcode[test$zipcode == "10306"] <- NA
test$zipcode[test$zipcode == "10307"] <- NA
test$zipcode[test$zipcode == "10308"] <- NA
test$zipcode[test$zipcode == "10309"] <- NA
test$zipcode[test$zipcode == "10310"] <- NA
test$zipcode[test$zipcode == "10312"] <- NA
test$zipcode[test$zipcode == "10314"] <- NA
test$zipcode[test$zipcode == "10451"] <- NA
test$zipcode[test$zipcode == "10453"] <- NA
test$zipcode[test$zipcode == "10454"] <- NA
test$zipcode[test$zipcode == "10455"] <- NA
test$zipcode[test$zipcode == "10456"] <- NA
test$zipcode[test$zipcode == "10457"] <- NA
test$zipcode[test$zipcode == "10458"] <- NA
test$zipcode[test$zipcode == "10459"] <- NA
test$zipcode[test$zipcode == "10460"] <- NA
test$zipcode[test$zipcode == "10461"] <- NA
test$zipcode[test$zipcode == "10462"] <- NA
test$zipcode[test$zipcode == "10463"] <- NA
test$zipcode[test$zipcode == "10465"] <- NA
test$zipcode[test$zipcode == "10466"] <- NA
test$zipcode[test$zipcode == "10467"] <- NA
test$zipcode[test$zipcode == "10469"] <- NA
test$zipcode[test$zipcode == "10470"] <- NA
test$zipcode[test$zipcode == "10471"] <- NA
test$zipcode[test$zipcode == "10472"] <- NA
test$zipcode[test$zipcode == "10473"] <- NA
test$zipcode[test$zipcode == "10474"] <- NA
test$zipcode[test$zipcode == "10475"] <- NA
test$zipcode[test$zipcode == "10705"] <- NA
test$zipcode[test$zipcode == "11020"] <- NA
test$zipcode[test$zipcode == "11103"] <- NA
test$zipcode[test$zipcode == "11109"] <- NA
test$zipcode[test$zipcode == "11203"] <- NA
test$zipcode[test$zipcode == "11204"] <- NA
test$zipcode[test$zipcode == "11208"] <- NA
test$zipcode[test$zipcode == "11209"] <- NA
test$zipcode[test$zipcode == "11210"] <- NA
test$zipcode[test$zipcode == "11212"] <- NA
test$zipcode[test$zipcode == "11214"] <- NA
test$zipcode[test$zipcode == "11219"] <- NA
test$zipcode[test$zipcode == "11223"] <- NA
test$zipcode[test$zipcode == "11224"] <- NA
test$zipcode[test$zipcode == "11228"] <- NA
test$zipcode[test$zipcode == "11234"] <- NA
test$zipcode[test$zipcode == "11236"] <- NA
test$zipcode[test$zipcode == "11354"] <- NA
test$zipcode[test$zipcode == "11356"] <- NA
test$zipcode[test$zipcode == "11357"] <- NA
test$zipcode[test$zipcode == "11360"] <- NA
test$zipcode[test$zipcode == "11361"] <- NA
test$zipcode[test$zipcode == "11362"] <- NA
test$zipcode[test$zipcode == "11363"] <- NA
test$zipcode[test$zipcode == "11364"] <- NA
test$zipcode[test$zipcode == "11365"] <- NA
test$zipcode[test$zipcode == "11366"] <- NA
test$zipcode[test$zipcode == "11367"] <- NA
test$zipcode[test$zipcode == "11368"] <- NA
test$zipcode[test$zipcode == "11369"] <- NA
test$zipcode[test$zipcode == "11370"] <- NA
test$zipcode[test$zipcode == "11372"] <- NA
test$zipcode[test$zipcode == "11373"] <- NA
test$zipcode[test$zipcode == "11374"] <- NA
test$zipcode[test$zipcode == "11375"] <- NA
test$zipcode[test$zipcode == "11378"] <- NA
test$zipcode[test$zipcode == "11411"] <- NA
test$zipcode[test$zipcode == "11412"] <- NA
test$zipcode[test$zipcode == "11413"] <- NA
test$zipcode[test$zipcode == "11414"] <- NA
test$zipcode[test$zipcode == "11415"] <- NA
test$zipcode[test$zipcode == "11416"] <- NA
test$zipcode[test$zipcode == "11417"] <- NA
test$zipcode[test$zipcode == "11418"] <- NA
test$zipcode[test$zipcode == "11419"] <- NA
test$zipcode[test$zipcode == "11420"] <- NA
test$zipcode[test$zipcode == "11422"] <- NA
test$zipcode[test$zipcode == "11426"] <- NA
test$zipcode[test$zipcode == "11427"] <- NA
test$zipcode[test$zipcode == "11428"] <- NA
test$zipcode[test$zipcode == "11429"] <- NA
test$zipcode[test$zipcode == "11432"] <- NA
test$zipcode[test$zipcode == "11433"] <- NA
test$zipcode[test$zipcode == "11434"] <- NA
test$zipcode[test$zipcode == "11435"] <- NA
test$zipcode[test$zipcode == "11436"] <- NA
test$zipcode[test$zipcode == "11559"] <- NA
test$zipcode[test$zipcode == "11581"] <- NA
test$zipcode[test$zipcode == "11691"] <- NA
test$zipcode[test$zipcode == "11692"] <- NA
test$zipcode[test$zipcode == "11693"] <- NA
test$zipcode[test$zipcode == "11694"] <- NA
test$room_type[test$room_type == "Hotel room"] <- NA
test$property_type[test$property_type == "Boat"] <- NA
test$property_type[test$property_type == "Boutique hotel"] <- NA
test$property_type[test$property_type == "Bungalow"] <- NA
test$property_type[test$property_type == "Cabin"] <- NA
test$property_type[test$property_type == "Camper/RV"] <- NA
test$property_type[test$property_type == "Casa particular (Cuba)"] <- NA
test$property_type[test$property_type == "Castle"] <- NA
test$property_type[test$property_type == "Cottage"] <- NA
test$property_type[test$property_type == "Dome house"] <- NA
test$property_type[test$property_type == "Earth house"] <- NA
test$property_type[test$property_type == "Guesthouse"] <- NA
test$property_type[test$property_type == "Hostel"] <- NA
test$property_type[test$property_type == "Hotel"] <- NA
test$property_type[test$property_type == "Houseboat"] <- NA
test$property_type[test$property_type == "Other"] <- NA
test$property_type[test$property_type == "Resort"] <- NA
test$property_type[test$property_type == "Serviced apartment"] <- NA
test$property_type[test$property_type == "Tent"] <- NA
test$property_type[test$property_type == "Tiny house"] <- NA
test$property_type[test$property_type == "Villa"] <- NA
test$property_type[test$property_type == "Aparthotel"] <- NA

pred = predict(modd,newdata=test)

# Construct submission from predictions
submissionFile = data.frame(id = test$id, price = pred)






## select numeric variables for imputation models
numeric_predictors <- which(colnames(combinedData) != "price" & sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])



## split back into training data
train <- combinedData[!is.na(combinedData$price),]
test <- combinedData[is.na(combinedData$price),]




moda <- lm(price ~ square_feet  + zipcode + guests_included + review_scores_rating + room_type, data = train)
summary(moda)

pred = predict(moda, newdata = test)


names(analysis)
head(analysis)

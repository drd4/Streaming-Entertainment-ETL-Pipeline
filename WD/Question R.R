scoringData = read.csv('scoringData.csv')
analysis <- read.csv("analysisData.csv")


scoringData$zipcode <- as.character(scoringData$zipcode)
scoringData$price <- NA
# combind data
total <- bind_rows(analysis, scoringData)

#clean up zip
total$zipcode <- substr(total$zipcode, 1, 5)
total$zipcode[nchar(total$zipcode)<5] <- NA_character_
total$zipcode <- as.factor(total$zipcode)
total$zipcode <- forcats::fct_lump_n(total$zipcode, 40) # combine factor levels for zipcodes less than 40

#clean up bedrooms and bathrooms
total$bedrooms[is.na(total$bedrooms)] <- 1
total$bathrooms[is.na(total$bathrooms)] <- 1

#create amenities variables
total$amenities = gsub("\\.", "", total$amenities)  ## remove the dot sign 
total$amenities = total$amenities %>% stringr::str_replace_all("\\s", "")   ## remove the space
total$amenities = noquote(total$amenities)   ##  remove quotation sign

total = cbind(total,mtabulate(strsplit(as.character(total$amenities), ',')))

# remove properties with less than 5 examples #
total %>%
  mutate(property_type = case_when(
    property_type %in%  names(which(table(property_type) <= 5)) ~ "Other",
    TRUE ~ property_type
  )) -> total

#split data 
train <- total[!is.na(total$price),]
test <- total[is.na(total$price),]


#This model, without including square feet, does not produce an error of 'property type has new factor levels'

#This model, without including square feet, does not produce an error on predict

nosqft <- lm(price ~ zipcode + bedrooms  + Pool + bathrooms + property_type + cleaning_fee + Shampoo + review_scores_rating + accommodates + TV + Gym  + Dryer +  Elevator + minimum_nights + Airconditioning  + Essentials + Petsallowed + `Selfcheck-in` + `Family/kidfriendly` + Breakfast, train)
pred = predict(nosqft, newdata = test)

## BUT, when I include square_feet, the model produces the error 'property type has new factor levels' when using predict
sqft <- lm(price ~ zipcode + bedrooms  + square_feet + Pool + bathrooms + property_type + cleaning_fee + Shampoo + review_scores_rating + accommodates + TV + Gym  + Dryer +  Elevator + minimum_nights + Airconditioning  + Essentials + Petsallowed + `Selfcheck-in` + `Family/kidfriendly` + Breakfast, train)
summary(sqft)
pred1 = predict(sqft, newdata = test)

#Yet, when I use, there are no missing levels 
table(test$property_type)
table(train$property_type)


## I also thought there might be a problem with square_feet having a ton of NA values, but even after replacing the NA's with the median to have a complete dataset, still having the same error
total$square_feet[is.na(total$square_feet)] <- mean(total$square_feet, na.rm=TRUE)









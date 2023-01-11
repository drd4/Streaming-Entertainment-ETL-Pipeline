### R BASICS

## @AUTHOR DANIEL DASGUPTA
## @DATE 9/15/2020

# arithmetic
45 * 34
2^100

#objects and assignments 
a = 45
b = 34
a * b

b <- 34
a * b -> c

#lets list objects
ls()
## or
objects()

pi

# variable names
my_first_variable = 2010 #this is my style
MyFirstVariable = 2010
my.first.variable = 2010

#Function
?log
log(x = 100, base = 10)
log(100,10)
log(base = 10, x = 100)
log(10, 100)
log(10)
sqrt(81)

## data types
class(a)
class(45 > 34)
class(TRUE)

# Data structures
set_of_integers = 4:9
is.vector(set_of_integers)

consecutive_integers <- 4:9

#vectors can be characters
names <- c("daz", "dino", "gupta")

# operations are element wise
vec <- 1:4
vec * vec

# matrix operations are special
vec %*% bec
sum(vec*vec)

# mixed type vectors
name_and_age <- c("dino", 24, 10, TRUE)
class(name_and_age)

# Matrix
matrix_1 <- matrix(1:8, nrow = 2, ncol = 4)
matrix_2 <- matrix(8:1, nrow = 2, ncoll =4)
matrix_3 <- matrix(1:8, byrow = TRUE)
#### the *byrow = TRUE* changes the structure of the data

## List
name_and_age = list('Dino', 25, 10, TRUE)

#data.frame
set.seed(10)
df = data.frame(id = 1:10, 
                gender = sample(c("Male", "Female"), size = 10, replace = T),
                attended=sample(c(T, F), size = 10, replace = T),
                score = sample(x = 1:100, size = 10, replace = T), stringsAsFactors = TRUE)


# NAVIGATION
getwd()

## reading in data
#read.table
read.csv()

#########

library(ggplot2)
team_salary <- read.csv(file = "team_salary.csv")

str(team_salary)
nrow(team_salary)
ncol(team_salary)

colnames(team_salary)
names(team_salary)

install.packages("tibble")
library(tibble)
team_salary_tb <- as_tibble(team_salary)

install.packages("DT")
library(DT)

#subset 
team_salary[18,2]
team_salary$Team_ID

team_salary[,c(TRUE,FALSE, TRUE, FALSE)]
team_salary[, c(1,3)]

team_salary_high_wins <- team_salary[team_salary$win_pct > 0.5 , ]

##### DESCRIPTIVE MEASURES
library(ggplot2)
head(mpg)

# default functions for stats
mean(mpg$hwy)
median(mpg$hwy)

#create function to calc mode

mode2 <- function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode2(mpg$hwy)

# Range
max(mpg$hwy) - min(mpg$hwy)
range(mpg$hwy)
diff(range(mpg$hwy))

quantile(mpg$hwy, c(.05, .5, .95))

# interquartile range
quantile(mpg$hwy, c(.75)) - quantile(mpg$hwy, c(.25))
diff(quantile(mpg$hwy, c (.25, .75)))

#variance
var(mpg$hwy)
sd(mpg$hwy)

summary(mpg$hwy)

## categorical variables
table(mpg$cyl)
prop.table(mpg$cyl)  #proportions instead of counts

prop.table(table(mpg$cyl))
xtabs(~cyl, data = mpg) #cross tab


#Metrics
tapply(mpg$hwy, INDEX=mpg$cyl, FUN = 'median')

###################################

#VISUAL SUMMARY
hist(mpg$hwy)
par(nfrow=c(3,1))
hist(mpg$hwy)
hist(mpg$hwy, breaks = 10)
hist(mpg$hwy, breaks = 100)

#scatter plot
plot(mpg$cyl, mpg$hwy)

library(ggplot2)
#histogram in ggplot2
ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram()

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram(binwidth = .1)

#compare distributions
ggplot(data = mpg, mapping = aes(x = hwy, fill = factor(year))) +
  geom_histogram()



ggplot(data = mpg, mapping = aes(x = hwy, color = factor(year))) +  #color instead of fill bc of the type of graph (bar vs line)
  geom_freqpoly(size = 1.2)

ggplot(mpg, aes(x = factor(year), y = hwy)) + 
  geom_boxplot()

table(mpg$drv)
ggplot(data = mpg, aes(x = drv)) +
  geom_bar()

## side-by-side via facets
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + facet_grid(.~cyl) ## period basically means you are looking at just that one variable 


-------------------
  #9/21 Assignment 1 section 2, data exlporation
  
day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

sum(number_of_smartwatches_sold)

sum(price_per_smartwatch * number_of_smartwatches_sold)

df > 25
number_of_smartwatches_sold > 25
sum(number_of_smartwatches_sold > 25)
mean(number_of_smartwatches_sold)

-------------------- 
  # 9/21 Assignment 1 Section 3, data exploration
  diamondsData = read.csv('diamonds_data.csv')
library(ggplot2)
(diamondsData)
mean(diamondsData$carat)
head(diamondsData)
ideal <- diamondsData[diamondsData$cut == 'Ideal' , ]
mean(ideal$carat)                
                
fair <- diamondsData[diamondsData$cut == 'Fair', ]
var(fair$carat)
good <- diamondsData[diamondsData$cut == 'Good', ]
var(good$carat)
verygood <- diamondsData[diamondsData$cut == 'Very Good', ]
var(verygood$carat)
prem <- diamondsData[diamondsData$cut == 'Premium', ]
var(prem$carat)
ideal <- diamondsData[diamondsData$cut == 'Ideal', ]
var(ideal$carat)


d <- diamondsData[diamondsData$color == 'D', ]
str(d)

ggplot(d, aes(stat = count(), x = cut)) +
  geom_histogram()
datatable(d)
ggplot(d, aes(x = cut, stat = "count")) + geom_bar()







pr <- diamondsData[diamondsData$carat > 2 ,]
mean(pr)
mean(pr$price)

ggplot(diamondsData, aes(price,cut)) +
  geom_point() + facet_grid(cut~.)

hist(diamondsData$price)

ggplot(diamondsData, aes( y = price)) +
  geom_histogram() + facet_grid(cut~.)

ggplot(diamondsData, aes(price, color = cut)) +
  geom_histogram() 


ggplot(data=diamondsData,aes(x=carat))+ 
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim=c(0,2.5))+
  scale_x_continuous(breaks=seq(0,2.5,0.1))



##############################################################9/22 
  #Mod 3
  # A basic analysis task:
  # get the mean of a continous variabvle within each"group" of an ordinal variable
## input => data.frame of input data
## output => a named vector with group means

data(mtcars)
head(mtcars)

# Loop (Base R)
cylinder_cats <- sort(unique(mtcars$cyl))
mean_mpg <- vector('numeric', length(cylinder_cats))
for (i in 1:length(cylinder_cats)) {
  print(i)
}

for (i in 1:length(cylinder_cats)) {
 ind <- mtcars$cyl == cylinder_cats[i]
mean_mpg[i] <- mean(mtcars$mpg[ind])
 }

names(mean_mpg) <- cylinder_cats
mean_mpg

# apply functions (tapply, lapply, sapply, apply....)
mean_mpg2 <- tapply(mtcars$mpg, INDEX = mtcars$cyl, FUN = mean)
mean_mpg2

### dyplr
install.packages("dyplr")
library(dplyr)

mtcars_by_cyl <- group_by(mycars, cyl)
mean_mpg_df <- summarize(mtcars_by_cyl, mean_mpg=mean(mpg))
mean_mpg3 <- mean_mpg_df$mean_mpg
names(mean_mpg_3) <- mean_mpg_df$cyl

# dplyr (pipes)
mtcars %>%  mt
 ## NEEEDD TO INSERT MISSSING CODE


## pro/cons of each approach


## Beginning of Slides
## Data parsing
## @data 9/22

## Variable types
# Number
a <- "$25" #character

as.numeric(a) #R doesnt know format of string bc of $

substring(a, 2, 3) # this strips out the dollar sign and is now numeric 
b <- as.numeric(substring(a,2,3))

install.packages("readr")
library(readr)

a <- parse_number('$25')
b <- parse_number(' $25') ## only detects first number

parse_number('increase in sales of $123,456,78', locale = locale(grouping_mark = '.'))


#Dates 
#ISO8601 #type
independence <- 'July 4, 1776'
class(independence)
independence_day <- as.Date(independence, format = '%b %d, %Y')
class(independence_day)

install.packages("lubridate")
library(lubridate)

mdy('7/4/1776')
dmy('4/7/1776')

mdy_hms("July 4, 1776, 3:23:13 pm", tz = 'US/Eastern')


location = c('USA, north america','China,asia','India, asia', 'Russia, asia','France, europe','UK, europe')
national_day = c('July 4, 1776','October 1, 1949','August 15, 1947','June 12, 1990','July 14, 1789','none')
national_day_alt = c('4/7/1776','1/10/1949','15/8/1947','12/6/1990','14/7/1789','none')
per_capita_dollars = c('$65,280.70','$10,261.70','$2,104.10','$11,585.00','$40,493.90','$42,300.30')
per_capita_euros = c('55.488,59 Euro','8.722,44 Euro','1.788,48 Euro','9.847,25 Euro','34.419,82 Euro','35.955,26 Euro')

messy_data = data.frame(location, national_day, national_day_alt, per_capita_dollars,per_capita_euros)
 ## Clean up the dataset

library(tidyverse)

messy_data %>%
  mutate(per_capita_dollars = parse_number(per_capita_dollars)) %>%
  mutate(per_capita_euros = parse_number(per_capita_euros, locale = locale(grouping_mark = '.'))) %>%
  mutate(national_day = mdy(national_day)) %>%
  mutate(national_day_alt = dmy(national_day_alt))
#something here isnt right

#### Data Transformations


# @date 9/20/2020


# A basic analysis task:
# get the mean of a continuous variable within each "group" of an ordinal variable
## input  => data.frame of input data
## output => a named vector with group means

data(mtcars)
head(mtcars)

# Loop
# for each category (cyl), compute the mean (mpg) on the subset
cylinder_cats <- sort(unique(mtcars$cyl))
mean_mpg <- vector('numeric', length(cylinder_cats)) # initialize an empty vector
for (i in 1:length(cylinder_cats)) {
  ind <- which(mtcars$cyl == cylinder_cats[i])   ## identify elements for each category
  mean_mpg[i] <- mean(mtcars$mpg[ind])           ## compute / store the mean
}
names(mean_mpg) <- cylinder_cats


## apply functions (tapply, lapply, sapply, apply, ...)
# simple! uses a single function that uses a secret loop
mean_mpg_2 <- tapply(mtcars$mpg, INDEX = mtcars$cyl, FUN = mean)


## dplyr (no pipes)
library(dplyr)
mtcars_by_cyl <- group_by(mtcars, cyl)
mean_mpg_df <- summarize(mtcars_by_cyl, mean_mpg=mean(mpg))
mean_mpg_3 <- mean_mpg_df$mean_mpg
names(mean_mpg_3) <- mean_mpg_df$cyl


## dplyr (end-to-end pipes)
## can use curly brace "expressions" and variable stand-in dot to use non-dplyr functions in a pipe sequence
mtcars %>%
  group_by(cyl) %>%
  summarize(mean_mpg=mean(mpg)) %>%
  {setNames(object = .$mean_mpg, nm = .$cyl)} -> mean_mpg_4


all(mean_mpg == mean_mpg_2)
all(mean_mpg == mean_mpg_3)
all(mean_mpg == mean_mpg_4)


## pro/cons of each approach
# loops:
##   pro: complete control
##   con: must manage more code

# apply:
#    pro: simplify code
#    con: function arguments and output format are not up to the coder

# dplyr (tidyverse):
#    pro: descriptive function names, fewer intermediate variables => clean code
#    con: many more functions (dplyr has ~260 functions!)
#         pipes are hard to debug
#         can be harder to generalize code


####### Assignment 2 Data Tidying
birthdays = read.csv('president_birthdays.csv')
heights = read.csv('president_heights.csv')
states = read.csv('president_states.csv')

library(tidyverse)


######################################### 9/29  Modeling Frameworks (HTML)

wages <- read.csv("wages.csv", stringsAsFactors = TRUE) #character types in data used to be factors by default and now they are strings

class(wages$gender)
levels(wages$gender)

library(ggplot2)
head(diamonds)
#Random Sampling 

#simple random sampling 
#split into training set, and test test

?sample
set.seed(61710) #to create resuable data
split <- sample(x= 1:nrow(diamonds), size = 0.7*nrow(diamonds))

length(split)/nrow(diamonds)  #to make sure the number of rows is right 

train <- diamonds[split,]
test <- diamonds[-split,]

#evaluate representativeness 
# lets look at price, something we might consider as the repsonse variable 
mean(train$price)
mean(test$price)

## go learn your model

# Stratified random samplig (with a numeric outcome)
install.packages("caret")
library(caret) #ML packages

set.seed(61710)
split <- createDataPartition(y = diamonds$price, p = .7, list = FALSE, groups = 50)

train = diamonds[split,]
test = diamonds[-split,]

## what does the stratified split look like? 

mean(train$price)
mean(test$price)

# stratified sampling with categorical outcome
diamonds$price_hilo <- ifelse(diamonds$price>mean(diamonds$price), 'High', 'Low')
table(diamonds$price_hilo) #shows ditribution of the categories

set.seed(61710)
split <- createDataPartition(y = diamonds$price_hilo, p = 0.7, list = FALSE)
train = diamonds[split,]
test = diamonds[-split,]

table(train$price_hilo)
table(test$price_hilo)

propmat <- rbind(train = table(train$price_hilo),
      test = table(test$price_hilo))
prop.table(propmat, margin = 1)

## stratified random sampling caTOOLs
install.packages("caTools")
library(caTools)

set.sed(61710)
split <- sample.split(Y = diamonds$price_hilo, SplitRatio = 0.8)

train <- diamonds[split,]
test <- diamonds[!split,]

dim(train)
dim(test)


#### Linear Regression HTML FIlE
wages <- read.csv("wages.csv", stringsAsFactors =  TRUE)

# clean data
library(ggplot2)
ggplot(data = wages, aes(x = earn)) +
geom_histogram(binwidth = 5000, fill = 'cadetblue')

sum(wages$earn < 0)
wages <- wages[wages$earn >= 0,]

library(dplyr)
wages <- wages %>% filter(earn >= 0)

## data splitting 
library(caret)
set.seed(1031)
split <- createDataPartition(y = wages$earn, p = .7, list = FALSE, group = 20)

train <- wages[split, ]
test <- wages[-split,]

str(train)


# Example Outliers
ggplot(train, aes(x = '', y = earn)) + 
  geom_boxplot(outlier.color = 'red' , fill = 'cadetblue')

#Simple regression model
ggplot(train, aes(x = age, y = earn)) + 
  geom_point() +
  geom_smooth(method = 'lm', size = 1.3, color = 'steelblue3') +
  coord_cartesian(ylim = c(0,200000))

# is there a linear relationship ebtween these variables
cor(train$age, train$earn)

# estimate
model1 <- lm(earn ~ age, data = train)
summary(model1)
coef(model1)

#prediction
anova(model1)

#compare predicted earning from actual earnigns
pred <- predict(model1)
head(pred)

data.frame(earn = train$earn[100:109], prediction = pred[100:109])
plot(train$earn, pred)

#sum of squared errors
SSE <- (pred - train$earn)^2  
SST <- sum((mean(train$earn)  - train$earn)^2)

model_1r2 <- 1 - SSE/SST

rmse1 <- sqrt(mean((pred - train$earn)^2))#diff from predicted - earning square



#####################################################################################################################

### Modeling Framework


# 
# The dataset may be split into a train and test sample using random approaches or non-random approaches. The latter are used in very specific situations, hence are not discussed here. For more on non-random approaches see, Chapter 4, p.67 in Applied Predictive Modeling, by Max Kuhn and Kjell Johnson. There are two approaches to random sampling:
#   
#   Simple Random Sampling: Each observation has an equal likelihood of getting picked.
# Stratified Sampling: Simple random sampling is done on subgroups or strata. In predictive modeling situations, stratified sampling is used to ensure similar distribution of the outcome variable in train and test samples.
# Data
# 
# To demonstrate these two approaches to splitting the sample, we will use the diamonds dataset that ships with ggplot2. We will split this data into a train and test set in the ratio, 70:30, so that 70% of the sample will go to the train sample.
# 

library(ggplot2)
head(diamonds)

# Simple Random Sampling
# 
# Set the seed to 61710. Choice of seed is arbitrary but is very important as it ensures the random split can be replicated. R has a memory of one line for the set.seed() function, so seed must be set right before the step that runs random sampling function.
# 
# Before, we actually split the data, we generate a random sample of row indices from a vector containing all the rows in diamonds 1:nrow(diamonds). We get 70% of the row indices by specifying a size that is 70% of the number of rows in diamonds: 0.7*nrow(diamonds). Take the individual pieces apart to convince yourself of the underlying process. Also, note by default sample() does sampling without replacement which is what we want.
# 

set.seed(61710)
split = sample(x = 1:nrow(diamonds),size = 0.7*nrow(diamonds))
split[1:10]


length(1:nrow(diamonds))

length(split)

#Next, we subset the rows of diamonds that have an index matching in split.
train = diamonds[split,]


#Next, we use a clever R trick to create a test sample by not including the rows in split. 
#Here, -split will not include any row indices that are contained in split.

test = diamonds[-split,]

#We can confirm it all worked well by checking the number of rows in the train and test sets.
#They must sum to the number of rows in the original dataset, diamonds.

nrow(train)

nrow(test)

nrow(diamonds)
#Lastly, we can check to see if the variables in the two samples are similar.
#Random sampling is going to generate similar samples but they are unlikely to be identical. So, let’s check

mean(train$price); mean(test$price)

#Stratfied Random sampling (with numeric outcome )

# When using data for prediction, it is important that the train and test samples be similar but even more important for the outcome variable to have a similar distribution. For this reason, it is common to sample in such a manner that the outcome variable is approximately equal across train and test samples.
# We will make use of createDataPartition() from the caret package.
# 
# As was the case above, we set a seed and run it immediately before the sampling function. 
# Assuming the goal here is to predict price, we setup the sample so that price is kept approximately equal across samples. 
# Since a numeric variable doesn’t have natural strata, it is divided into groups based on percentiles. Here, we are using groups=50. We are keeping 70% observations in the train sample.
# 

library(caret)
set.seed(61710)
split = createDataPartition(y = diamonds$price, p = 0.7, list = F, groups = 50)

#Once we get the split vector, we proceed in the same manner as for random sampling

train = diamonds[split,]
test = diamonds[-split,]

#Verify that the split divided up the diamonds in approximately 70:30 ratio.
nrow(train)
nrow(test)
nrow(diamonds)
#Compare the outcome variable, price, across the samples.  how does this compare to simple random sampling?
mean(train$price);
mean(test$price)

# Stratified random sampling with categorial outcome 
# A categorical outcome (e.g., email response) has strata that are defined by the levels of the variable 
# (e.g., for email response: respond and not respond). 
# In this case, simple random samples will be drawn from each strata and then combined.
# 
# First, let’s create a categorical outcome from the diamonds dataset. The new variable, price_hilo has two levels, high and low.

diamonds$price_hilo = ifelse(diamonds$price>mean(diamonds$price), "High", "Low")
head(diamonds)

#We are now, going to conduct stratified random sampling to ensure the breakdown of high and low in price_hilo is the same across train and test samples. 
#Set seed and run createDataPartition with price_hilo.

set.seed(61710)
split = createDataPartition(y = diamonds$price_hilo, p = 0.7, list = F)

#Create train and test samples using split
train = diamonds[split,]
test = diamonds[-split,]
nrow(train)
nrow(test)
nrow(diamonds)
#Since the outcome is categorical, we look at counts rather than average
table(train$price_hilo)

table(test$price_hilo)


# to compare, proporition of high and low prices in each sample, we examine proportions

prop.table(rbind(train = table(train$price_hilo),
                test = table(test$price_hilo)),
           margin = 1)

#Stratified random sampling with categorical outcome using caTOOLs

library(caTools)
set.seed(61710)
split = sample.split(Y = diamonds$price_hilo, SplitRatio = 0.7)

#the key difference of sample.split() from prevous sampling functions is that it generates a logical, not a vector of numbers
table(split)

#since sample.split() generates a logical, to subset, we will have to make a subtle but important change
# rahter than using -, we use ! operator for the test sample


train = diamonds[split,]
test = diamonds[!split,]
nrow(train)
nrow(test)

#since the outcome is a categorical variable, we look at counts rather than average
table(train$price_hilo)
table(test$price_hilo)

# to compare, proportion of high and low prices in each sample, we examine proportions
prop.table(rbind(train = table(train$price_hilo), 
                 test = table(test$price_hilo)),
           margin = 1)

# to compare the results of createDataPartition() to sample.split(), you will note the results are similar but not identifcal 

# IN CONCLUSION
# For simple random sampling, use sample(). For stratified sampling with a numeric outcome variable, use caret::createDataPartition. 
# For stratified sampling with a categorical outcome, use either caret::createDataPartition() or caTools::sample.split().
















#6 and #10
birthdays = read.csv('president_birthdays.csv')
heights = read.csv('president_heights.csv')
states = read.csv('president_states.csv')

model = paste('model',1:10,sep = '')
r2 = c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp = c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss = c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results = data.frame(model, r2, cp, rss)

library(tidyverse)

# question 9 
heights = read.csv('president_heights.csv')
heights$height <- as.numeric(gsub("inches", "", heights$height))  #this removes inches


heightsstates <- heights %>%
  inner_join(states, by = ("Name"))
filter(heightsstates, Birth.State == "Virginia")
a<- heightsstates %>%
  filter(Birth.State == "Virginia")
mean(a$height)

heights1 <- as.data.frame(scale(heights$height))
mean(heights1)

heights_scaled <- apply(X = heights$height, MARGIN = 2, FUN = function(x) round(scale(x), 2))

install.packages("robustHD")
library(robustHD)
a <- standardize(heights$height)
mean(a)
sd(a)

##### THIS IS HOW TO SOLVE matching the variables
qw <- scale(heights$height, center= TRUE, scale = TRUE)
qw_c <- data.frame(id = heights$Name, qw)

team<- read.csv("team_salary.csv")

tt <- scale(team$Average_Salary,center = TRUE, scale = TRUE)
tt_c <- data.frame(id= team$Team_Name, tt)

ggplot(tt, aes(tt$Team_Name, tt$Average_Salary)) +
  geom_bar()
str(tt)
str(heights)
as.numeric(tt$A)
result = apply(heights[,1:2],
               MARGIN = 2,
               FUN = function(x)c(round(mean(x),2),round(var(x),2)))

str(heights)
coupons_scaled = apply(X = heights[heights$height,],
                       MARGIN = 2,
                       FUN = function(x) round(scale(x),2))
coupons_scaled = data.frame(id = heights$Name,coupons_scaled)
head(coupons_scaled)


a <- scale(heights$height)
mean(a)
sd(a)

aa <- heights %>% mutate_at(c("height"), ~ (scale(.) as.vector))

a <- scale(heights$height)
gsub(" ", "", x, fixed = TRUE) 

states1<- as.data.frame (sapply (states, function (x) gsub ("\"", "", x)))
b <- strtrim(a, width = 1)

b<- states1 %>%
  filter(Birth.State == "Virginia")

str(a)
str_replace( " ", "", a)
trimws(a, which = c("both", "left", "right"))
str(a)
library(tidyverse)

str(states1)
#remove spaces
trimws(states1$Birth.State)
a <- str_trim(states1$Birth.State)
str(a)

str(states1)

### THIS IS HOW TO REMOVE THE SPACES
states1$Birth.State <- str_trim(states1$Birth.State)


#############################################################################
#10/6 Lecture - missed first half 

# Missed entire linear regression

#Logistical regression

#read data
data <- read.csv()

install.packages("dplyr")

##########################################################################################################################################################

# @data 10/12
#Assignment 3: Modeling Framework

install.packages('ISLR')
install.packages('caret') 
install.packages('caTools')

library(ISLR)
library(caret)
library(caTools)
library(tidyverse)


# QUESTION 1
# An analyst is interested in making good predictions but doesn't care much about inference or interpretability of the model.
#Which of the following techniques should the analyst use?sent

# ---- Support Vector Machines

# Question 2

#An analyst collects a set of data on the Top 500 firms in the US. For each firm she records profit, number of employees, industry, and CEO salary. 
#She is interested in understanding which factors affect CEO salary. Which of the two is the analyst most interested in?

# ----- Inference 

# Question 3
# A researcher is using a set of price, and promotion variables 
# (e.g., size of discount, special) to predict whether a shopper buys Citrus Hill or Minute Maid orange juice.
# The data is contained in the dataset OJ which is included with the ISLR package. Run the following code to inspect the data.  

library(ISLR) 
data(OJ)
# -----  Classification


# Question 4
#As model complexity goes up, prediction error on sample used to estimate the model (i.e, training data) goes down.

# ---- TRUE


#Question 5
#As model complexity goes up, prediction error on sample NOT used to estimate the model
#(i.e, test data) first goes down, then goes up.

# ---- TRUE

# Question 6
# The Sacramento data that accompanies library(caret) contains house and sale price data for homes in Sacramento CA. 
# To access the data load the caret package and call the Sacramento data as illustrated below.
# Use simple random sampling to split the data into a train and test sample with 80% of the data in the train sample. Use a seed of 1706. 
# What is the absolute difference in average price between the train and test samples?
 
library(caret)
data(Sacramento)
nrow(Sacramento)
head(Sacramento)
set.seed(1706)
split = sample(x = 1:nrow(Sacramento), size = 0.8*nrow(Sacramento))
split[1:10]

length(1:nrow(Sacramento))
length(split)

train = Sacramento[split,]
test = Sacramento[-split,]

nrow(train)
nrow(test)
nrow(Sacramento)

mean(train$price);mean(test$price)
mean(test$price) - mean(train$price)

mod <- lm(price~ beds + baths + sqft + type + latitude + longitude, data = train)
summary(mod)

pred = predict(mod, newdata = test)

rmse1 = sqrt(mean((pred-train$price)^2)); rmse1


pred = predict(model1)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1


# ----- 6639.279

#Question 7
# Now, use stratified sampling to split the Sacramento dataset (used above)
# into a train and test sample with 80% of the data going to the train sample.
# Do the sampling in such a way that the distribution of price is approximately equal in both samples.
# Use a seed of 1706 and utilize createDataPartition from caret package, for the split and set groups to 50. 
#What is the absolute difference in average price between the train and test samples?

data(Sacramento)
set.seed(1706)

split = createDataPartition(y = Sacramento$price, p = 0.8, list = F, groups = 50)
train = Sacramento[split,]
test = Sacramento[-split,]
nrow(train)
nrow(test)
nrow(Sacramento)
mean(train$price);mean(test$price)
mean(train$price) - mean(test$price)
# ---- 1041.071



#Question 8
#The OJ data that accompanies ISLR package contains Orange Juice purchases where the customer either purchased Citrus Hill or Minute Maid.
#To access, the data load the ISLR package and call the OJ data as illustrated below.
# Use stratified sampling to split the OJ dataset into a train and test sample with 80% of the data going to the train sample. 
# Ensure that the proportion of Minute Maid (MM) noted in the Purchase variable is approximately equal across train and test samples.
# Use a seed of 1706. Utilize sample.split() from the caTools package for this problem. What is the proportion of Minute Maid (MM) purchases in the train dataset?

library(ISLR)
data(OJ)
str(OJ)

library(caTools)
head(OJ)
set.seed(1706)
split = sample.split(Y = OJ$Purchase, SplitRatio = 0.8)
table(split)
train = OJ[split,]
test = OJ[!split,]
nrow(train)
nrow(test)
table(test$Purchase)
table(train$Purchase)
prop.table(rbind(train = table(train$Purchase), test = table(test$Purchase)), margin = 1)
# --- 0.3902

#Question 9 
#if the probabilty associated with the test statistic (p-value) is greater than the level of significance (alpha), the null hypothesis is rejected
# --- FALSE

#Question 10 
# level of significance (alpha) should be set only after examining the data
# -- FALSE


##########################################################################################################################################################

## MOD 4 Linear Regression
# Linear Regression HTML

wages = read.csv("wages.csv", stringsAsFactors = T)

# Clean Data, Examine distrivbution of earn

library(ggplot2)
ggplot(data=wages,aes(x=earn))+
  geom_histogram(binwidth=5000,fill='cadetblue')

#Remove Negative earn

sum(wages$earn<0)
wages = wages[wages$earn>0,]

#DATA PARTITION
# Evaluating a model using data to estimate it risks overfitting the model to the data used to build it. 
# For this reason, it is best to evaluate the model using a different dataset. 
# Since getting a fresh dataset for evaluating a model may be costly, time consuming and in some cases impossible, it is common to split the data into a train and test sample, estimating the model using the train and evaluating it using the test.
# 
# Let us start by splitting the data into a train and test sample such that 70% of the data is in the train sample. Use createDataPartition from the caret package with groups = 20. Set seed to 1031 and be sure to do this just before passing createDataPartition()

library(caret)
set.seed(1031)
split = createDataPartition(y=wages$earn,p = 0.7,list = F,groups = 20)
train = wages[split,]
test = wages[-split,]

#Explore Data
str(train)
head(train)

install.packages("skimr")
library(skimr)
skim(train) # GIVES YOU DATA SUMMARY

#Examine Outliers

ggplot(data=train,aes(x='',y=earn))+
  geom_boxplot(outlier.color='red',outlier.alpha=0.5, fill='cadetblue')+
  geom_text(aes(x='',y=median(train$earn),label=median(train$earn)),size=3,hjust=11)+
  xlab(label = '')

#Model 1, Simple regression: earn = f(age)
#Visualize
library(ggplot2)
ggplot(data=train,aes(x=age,y=earn))+
  geom_point()+
  coord_cartesian(ylim=c(0,200000))

#Correlation - is there a linear relationship?

cor(train$age,train$earn)

ggplot(data=train,aes(x=age,y=earn))+
  geom_point()+
  geom_smooth(method='lm',size=1.3,color='steelblue3')+
  coord_cartesian(ylim=c(0,200000))

#Estimate
model1 = lm(earn~age,data=train)
paste('earn','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'age')

#predict
anova(model1)
summary(model1)
pred = predict(model1)
data.frame(earn = train$earn[100:109], prediction = pred[100:109])

sse = sum((pred - train$earn)^2)
sst = sum((mean(train$earn)-train$earn)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$earn)^2); sse1
rmse1 = sqrt(mean((pred-train$earn)^2)); rmse1

#Inference
#Does age influence earn?  What would the wage of a 35 year old be?  can be asnwered by subsititing 35 into regression equation

model1$coef[1]+ model1$coef[2]*35 

#Better way to do it is
predict(model1,newdata=data.frame(age=35))

### Model 2, simple regrssion: earn = f(height)

#Visualize
ggplot(aes(x=height,y=earn),data=train)+
  geom_point()+
  geom_smooth(method="lm",size=1.3,color='steelblue3',se=FALSE)+
  coord_cartesian(ylim = c(0,200000))

#estimate
model2 = lm(earn~height,data=train)
paste('earn','=',round(coef(model2)[1],0),'+',round(coef(model2)[2],0),'height')

#Predict
summary(model2)

pred = predict(model2)
data.frame(earn = train$earn[100:109], prediction = pred[100:109])

sse2 = sum((pred - train$earn)^2)
sst2 = sum((mean(train$earn)-train$earn)^2)
model2_r2 = 1 - sse2/sst2; model2_r2

rmse2 = sqrt(mean((pred-train$earn)^2)); rmse2

#inference - does heigh influence earn?
summary(model2)

#what impact will a 2 inch increase have on wage
2 * coef(model2)[2]

#How much will a six foot person earn (all else being equal)
predict(model2,newdata=data.frame(height=72))

## MODEL 3- simple regression (categorical predictor): earn = f(gender)

#Visualize -- Bar plots make more sense than scatter plots
ggplot(data=train,aes(x=gender,y=earn,fill=gender))+
  geom_bar(stat='summary',fun='mean',position='dodge')+
  guides(fill=F)

library(dplyr)
train%>%
  group_by(gender)%>%
  summarise(meanEarn=mean(earn),earnLow=mean(earn)-1.96*sd(earn)/sqrt(n()),earnHigh=mean(earn)+1.96*sd(earn)/sqrt(n()))%>%
  ungroup()%>%
  ggplot(aes(x=gender,y=meanEarn))+
  geom_errorbar(aes(ymin=earnLow,ymax=earnHigh),size=1.1)+
  geom_line(aes(x=gender,y=meanEarn,group=1),linetype=3)+
  geom_bar(data=train,aes(x=gender,y=earn,alpha=0.2,fill=gender),stat='summary',fun='mean',position='dodge')+
  guides(fill=F,alpha=F)

#Estimate - gender IV, gender is categorical
model3 = lm(earn~gender,data=train)
class(train$gender) # factor

levels(train$gender)
table(train$gender) # for an unordered factor, levels are in alphabetical order

#Predict
summary(model3)
pred = predict(model3)
data.frame(earn = train$earn[100:109], prediction = pred[100:109])

sse3 = sum((pred - train$earn)^2)
sst3 = sum((mean(train$earn)-train$earn)^2)
model3_r2 = 1 - sse3/sst3; model3_r2
rmse3 = sqrt(mean((pred-train$earn)^2)); rmse3

#Inference = do females earn more than males
anova(model3)

#Visualizing data can show things that the indices may not reveal. Dig deeper by plotting a density plot. What do you find?

ggplot(data=train,aes(x=earn,color=gender))+
  geom_density(size=1.1)

#Model 4, simple regression (categorical predictor): earn = f(race)
#Visualize relationship
#examine relationship between race and earn- since race (like gender) is a categorical variable, we will use a bar chat

ggplot(data = train, aes(x = race, y = earn, fill = race)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge") + 
  guides(fill = F)

class(train$race)

levels(train$race)

table(train$race)

#Estimate model 
model4 = lm(earn~race, train)

#predict
summary(model4)

pred = predict(model4)
data.frame(earn = train$earn[100:109], prediction = pred[100:109])

sse4 = sum((pred - train$earn)^2)
sst4 = sum((mean(train$earn)-train$earn)^2)
model4_r2 = 1 - sse4/sst4; model4_r2
rmse4 = sqrt(mean((pred-train$earn)^2)); rmse4

#inference - does race influence earn?  on average, how much do those race with african american earn, if race


#Model 5: mulitple regression: earn = f(height, gender)

#Estimate
model5 = lm(earn~height+gender,data=train)
summary(model5)
pred = predict(model5)

sse5 = sum((pred - train$earn)^2)
sst5 = sum((mean(train$earn)-train$earn)^2)
model5_r2 = 1 - sse5/sst5; model5_r2
rmse5 = sqrt(mean((pred-train$earn)^2)); rmse5
summary(model5)

#Which variable is a strong predictor?
install.packages("lm.beta")
library(lm.beta)
lm.beta(model5)

#model 6 multiple regression: earn = f(height, gender, race,ed, age)
model6 = lm(earn~height+gender+race+ed+age,data=train)
summary(model6)
pred = predict(model6)

sse6 = sum((pred - train$earn)^2)
sst6 = sum((mean(train$earn)-train$earn)^2)
model6_r2 = 1 - sse6/sst6; model6_r2
rmse6 = sqrt(mean((pred-train$earn)^2)); rmse6
summary(model6)
library(lm.beta)
lm.beta(model6)


#Model 7, multiple regression with interaction; earn = f(age, gender, age*gender)

model7 = lm(earn~age+gender+age:gender,data=train)
summary(model7)
pred = predict(model7)

sse7 = sum((pred - train$earn)^2)
sst7 = sum((mean(train$earn)-train$earn)^2)
model7_r2 = 1 - sse7/sst7; model7_r2
rmse7 = sqrt(mean((pred-train$earn)^2)); rmse7
summary(model7)
#statititicans recommend not interpreting the main effects when there is an interaction
ggplot(aes(x=age,y=earn),data=train)+
  geom_point(aes(color=gender))+
  geom_smooth(aes(color=gender),method="lm",size=1.1,se=F)+
  coord_cartesian(ylim = c(1,200000))
#so age is positivley related to earn but only for males


#Model 8 linear regression to examine a non linear relationship (earn = f(age, age^2))

#polynomial regression

model8 = lm(earn~poly(age, 2), data = train)
summary(model8)

pred = predict(model8)
sse8 = sum((pred - train$earn)^2)
sst8 = sum((mean(train$earn)-train$earn)^2)
model8_r2 = 1 - sse8/sst8; model8_r2

rmse8 = sqrt(mean((pred-train$earn)^2)); rmse8
ggplot(aes(x=age,y=earn),data=train)+
  geom_point()+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='steelblue3')+
  coord_cartesian(ylim = c(1,200000))

ggplot(data=train,aes(x=age,y=earn))+
  geom_point()+
  geom_smooth(method='lm',size=1.3,color='steelblue3', se=FALSE)+
  coord_cartesian(ylim=c(0,200000))

#Model 9 - multiple regression

model9 = lm(earn~height+gender+race+ed+age,data=train)
summary(model9)
pred = predict(model9)
sse9 = sum((pred - train$earn)^2)
sst9 = sum((mean(train$earn)-train$earn)^2)
model9_r2 = 1 - sse9/sst9; model9_r2
rmse9 = sqrt(mean((pred-train$earn)^2)); rmse9

#predict out of sample
pred = predict(model9, newdata=test)
sse9_test = sum((pred - test$earn)^2)
sst9_test = sum((mean(train$earn)-test$earn)^2)
model9_r2_test = 1 - sse9_test/sst9_test; model9_r2_test
rmse9_test = sqrt(mean((pred-test$earn)^2)); rmse9_test

#comparing models
model = c('model1', 'model2', 'model3', 'model4', 'model5', 'model6', 'model7', 'model8', 'model9')
sse = c(sse1, sse2, sse3, sse4, sse5, sse6, sse7, sse8, sse9)
rmse = c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7, rmse8, rmse9)
r2 = round(c(model1_r2, model2_r2, model3_r2, model4_r2, model5_r2, model6_r2, model7_r2, model8_r2, model9_r2),4)
results = data.frame(model, sse, rmse, r2)

library(tidyr); library(dplyr)
results

results%>%
  gather(key = metric, value = values,2:4)%>%
  ggplot(aes(x=model, y=values))+
  geom_bar(stat='identity', fill='cadetblue')+
  facet_grid(metric~., scales = 'free_y')

##########################################################################################################################################################

#HW 4, Section 1 Linear Regression 
house = read.csv("house.csv")

library(caret)

#Question 1:  split data, 70% train.  set groups 100, seed 1031, what is average house price in train sample?

set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]
nrow(train)
nrow(test)
nrow(house)
mean(train$price)
 #-- 540165.7

#Question 2: What is the average house price in the test sample?
mean(test$price)

#- 539905.5

# Question 3 
# What is the living area (sqft living) for the house with the most bedrooms?
library(dplyr); library(tidyr)
train %>%
  select(id,price:sqft_lot,age)%>%
  pivot_longer(price:age, names_to='numericVariable',values_to='value')%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales="free_y")

# this graph shows us that there is a house with over 30 bedrooms- from here we can just look at the train dataset to see the living area

## -- 1620

#Question 4: construct a scatterplot to examine relationship between swft_living and price- price on vertical axis- what are direction of points

library(ggplot2)

ggplot(train, aes(y = price, x = sqft_living)) + geom_point()

## - bottom left to top right 

#Question 5  What is the correlation between sqft_lving and price?  

cor(train$price, train$sqft_living)

# --  0.7048
##########################################################################################################################################################

#HW 4, Section 2 Linear Regression 
house = read.csv("house.csv")

library(caret)

#Question 1: Conduct a simple regression to predict house from from area (sqft_living). Call this model1.  what is the pvalue for the f statistic?

set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]
nrow(train)
nrow(test)
nrow(house)

model1 <- lm(price~sqft_living, data = train)
summary(model1)
anova(model1)

# -- less than 0.05'

# Question 2 - what is the R2 for model 1?
#- .4968


# Question 3: what is the RSME

#Estimate
model1 <- lm(price~sqft_living, data = train)
summary(model1)
anova(model1)

pred = predict(model1)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2
sse1 = sum((pred-train$price)^2); sse1

rmse1 = sqrt(mean((pred-train$price)^2)); rmse1

# Question 4:  Is the sqft_living coefficent significantly different from zero? 

summary(model1)

#- True

#Question 5 Based on Model1, what is the average price for a 1400 sqft house

price = intercept (-47764.278) + sqft_living*1400
model1$coef[1]+ model1$coef[2]*1400 

#or

predict(model1,newdata=data.frame(sqft_living = 1400))

# - 347164.3

# Question 6 - if a homeowver were to put in a 200 ft addition, how much would the price be expected to go up?
model1 <- lm(price~sqft_living, data = train)
summary(model1)

200 * coef(model1)[2]
# - 56418.37

#Question 7 - construct regression to predict price from waterfront.  what is r2

model2 <- lm(price~waterfront, data = train)
summary(model2)
# - 0.05889

#question 8 - does waterfront view influence price?  
library(ggplot2)
ggplot(train, aes(y = price, x = waterfront)) + geom_point()

#TRUE

#Question 9- how much more do you expect price with waterfront view 
# look at coeffiecnet- 1103728

#Question 10

pred = predict(model2)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1
# - 357,030.1 compared to 261068.9 rsme for model 1

################################################################################################################################

# HW 4, Section 3
house = read.csv("house.csv")

library(caret)
set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]
nrow(train)
nrow(test)
nrow(house)
#Question 1 use sqft_living and waterfront to predict price. how does model 3 compare to model 1 and 2 in R2


model1 <- lm(price~sqft_living, data = train)
summary(model1)

model2 <- lm(price~waterfront, data = train)
summary(model2)

model3 <- lm(price~sqft_living+waterfront, data = train)
summary(model3)

#- model 3 R2 higher than both (first two options)

#Question 2- Using model3, quantify the impact of a waterfront view on the expected price, while holding area (sqft_living) constant. Specifically, how much more is the expected price of a house with a waterfront view compared to one without a waterfront view, while holding area (sqft_living) constant? 
#This question is slightly different from the question asked of model2 and so is the answer.

summary(model3)

#- 821022.921

#Question 3:  bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade

model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model4)
#- 0.6496

#Question 4L rmse for model 4
pred = predict(model4)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1
#- 217865.8

#Question 5 which of four modle have lowest rsme 
pred = predict(model1)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1

#model 2
pred = predict(model2)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse2 = sqrt(mean((pred-train$price)^2)); rmse2

#model 3
pred = predict(model3)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse3 = sqrt(mean((pred-train$price)^2)); rmse3

#model 4
pred = predict(model4)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1
rmse4 = sqrt(mean((pred-train$price)^2)); rmse4

rmse1
rmse2
rmse3
rmse4

#- model 4



#Question 6 - based on model4, which of the follwoing predictors have an influence on price

summary(model4)

# - all 

#Question 7 - based on model 4, what is the change in price with another bathroom

#- again just look at coeffiecent   53366.25732
model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model4)

#Question 8 - which predictor has strongest influence
model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model4)
library(lm.beta)
lm.beta(model4)
#- sqft_lviing has the largest standardize coefficent 

#Question 9  use test data
library(modelr)
model4 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model4)
pred_test = predict(model4, newdata = test)
y_test <- test$price
SSR_y_test <- sum((y_test-mean(y_test))^2)
cor(pred_test, y_test)^2
rsquare(model4, test)

# 0.658861


#Question 10 :  rsme for model 4 on test sample
pred_test = predict(model4, newdata = test)
rmse_test = sqrt(mean((pred_test - test$price)^2))
rmse_test

# 213162.8

  scoringData[is.na(data$cleaning_fee),]$cleaning_fee <- 0
  str(scoringData$cleaning_fee)






#############################################################################################################################################

#10/06 Lecture #Logistic Regressions



data = read.csv("eBayClean.csv")

#Split Data

library(caTools)
set.seed(617)
split = sample.split(data$sold,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

#Examine Relationship between each variable and SOLD

tapply(train$startprice, train$sold, mean)
library(ggplot2)
ggplot(data=train,aes(x=factor(sold),y=startprice,fill=factor(sold)))+
  geom_bar(stat='summary',fun.y='mean')+
  coord_flip()

library(dplyr)  #cleaner version
train%>%
  mutate(sold = factor(sold,labels = c('not sold','sold')))%>%
  group_by(sold)%>%
  summarize(avg_startprice = mean(startprice))%>%
  ungroup()%>%
  ggplot(aes(x=sold,y=avg_startprice,fill=sold))+
  geom_col()+
  geom_text(aes(x=sold,y=avg_startprice,
                label=paste0('$',round(avg_startprice,2),'   ')),
            nudge_y=25)+
  ylab('start price')+
  coord_flip()+
  theme_bw()


#Biddable is a binary variable, so lets plot biddable against the proportion of ipads sold

tapply(train$sold, train$biddable, mean)
ggplot(data=train,aes(x=biddable,y=sold,fill=biddable))+
  geom_bar(stat='summary',fun.y='mean')+
  coord_flip()



#Model 1: The bar chart above indicates differences in startprice between iPads that sold vs. those that did not sell. So, let us see if startprice has an effect on whether an iPad sells.

model1 = glm(sold~startprice,data=train,family='binomial')
model1

#Prediction: What is the probability of an Ipad priced at $200 selling?  Two ways of doing this:

#can enter coefficents into the model

exp(model1$coef[1] + model1$coef[2]*200)/(1+exp(model1$coef[1] + model1$coef[2]*200))

#OR use predict function  #need to use type = response to get a probability

predict(model1,newdata=data.frame(startprice=200),type='response') 

#Is the coeff of startprice significant?
#To interpret coefficent, 
summary(model1)$coef[2]

#Specifically, what is the percent increase in likelihood of an upad being sold with a $1 increase in startprice?

100*(exp(summary(model1)$coef[2])-1)


#MODEL 2:   The bar chart of storage indicated differences in percentage of iPads sold for different levels of storage. So, let us use storage to predict whether an iPad is sold. As you examine the result below, bear in mind that storage is a nominal scaled variable with three levels.

levels(train$storage)
train$storage <- as.factor(train$storage)

model2 = glm(sold~storage,data=train,family='binomial')
summary(model2) 

#Predict the probability of sale (sold= 1) for an ipad with storage less than 128 GB

#Method 1, entering in coefficents 
exp(model2$coef[1]+model2$coef[2]*1+model2$coef[3]*0)/
  (1+exp(model2$coef[1]+model2$coef[2]*1+model2$coef[3]*0))

#use predict function
predict(model2,newdata=data.frame(storage='Less than 128 GB'),type='response')

#Inference is the impact of storage on probabilty of selling an ipad signficant?
summary(model2)

#What does the coefficient of storage mean? Specifically, what is the chance of selling an iPad with storage Less than 128 GB (relative to 128GB storage)?
summary(model2)$coef[2] # coefficient of storage "Less than 128 GB"

#How many times better is the likelihood of selling an iPad with less than 128 GB vs an iPad with 128 GB of storage?
exp(summary(model2)$coef[2]) 

#Phrased differently, how much more is the likelihood of selling an iPad with less than 128 GB of storage relative to one with 128GB

100*(exp(summary(model2)$coef[2])-1)


#model 3 - use all predictor variables


model3 = glm(sold~biddable+startprice+condition+cellular+carrier+color+
               storage+productline+noDescription+upperCaseDescription+startprice_99end,
             data=train,
             family='binomial')
summary(model3)

#is model3 beter than model1 and 2? compare AIC, lower AIC, the better the model

summary(model3)$aic
summary(model2)$aic
summary(model1)$aic

#infereence, which coefficents are stat significant?
summary(model3)


#Predict:  
pred = predict(model3, type = 'response')
#now lets examine the quality of predictions by comparing them to the true values:  here are the first ten observations

data.frame(sold = train$sold[1:10],
           predicted_probability = pred[1:10])

#To convert prediction probabilities to a binary outcome, one can use a cut off or threshold value. Lets use a cutoff of 0.5.

data.frame(sold = train$sold[1:10],
           predicted_probability = pred[1:10], 
           prediction_binary = as.integer(pred[1:10]>0.5))

#Accuracy
#Once the probabilities are converted to a binary outcome, they can easily be compared to the true values. Let us summarize the predictions in a classification table.

ct = table(sold = train$sold,
           predictions = as.numeric(pred>0.5))
ct

#Overall quality of predictions can be computed as the proportion of correct predictions, known as accuracy. Cost of false negatives can be accounted for by the Specificity and cost of false negatives by the Sensitivity.
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

#While higher accuracy is desirable, what is a good value really depends on the data. It may be tempting to use a coin flip as a threshold for predicting a binary outcome but in most cases this may be setting a very low bar. A better baseline is to compare it to majority class. This is the proportion of correct predictions if one predicts the outcome to be the same as the more common category. In our specific example, the majority class threshold would be the proportion of ipads that are not sold.
prop.table(table(train$sold))

# or
max(sum(train$sold==0), sum(train$sold==1))/nrow(train)

#Predict on Test
#Model Performance on train sample is bound to be be inflated, so we are going to evaluate prediction quality or model performance on the test sample. But, before that, let us establish a baseline for accuracy. Since in our train sample, most ipads were not sold, the majority class is sold==0.

sum(test$sold == 0)/nrow(test)

#Accuracy Lets us compute prediction quality on the test sample using model3
pred = predict(model3,newdata=test,type='response')
ct = table(sold = test$sold,
           predictions = as.integer(pred>0.5)); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

#Cutoff value The accuracy (also known as hit-ratio) generated above is dependent on the cutoff applied to the prediction probabilities. The histogram below shows a bimodal distribution of predicted probabilities and the cufoff used above. So far, we have used a cutoff of 0.5, however this threshold is is arbitrary. Using a different cutoff is likely to generate a different value of accuracy.
ggplot(data=data.frame(pred),aes(x=pred))+
  geom_histogram(fill='steelblue3')+
  geom_vline(xintercept =0.5, size=1.5,color='rosybrown')

#Let us use a different cutoff (and also a slighly different albeit equivalent way of computing accuracy). You can experiment with other cutoffs as well.
accuracy_0.5 = sum(as.integer(pred>0.5)==test$sold)/nrow(test); accuracy_0.5
accuracy_0.4 = sum(as.integer(pred>0.4)==test$sold)/nrow(test); accuracy_0.4
accuracy_0.3 = sum(as.integer(pred>0.3)==test$sold)/nrow(test); accuracy_0.3
accuracy_0.6 = sum(as.integer(pred>0.6)==test$sold)/nrow(test); accuracy_0.6
accuracy_0.7 = sum(as.integer(pred>0.7)==test$sold)/nrow(test); accuracy_0.7

#Here is a plot of accuracy values varying by cutoff. While accuracy and its sister metrics specificity and sensitivity are simple and intuitive, their dependence on cutoff value calls for a metric that is independent of the cufoff value.
acc = sapply(X = seq(0.050,0.95,0.01),
         FUN = function(x) sum((as.integer(pred>x) == test$sold))/nrow(test))

ggplot(data=data.frame(cutoff=seq(0.05,0.95,0.01),
                       accuracy=acc),
       aes(x=cutoff,y=accuracy))+
  geom_point()


#ROC and area under the curve
install.packages("ROCR")

library(ROCR)
ROCRpred = prediction(pred,test$sold)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)

#color coded and annotated ROC curve
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),
     xlab="1 - Specificity",ylab="Sensitivity") 

#Area under the curve
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#As a reference, here is what the ROX for a baseline model will look like
baselinePred = pred*0
ROCRpred = prediction(baselinePred,test$sold)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,xlab="1 - Specificity",ylab="Sensitivity") # relabeled axes

#Baseline AUC
as.numeric(performance(ROCRpred,"auc")@y.values)




#Assignment 5, Section 1: Logistic Regression

data <- read.csv("ebay.csv", stringsAsFactors = T)
head(data)


#Question 1:  How many rows are in the dataset
nrow(data)
#1861

#Question 2:  How many iPads are black?

head(data)
library(tidyverse)
unique(data$productline)
subset(data, data$color == "Black" & data$productline == c("iPad 2", "iPad 4", "iPad mini 2", "iPad 3", "iPad 1", "iPad mini", "iPad Air 1/2", "iPad mini Retina")) %>%
  count(color)

subset(data, data$color=="Black") %>%
  count(color)


#a <- subset(data, data$color == "Black" | data$productline == "iPad 2" | data$productline == "iPad 4")

#425 black ipads



#Question 3:  Which of the following ipads dos this dataset contain
levels(data$productline)
#ipad1, ipad2, ipad3, ipad4, ipad air 1/2 , ipad mini, ipad mini2, ipad mini retina, ipad mini3, unkwown
#ALL BUT IPAD 5, IPAD 4 Mini

#Question 4 What is the uniqueID of the ipad with the highest startprice?
data[data$startprice == max(data$startprice), ]
#Unique ID = 11397

##################### 
# Assignment 5, Section 2: Logisitic Regression
#Question 1: Split data in train and test, 80% in train.  Use sample.split.  How many rows in train sample


data <- read.csv("ebay.csv", stringsAsFactors = T)

library(caTools)
head(data)
set.seed(196)
split = sample.split(Y = data$sold, SplitRatio = 0.8)
table(split)
train = data[split,]
test = data[!split,]
nrow(train)
#1489

#questoin 2: what is the median startprice of ipads that sold?
head(train)
sold <- subset(train, sold == '1')
median(sold$startprice)

#99

#question 3: what is the median start price of ipads that did not sell?
nosell <- subset(train, sold == '0')
median(nosell$startprice)

#249.99


#Question 4: Now, let us run a model to predict the variables that influence whether an iPad will be sold or not.  
#Since the variable to be predicted only takes on two values, we will use a logistic regression model. Use the 'glm' function to build a model,
#setting family to binomial with sold as the dependent variable and the following independent variables:

model1 <- glm(sold ~ biddable + startprice +  condition+ cellular+ carrier+ color+ storage+ productline+ noDescription+ charCountDescription+ upperCaseDescription+ startprice_99end, data = train, family = 'binomial')
summary(model1)
# 1454.5 
#but when you just do 
model1
#AIC is 1454


#Question 5 Now let us examine individual variables.  Which of the following variables has a singificant influence on whether an ipad is sold or not
summary(model1)
#Think we are just looking at the variables that have the *, so it is stat significant
#biddable, startprice, cellularunkwon, productline


#Question 6 Based on the results of the model, does 99 ending for startprice singifncatly increase the change of an iPad being sold?
summary(model1)
#statitically, no

# Question 7 Based on the model, does color of the upad have a statitically signifncat impact on wheter an ipad is sold? 
#no



##########
#Assignment 5, Section 3
data <- read.csv("ebay.csv", stringsAsFactors = T)

library(caTools)
head(data)
set.seed(196)
split = sample.split(Y = data$sold, SplitRatio = 0.8)
table(split)
train = data[split,]
test = data[!split,]
nrow(train)

#question 1: Simpler models are generally preferred to more complex models because they are less likely to overfit the data. So, let us drop out non-signficant variables from model1 above but keep variables that previous research or experience indicates should have an effect. So, estimate model2 with the following variables:

#biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end
#What is the AIC for model2?

model2 <- glm(sold ~ biddable+ startprice + condition + storage + productline + upperCaseDescription + startprice_99end, data = train, family = 'binomial')
summary(model2)
#1448.5

#Question 2:  based on the coeffiecnet of upperCaseDescription, what advice would you give someone selling ipads on ebay?
#coefficent is negative, so i would suggest use fewer upper case letters
summary(model2)
levels(train$productline)

#Question 3  You will note that the data contains a number of factor variables. In order to model factor variables, they have to be dummy coded. 
#Fortunately, glm and lm functions automatically dummy code factor variables and then run the dummy variables in the model. The first level is usually selected to be the baseline or reference variable to which each of the other levels is compared.
#Review the results of model2 and the coefficients of the variables. After controlling for the effects of all other variables in the model, what sells better iPad3 or iPad 1?
summary(model2)
100*(exp(summary(model2)$coef[10])-1) #ipad 3
100*(exp(summary(model2)$coef[17])-1) #unknown (ipad 1)
#since ipad 3 is greater, it sells better

#Question 4 If startprice goes up by $1, what will be the % reduction in the chance of selling an iPad. To interpret coefficients in logistic regression, you have to exponentiate the coefficient. E.g., exp(coefficient)

100*(exp(summary(model2)$coef[3])-1) #looking at third coefficent (start price)

# -.8965784
#closest to 1% 


#Question 5: Based on model2 (and controlling for the effects of all other variables), how much more (or less) likely is an iPad Air 1/2 to sell compared to an iPad 1?
100*(exp(summary(model2)$coef[12])-1) #ipad air 1/2
100*(exp(summary(model2)$coef[17])-1) #unknown (ipad 1)

#Question 6 Now, let us run one more model called model_productline. For this model, predict the variable 'sold' using only 'productline'. Is the sign of the coefficient for iPad Air1/2 in this model the same as that in model2? 

model_productline <- glm(sold ~ productline, data = train, family = 'binomial')
summary(model_productline)
#in this model, ipad air 1/2 sign is negative
summary(model2)
#in model 2, ipad air 1/2 sign is positive 


#Assignment 5: Section 4

#Question 1: Use model2 to generate predictions for 'sold' in the test set. What is the probability of sale for an iPad with UniqueID 10940?

pred_test <- predict(model2, newdata = test, type = 'response')
df <- data.frame(ID = test$UniqueID, prediction = pred_test)
df %>%
  filter(ID == 10940)
##0.02824507

#Question 2 #what is the accuracy of model 2 on the test set?  use threshold of 0.5
pred = predict(model2, newdata = test, type = 'response')
ct = table(sold = test$sold,
           predictions = as.integer(pred>0.5)); ct
accuracy = sum(ct[1,1], ct[2,2])/nrow(test); accuracy
#0.8064516

#Question 3:In order to evaluate a model, one has to have a point of reference or a baseline. A suitable baseline the accuracy of the test sample using the majority class in the train sample. In other words, what is the accuracy in the test sample, if we assumed the majority class of the train sample for all observations?

baseline = table(test$sold)[1]/nrow(test); baseline
#0.5376
#compared to question 2 accuracy, model 2 is performing better

#Question 4: Is model2 performing better than the baseline?
#since baseline is above 0.5, then model2 is performing better

#Question 5 The accuracy measure depends on the cut-value (or threshold) used. Hence a more popular measure is area under the curve (or AUC). AUC is computed by finding the area under the curve of a plot of Senstivity vs. 1-Specificity. AUC is a model performance measure that is independent of any particular threshold. What is the AUC for model2 on the test sample?

library(ROCR)
ROCRpred = prediction(pred, test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.868968

##################################################################################################################################################################

#### 10/27/2020  Module 8
#Classification and Regressiont trees (CART)

data <- read.csv("ebayClean-1.csv")

#Split Data
library(caTools)
set.seed(617)
split = sample.split(data$sold,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]


head(train)
names(train)


#Classification Tree1
#CART models can be used to address both classification and regression problems.
#Since sold is a categorical variable, let us construct a classification tree by setting method  = "class" in rpart

install.packages("rpart")
install.packages("rpart.plot")

#Estimate
library(rpart)
classTree1 = rpart(sold~startprice, data = train, method = 'class')
print(classTree1)
#Inferenece
#The visual result from running a CART model is easier to interpret than the summary result
summary(classTree1)

#Based on the inverted tree generated from running the CART model, does startprice appear to influence whether an ipad is sold?


library(rpart.plot)
rpart.plot(classTree1)
prop.table(table(train$sold))

#predict 
#can apply to training data
prop.table(table(train$startprice > 101))
 #can use this when have only one decision from the tree

#Predict 
#Let us predict whether an iPad will sell if startprice is 150 dollars. As in the case of regression, there are two ways of generating predictions. For a simple tree, one can interpret the tree to make predictions. In the tree output, a price fo 150 dollars is greater than 101 dollars, therefore an iPad with a startprice of 150 dollars is predicted to not sell. For constructing a large number of predictions or for even moderately complex trees, it makes sense to use the predict function. For a classification tree, the default type is ‘prob’.

predict(classTree1,newdata = data.frame(startprice=150),type = 'prob')

#Alternatively, one can set type=‘class’ to get a class prediction.
predict(classTree1,newdata = data.frame(startprice=150),type = 'class')

#Now, let us gather probability predictions of observations in the train sample. Here, we examine the first ten predictions. Note the result contains the probability for each level.

predict(classTree1)[1:10,]

#####
#Regression Tree1  #PREDICTING MEAN VALUE AT EACH DECISION BRANCH
#Since sold is a binary variable with values of 0 and 1, one can use a regression tree, interpreting the prediction as a probability. For a regression tree, use method=‘anova’ in rpart(). However, if you do not mention the method, rpart will use method=‘anova’ as it is the default.

#Estimate
regTree1 = rpart(sold~startprice,data=train,method = 'anova') 

#inference Based on inverted tree generated from running a CART model, does startprice appear to influence whether an iPad is sold?

rpart.plot(regTree1)


summary(regTree1)

#Predict Let us predict whether an iPad will sell if startprice is 150 dollars. The regression tree output is more nuanced than the one from the classification tree as it requires navigating through a few nodes to arrive at the leaf with the probability of 0.41. Replicating this using predict
predict(regTree1,newdata = data.frame(startprice=150))


####### Regression Tree1 Complex

#Let us construct a larger tree by changing the value of cp. In general, the smaller the value of cp, the larger the tree and the greater the complexity of the model.

regTree1_complex = rpart(sold~startprice,train,cp=0.005)
rpart.plot(regTree1_complex)

#  For the above tree, what is the chance of selling an iPad if price is 150 dollars?
# for the tree itself, how many nodes does the final tree contain? How many nodes did the algorithm consider? Note the complexity parameter for the chosen nodes. How many leaves does the tree contain?
  
summary(regTree1_complex)

mean(train$sold)

# Classification Tree2

#Let us examine the effect of storage on sale of an iPad. Does storage influence sale of an iPad?

classTree2 = rpart(sold~storage,data=train,method='class')
rpart.plot(classTree2)

summary(classTree2)
predict(classTree2)[1:10,]

#Regression Tree2 
#Let us use a regression tree to examine the effect of storage on sale of an iPad. Note, the difference of the root node from that in the classification tree above.
regTree2 = rpart(sold~storage,data=train,method='anova')
rpart.plot(regTree2)
summary(regTree2)
predict(regTree2)[1:10]


#Classification Tree 3
#Lets now use a ton of variables
classTree3 = rpart(sold~startprice+biddable+condition+cellular+carrier+color+
                     storage+productline+noDescription+upperCaseDescription+startprice_99end,
                   data=train,
                   method='class')

#Inference Which variables influence whether an iPad sells? Which of these is the most important predictor?

rpart.plot(classTree3) # three predictions lead to 0, two predictions lead to 1


sort(classTree3$variable.importance)
#Lets us generate predictions and then summarize them into a classification table

pred = predict(classTree3,type='class')
ct = table(sold = train$sold, predictions = pred)
ct

#Overall quality of predictions can be computed as the proportion of correct predictions, known as accuracy. Cost of false negatives can be accounted for by the Specificity and cost of false negatives by the Sensitivity.

accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

#Classification Tree3 Complex
#What would happen if the tree were more complex? Instead of using cp as above, let us increase complexity by varying the minbucket parameter. On examining the tree, you will note in this particular case, it doesn’t result in a larger tree. This is because of a stopping rule built into rpart.

#alternate way to identify the simplicity/complexity from a regression model
classTree3Complex = rpart(sold~startprice+biddable+condition+cellular+carrier+color+
                            storage+productline+noDescription+upperCaseDescription+startprice_99end,
                          data=train,
                          method='class',
                          control=rpart.control(minbucket = 25))
rpart.plot(classTree3Complex)
#this is the same model
classTree3Complex = rpart(sold~startprice+biddable+condition+cellular+carrier+color+
                            storage+productline+noDescription+upperCaseDescription+startprice_99end,
                          data=train,
                          method='class',
                          cp = 0.004  # to find a tree that more complex
                          #control=rpart.control(minbucket = 25))
)
rpart.plot(classTree3Complex)

summary(classTree3Complex)

#prdict 
pred = predict(classTree3Complex,type='class')
ct = table(sold = train$sold,predictions = pred); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity


#cutoff
#As in the case of logistic regression, it is possible to manually set a cutoff or threshold for splitting probability of an iPad selling. Here, we illustrate this using a threshold of 0.6.
# prob[y ==1]

pred = predict(classTree3Complex,type='prob')
ct = table(sold = train$sold,
           predictions = as.numeric(pred[,2]>0.6))
ct
# can compare different cutoffs
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity


#Prediction on test
#out of sample performance
#let us apply the predictions to the test sample

pred_complex = predict(classTree3Complex,newdata=test,type='class')
ct = table(test$sold,pred_complex); ct

accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

# simple vs complex ( test prediction performance)  ## Which is better????

#ROC and Area under the curve
#ROC curves allow us to visualize the impact of different thresholds on Specificity and Sensitivity. AUC is a model performance measure that is independent of any particular cutoff or threshold.
#By default, a classification tree will generate class predictions. In order to construct an ROC curve, we need probability predictions, so we run predict with type=‘prob’ and use the predictions in the second column, i.e., pred[,2]. (It is possible to use regression tree predictions to construct an ROC curve but the curve and resultant AUC will be slightly different owing to differences in how a regression tree is optimized.)

library(ROCR)
pred_complex = predict(classTree3Complex,newdata=test,type='prob')
ROCRpred_complex = prediction(pred_complex[,2],test$sold)
ROCRperf_complex = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf_complex)

pred_simple = predict(classTree3, newdata = test, type = 'prob')
ROCRpred_simple= prediction(pred_simple[,2],test$sold)
ROCRperf_simple = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf_simple)

par(mfrow = c(1,2))
plot(ROCRperf_simple)
plot(ROCRperf_complex)

#AUC 
as.numeric(performance(ROCRpred_complex,"auc")@y.values) # auc measure
as.numeric(performance(ROCRpred_simple, "auc")@y.values)
#color coded and annotated ROC curve
plot(ROCRperf,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,0.4),
     text.adj=c(-0.3,2),
     xlab="1 - Specificity",
     ylab="Sensitivity") 



########################################################################

#Visualize Results Tree Splitting Algo

data = iris[1:100,]
data$Species <- droplevels(data$Species)
table(data$Species)

library(ggplot2)
ggplot(data=data, aes(x=Sepal.Length, y = Sepal.Width, color=Species))+
  geom_point()

library(rpart); library(rpart.plot)
tree = rpart(Species~Sepal.Length+Sepal.Width, data, method='class')
print(tree)
rpart.plot(tree)

#if less than 5.5, predict is as mostly setosa
ggplot(data=data, aes(x=Sepal.Length, y = Sepal.Width, color=Species))+
  geom_point()+
  geom_vline(xintercept=5.45, size=1.4, color='black') #5.45 is the condition thats specificed at first node
#this simple split is able to classify the data ^

## below - added another split of the data based on the decision tree

ggplot(data=data, aes(x=Sepal.Length, y = Sepal.Width, color=Species))+
  geom_point()+
  geom_vline(xintercept=5.45, size=1.4, color='black')+
  geom_segment(aes(x=4.3,xend=5.45,y=2.95,yend=2.95), size=1.4, color='black')



# add another 

ggplot(data=data, aes(x=Sepal.Length, y = Sepal.Width, color=Species))+
  geom_point()+
  geom_vline(xintercept=5.45, size=1.4, color='black')+
  geom_segment(aes(x=4.3,xend=5.45,y=2.95,yend=2.95), size=1.4, color='black')+
  geom_segment(aes(x=5.45,xend=7,y=3.25,yend=3.25), size=1.4, color='black')








######################################################################################################

#Module 6: Feature Selection

#Feature Selection Methods
# Use Theory
#Test all possible subsets
#Stepwise methods
#Ridge and Lasso
#Dimension Reduction


wine = read.table("winequality-white.csv",header=TRUE,sep=";")
library(caret)
set.seed(1031)
split = createDataPartition(y=wine$quality,p = 0.7,list = F,groups = 100)
train = wine[split,]
test = wine[-split,]


#Manual Approach 
#Predictors used in a model must be

#Relevant: Related to the outcome variable, and
#Non-redundant: Not related to other predictors

#Examine Individual Variables
#One can manually examine individual predictors to assess relevance and redudancy. A predictor that is associated with the outcome is relevant and one that is not related to other predictors is non-redundant.

#Examine bivariate correlations
#Large numbers indicate high correlations. A good predictor has a high correlation with the outcome but low correlations with other predictors.

cor(train[,-12])

#simplify the matrix to see the numbers better

round(cor(train[,-12]), 2)*100

#Visualizing the matrix to spot high correlations may be easier. Here, we use ggplot2 to construct a correlation matrix.
library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(train[,-12]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

#There are also packages to build a correlation matrix.
install.packages("corrplot")
library(corrplot)
corrplot(cor(train[,-12]),method = 'square',type = 'lower',diag = F)


#Correlations focus on bivariate relationships to assess relevancy and redundancy. A predictor deemed to be irrelevant or redundant based on bivariate correlations must be dropped. On the other hand, a predictor identified as being relevant and non-redundant based on bivariate correlations may still not be included in the model. This is because a predictor may still be irrelevant or redudant in a multivariate sense. To examine this possibility, we can examine statistical significance of regression coefficients and variance inflating factor.

model = lm(quality~.,train)
summary(model)

#Threat of collinearity can also come from linear relationships between sets of variables. One way to assess the threat of multicollinearity in a linear regression is to compute the Variance Inflating Factor (VIF). 1<VIF<Inf. VIF>10 indicates seious multicollinearity while 5<VIF<10 may warrant examination.
install.packages("car")
library(car)
vif(model)



#######
#Subset Selection
# 4 subset selection approaches
#  Best subset selection
#  Forward
#  Backward
#  Stepwise


# Best Subset Selection
# In this approach, we test all possible subsets of p predictors.

#regsubsets compares different models with adjusted R2 or Mallow’s Cp or BIC. The process involves finding the prediction error for each subset and use the subset with the lowest prediction error. This process can be slow and computationally intensive.

#Since we have 11 predictors, regsubsets() will generate 11 models to reflect the best combination for every set (e.g., 1 predictor, two predictors,.. )

install.packages("leaps")

library(leaps)
subsets = regsubsets(quality~.,data=train, nvmax=11)
summary(subsets)
names(summary(subsets))

subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures

library(ggplot2)
library(tidyr)
subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  ggplot(aes(x=model,y=value))+
  geom_line()+
  geom_point()+
  facet_grid(type~.,scales='free_y')

#regsubsets solution with lowest cp

which.min(summary(subsets)$cp)
coef(subsets,which.min(summary(subsets)$cp))


### Forward Selection

start_mod = lm(quality~1,data=train)
empty_mod = lm(quality~1,data=train)
full_mod = lm(quality~.,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)


#Backward selection
start_mod = lm(quality~.,data=train)
empty_mod = lm(quality~1,data=train)
full_mod = lm(quality~.,data=train)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)

#Stepwise variable Selection
start_mod = lm(quality~1,data=train)
empty_mod = lm(quality~1,data=train)
full_mod = lm(quality~.,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)

####Shrinkage

#Ridge
install.packages("glmnet")
library(glmnet)
x = model.matrix(quality~.-1,data=train)
y = train$quality
ridgeModel = glmnet(x,y,alpha=0)
plot(ridgeModel,xvar='lambda',label=T)

# for cv.glmnet, default is 10-fold cross validation

set.seed(617)
cv.ridge = cv.glmnet(x,y,alpha=0) 
plot(cv.ridge)
coef(cv.ridge)

## Lasso

#note default for alpha in glment is 1 which corresponds to Lasso
lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
plot(lassoModel,xvar='dev',label=T)


set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)

coef(cv.lasso)


### Dimension Reduction

#In this approach, p predictors are reduced to a smaller number of components based on a measure of similarity (e.g., correlation). Here, we will examine a popular dimension reduction technique called Principal Components Analysis.

#Extract the predictors to be reduced.
#PCA

trainPredictors = train[,-12]
testPredictors = test[,-12]

#Conduct Principal Components Analysis on train sample. Principal components analysis will always generate as many components as variables. The first few components contain the most amount of variance. One heuristic for number of components to retain is a cumulative variance greater than 70%. In this case, we are extracting only six of eleven components.

pca = prcomp(trainPredictors,scale. = T)
train_components = data.frame(cbind(pca$x[,1:6], quality = train$quality))

#Let’s examine the components generated for these six components for the first 6 obs

head(train_components)

#Construct a model using the components derived from the original predictors

train_model = lm(quality~.,train_components)
summary(train_model)

#Before we can apply the train model to the test set, we need to ensure that we apply the same variable transformations for the train sample on the test sample. Specifically, we need to apply the same component structure from the train sample predictors to the test predictors. Note, we are not running a fresh principal components analysis on the test sample, rather we are imposing the train component structure on the test sample.

test_pca = predict(pca,newdata=testPredictors)
test_components = data.frame(cbind(test_pca[,1:6], quality = test$quality))
#Note, train_components and test_components have the same structure.

str(train_components)
str(test_components)

# Finally, we evaluate the estimated train model on the test data to assess model performance.
pred = predict(train_model,newdata=test_components)
sse = sum((pred-test_components$quality)^2)
sst = sum((mean(train_components$quality) - test_components$quality)^2)
r2_test = 1 - sse/sst
r2_test

###################################################
#11/10  Module 9, Advanced Trees


data = read.csv('wages.csv', stringsAsFactors = T) 
data = data[data$earn>0,]

set.seed(617)
#sample 80 percent of the data using random sampling - without balancing effect of response variables
split = sample(1:nrow(data),size = nrow(data)*0.8) 
train = data[split,]
test = data[-split,]


library(rpart); library(rpart.plot)
tree = rpart(earn~.,data=train)
pred = predict(tree,newdata=test)

rpart.plot(tree)
rmse_tree = sqrt(mean((pred-test$earn)^2)); rmse_tree


maximalTree = rpart(earn~.,data=train,control=rpart.control(cp=0)) 
pred = predict(maximalTree,newdata=test)
rmse_maximalTree = sqrt(mean((pred-test$earn)^2)); rmse_maximalTree

# how do we decide what the right complexity parameter is?  

# cross validation 
library(caret)
trControl = trainControl(method='cv',number = 5) # 5 fold vs 10 fold cross validation 
tuneGrid = expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001)) #run 100 values of the cp parameter
set.seed(617)
cvModel = train(earn~., data = train, 
                method = "rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)
cvModel$results
cvModel$bestTune$cp  # this shows what the best cp is

summary(cvModel)

plot(cvModel$results$cp, cvModel$results$RSME, type = "b")

library(ggplot2)
ggplot(data=cvModel$results, aes(x=cp, y=RMSE))+
  geom_line(size=0.5,alpha=0.2)+
  geom_point(color='brown')+
  theme_bw()+
  ggtitle(label=paste('Lowest RMSE is at a cp of ',cvModel$bestTune$cp))
# now use cp control 
cvTree = rpart(earn~.,data=train,cp = cvModel$bestTune$cp)
pred = predict(cvTree,newdata=test)
rmse_cvTree = sqrt(mean((pred-test$earn)^2)); rmse_cvTree


## Bootstrapping models
#Bag models
#Bootstrap Aggregation models generate a large number of bootstrapped samples. A tree is fit to each boot- strapped sample. Predictions are generating as an average of all models (for numerical outcome variables) or the majority group (for categorical outcome variables).
# These models can be implemented using many packages including randomForest,adabag, bagEarth, treeBag, bagFDA. In this illustration, we are using a randomForest model by setting mtry to be the number of predictors, i.e., 5.

#bootstrapp aggregation (Bagging)
library(randomForest)
set.seed(617)

bag = randomForest(earn~.,data=train,mtry = ncol(train)-1,ntree=1000)  #mtry = number of predictors  #fit 1000 trees
pred = predict(bag,newdata=test)
rmse_bag = sqrt(mean((pred-test$earn)^2)); rmse_bag

hist(treesize(bag))

getTree(bag , k = 100 ) #one can examine a given tree

#generally, error for a bag model with an increase in the trees
plot(bag)
plot(bag, log = 'x')

head(getTree(bag, 1, labelVar = TRUE))
varImpPlot(bag) #relative importance of predictors

importance(bag)


# Random Forest -  different from bagging, not only bootstrapping, also randomly subsampling predictor variables. 
# #Unlike bag models which consider all predictors in constructing each tree, randomForest models consider a subset of predictors (default is p/3 for numerical outcomes or sqrt(p) for categorical outcomes) for con- structing each tree. In library(randomForest), the mtry controls number of variables considered for each tree.
library(randomForest)
set.seed(617)

#mtry p/3 or sqrt(p)  p being the predictors
forest = randomForest(earn~., data=train, ntree=1000, mtry = floor(5/3)) #number of predictors / 3 
pred = predict(forest, newdata = test)
rmse_forest = sqrt(mean((pred-test$earn)^2)); rmse_forest

plot(forest)
plot(forest, log = 'x')

varImpPlot(forest)

par(mfrow=c(1,2)) # for side by side plot
varImpPlot(bag)
varImpPlot(forest)

#Cross validation -- aka tuning random forest

#The mtry parameter of a randomForest model can be tuned to improve model predictions. Here, we will use 5-fold cross validation to examine four values of mtry.
trControl=trainControl(method="cv",number=5) 
tuneGrid = expand.grid(mtry=1:4) 
set.seed(617)

cvModel = train(earn~., data = train, method = "rf", ntree = 1000, trControl= trControl, tuneGrid= tuneGrid)
cvModel

# use best selected mtry
cvForest = randomForest(earn~.,data=train,ntree = 1000,mtry=cvModel$bestTune$mtry) 
pred = predict(cvForest,newdata=test)
rmse_cv_forest = sqrt(mean((pred-test$earn)^2)); rmse_cv_forest

#different package type 
install.packages("ranger")
library(ranger)
forest_ranger = ranger(earn~.,data=train,num.trees = 1000)
pred = predict(forest_ranger, data =test,num.trees = 1000)
rmse_forest_ranger = sqrt(mean((pred$predictions-test$earn)^2)); rmse_forest_ranger

trControl=trainControl(method="cv",number=5)

tuneGrid = expand.grid(mtry=1:4,
                splitrule = c('variance','extratrees','maxstat'), min.node.size = c(2,5,10,15,20,25))
set.seed(617)
cvModel = train(earn~.,
                data=train, method="ranger", num.trees=1000, trControl=trControl, tuneGrid=tuneGrid )
cv_forest_ranger = ranger(earn~., data=train,
                          num.trees = 1000,
                          mtry=cvModel$bestTune$mtry,
                          min.node.size = cvModel$bestTune$min.node.size, splitrule = cvModel$bestTune$splitrule)
pred = predict(cv_forest_ranger, data =test, num.trees = 1000)
rmse_cv_forest_ranger = sqrt(mean((pred$predictions-test$earn)^2)); rmse_cv_forest_ranger

### boosting 
# Like bag and forest models, boosting models fit a tree to bootstrapped samples. The key differenc is that in boosting, trees are grown sequentially, each tree is grown using information from previously grown trees. Thus, boosting can be seen as a slow learning evolutionary model. Since we are predicting a numerical variable, earn, the distributio is set to ‘gaussian’. Had the goal been to predict a binary outcome, we would have set distribution to ‘bernoulli’.

install.packages("gbm")
library(gbm)

set.seed(617)
boost = gbm(earn~.,
            data=train, distribution="gaussian", n.trees = 500, interaction.depth = 2, shrinkage = 0.01)
# boosting models offer a way to examine the relative influence of variables
summary(boost)


pred = predict(boost,n.trees = 500)
rmse_boost_train = sqrt(mean((pred-train$earn)^2)); rmse_boost_train

#now rsme on test
pred = predict(boost,newdata=test,n.trees = 500)
rmse_boost = sqrt(mean((pred-test$earn)^2)); rmse_boost


#boosting with cross validation
#Boosting models are notorious for overfitting training data. A very simple way to see this borne out is to see the effect of increasing number of trees on train and test rmse. Specifically, in the model above, try running the models with the following number of trees: 200, 1e3, 1e4, 1e6. To avoid the folly of overfitting, it is best to tune the model using cross-validation. In the code that follows, we will tune a gradient boosting model using interaction depth, shrinkage and minobsinnode.

library(caret)
set.seed(617)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 500,
           interaction.depth = c(1,2,3), 
           shrinkage = (1:100)*0.001, # or shrinkage = seq(1e-3, 1, by = .1)
           n.minobsinnode=c(5,10,15))

garbage = capture.output(cvModel <- train(earn~., data=train,
    method="gbm", trControl=trControl, tuneGrid=tuneGrid))

cvBoost = gbm(earn~., data=train,
              distribution="gaussian", n.trees=cvModel$bestTune$n.trees, interaction.depth=cvModel$bestTune$interaction.depth, shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)
pred = predict(cvBoost,test,n.trees=500)
rmse_cv_boost = sqrt(mean((pred-test$earn)^2)); rmse_cv_boost

# boosting with xgboost

#XGBoost is an optimized distributed gradient boosting library designed to be highly efficient, flexible and portable.
#The algorthim is also a bit picky about the format of variables used. All factor class variables need to be dummy coded and fed into the model as a matrix. To do this, we will dummy code using library(vtreat)

install.packages("vtreat")
library(vtreat)
trt = designTreatmentsZ(dframe = train,
                        varlist = names(train)[2:6])
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']
newvars

train_input = prepare(treatmentplan = trt, dframe = train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt,
         dframe = test, varRestriction = newvars)
head(train_input)

#use cross validation to identify optimarl nrounds
library(xgboost)
library(caret)
set.seed(617)


tune_nrounds = xgb.cv(data=as.matrix(train_input),
                      label = train$earn, nrounds=250,
                      nfold = 5,
                      verbose = 0)

ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean))+ geom_point(size=0.4, color='sienna')+
  geom_line(size=0.1, alpha=0.1)+
  theme_bw()

# from graph, optimal nrounds is small
which.min(tune_nrounds$evaluation_log$test_rmse_mean)

# next we use xgboost to fit the train data wih nrounds = 6 and apply model to test data
xgboost2= xgboost(data=as.matrix(train_input), label = train$earn,
                  nrounds=6,
                  verbose = 0)
pred = predict(xgboost2,
               newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred - test$earn)^2)); rmse_xgboost


#data.frame(models = c('Tree', 'Maximal Tree', 'Tuned Tree', 'Bag','Forest','Tuned Forest','Ranger','Tune') 
#RMSE = c(rmse_tree, rmse_maximalTree, rmse_cvTree, rmse_bag, rmse_forest, rmse_cv_forest, rms
 #                     rmse_cv_forest_ranger, rmse_boost, rmse_cv_boost, rmse_xgboost))


############# Advanced trees: cross validation with S&P data

install.packages("ISLR")
library(ISLR)

str(Smarket)

# lots of the categorical variables are useless, 

train$amenities


names(train)

library(ggplot2)
head(diamonds)
library(tidyverse)
ideal <- diamonds %>% subset(diamonds$cut == "Ideal")
head(ideal)

var(ideal$carat)


library(caret)
head(diamonds)
set.seed(61710)
split = createDataPartition(y = diamonds$price, p = 0.7, list = F, groups = 50)

#Once we get the split vector, we proceed in the same manner as for random sampling

train = diamonds[split,]
test = diamonds[-split,]

library(caTools)
head(OJ)
set.seed(1706)
split = sample.split(Y = OJ$Purchase, SplitRatio = 0.8)
table(split)
train = OJ[split,]
test = OJ[!split,]
nrow(train)
nrow(test)
table(test$Purchase)
table(train$Purchase)
prop.table(rbind(train = table(train$Purchase), test = table(test$Purchase)), margin = 1)


head(sacramento)



########### 
# high performance computing module 11


# speed
x = sample(x = 1:10,size = 1e6,replace=T)
system.time(sample(x = 1:10,size = 1e6,replace=T))


s1 = function(n) sum(as.numeric(1:n))

system.time(s1(1e8))

s2 = function(n) sum(seq(1,n,1))

system.time(s2(1e8))

s3 = function(n) {
  sum = 0
  for (i in 1:n){
    sum = sum+i
  }
  return(sum)
}

system.time(s3(1e8))

#Note, if you would like to not only time but also save the result, then run (using arrow assignment rather than =)

system.time(s1_result <- s1(1e8))



#microbenchmark
#One of the problems with using system.time, is that every time you run it the time is different! So, it makes sense to average a few runs. Also, comparing multiple functions is harder with system.time(). microbenchmark makes it easier to compare multiple functions running them multiple times and running summary statistics on time taken.

install.packages("microbenchmark")
library(microbenchmark)
microbenchmark(sample(x = 1:10,size = 1e6,replace=T),times = 10)
microbenchmark(s1(1e8),s2(1e8),s3(1e8),times=10)


#rbenchmark  makes it possible to find time from multiple replications
install.packages("rbenchmark")

library(rbenchmark)
benchmark(sample(x = 1:10,size = 1e6,replace=T),replications = rep.int(1,10))
microbenchmark(sample(x = 1:10,size = 1e6,replace=T),times = 10)


#Spotting bottlenecks
#code profiling is a way of monitioirng step by step execution of code to identify bottlenecks

Rprof('profiling.out') # Start Profiling
x1 = sample(1:10,size = 10000,replace = T)
x2 = sample(1:10,size = 10000,replace = T)
x3 = sample(1:10,size = 10000,replace = T)
y = rnorm(10000,mean = 100,sd = 5)
data = data.frame(x1,x2,x3,y)
reg = lm(y~.,data)
library(rpart)
tree = rpart(y~.,data)
library(randomForest)


bag = randomForest(y~.,data = data,ntree=500,mtry=1)
forest = randomForest(y~.,data = data,ntree=100)
forest = randomForest(y~.,data = data,ntree=500)
forest = randomForest(y~.,data = data,ntree=1000)
library(gbm)


boost = gbm(y~.,data,distribution = 'gaussian',n.trees = 500,interaction.depth = 1,n.minobsinnode = 5)
Rprof(NULL)         # Stop Profiling
summaryRprof('profiling.out')  # Print profiling information


# profvis - Another alternative is the profvis which is RStudio friendly, presents results in an interactive chart, and checks for memory utilization.
install.packages("profvis")
library(profvis)
profvis({
  x1 = sample(1:10,size = 10000,replace = T)
  x2 = sample(1:10,size = 10000,replace = T)
  x3 = sample(1:10,size = 10000,replace = T)
  y = rnorm(10000,mean = 100,sd = 5)
  data = data.frame(x1,x2,x3,y)
  reg = lm(y~.,data)
  library(rpart)
  tree = rpart(y~.,data)
  library(randomForest)
  bag = randomForest(y~.,data = data,ntree=500,mtry=1)
  forest = randomForest(y~.,data = data,ntree=100)
  forest = randomForest(y~.,data = data,ntree=500)
  forest = randomForest(y~.,data = data,ntree=1000)
  library(gbm)
  boost = gbm(y~.,data,distribution = 'gaussian',n.trees = 500,interaction.depth = 1,n.minobsinnode = 5)
})

# Memory utilization
#The benefits of lighting fast speed that come from in-memory processing comes with the challenge of limits on how much data can be processed. There are three tools to examine memory usage.

#Object size
# Base R function object.size can be used to determine object size. But, there are situations where pryr::object_size() is more accurate.

x = runif(1e7, 0 ,10)
object.size(x)
object.size(as.double(x))
object.size(as.numeric(x))
install.packages("pryr")
#ronuding doesnt change storage type
head(floor(x))
object.size(floor(x))

# casing as an integer does 
object.size(as.integer(x))

as.integer(head(x))

#single precision floats requires the float packages
install.packages("float")
library(float)
object.size(float::fl(x))


library(pryr)

object_size(x)
pryr::object_size(float::fl(x))
gc()

#rprof - note that rprof does not account for garbage collector that constantly releases memory.  Hency rprof may inflate memory useage estimates
Rprof('memory.out',memory.profiling=T) # Start Profiling
x1 = sample(1:10,size = 10000,replace = T)
x2 = sample(1:10,size = 10000,replace = T)
x3 = sample(1:10,size = 10000,replace = T)
y = rnorm(10000,mean = 100,sd = 5)
data = data.frame(x1,x2,x3,y)
reg = lm(y~.,data)
library(rpart)
tree = rpart(y~.,data)
library(randomForest)
bag = randomForest(y~.,data = data,ntree=500,mtry=1)
forest = randomForest(y~.,data = data,ntree=100)
forest = randomForest(y~.,data = data,ntree=500)
forest = randomForest(y~.,data = data,ntree=1000)
library(gbm)
boost = gbm(y~.,data,distribution = 'gaussian',n.trees = 500,interaction.depth = 1,n.minobsinnode = 5)
Rprof(NULL)         # Stop Profiling
summaryRprof('memory.out',memory = 'both')  # Print profiling information


#profvis
library(profvis)

profvis({
  x1 = sample(1:10,size = 10000,replace = T)
  x2 = sample(1:10,size = 10000,replace = T)
  x3 = sample(1:10,size = 10000,replace = T)
  y = rnorm(10000,mean = 100,sd = 5)
  data = data.frame(x1,x2,x3,y)
  reg = lm(y~.,data)
  library(rpart)
  tree = rpart(y~.,data)
  library(randomForest)
  bag = randomForest(y~.,data = data,ntree=500,mtry=1)
  forest = randomForest(y~.,data = data,ntree=100)
  forest = randomForest(y~.,data = data,ntree=500)
  forest = randomForest(y~.,data = data,ntree=1000)
  library(gbm)
  boost = gbm(y~.,data,distribution = 'gaussian',n.trees = 500,interaction.depth = 1,n.minobsinnode = 5)
})

##### 
install.packages("benchmarkme")
library(benchmarkme)

get_ram()

# cpu
speedResult = benchmark_std(runs = 3)
plot(speedResult)

#i/o how fast you can read data off disk
speed_io = benchmark_io(runs=3,size = 50)
plot(speed_io)



# Vectorization (over loops)  --- this is optimizing code
#Vectorized operations involve applying a function once to an entire vector. On the other hand, a loop will apply the function to each element n times.

#Loop
num = sample(1:10,size=1e7,replace=T)
num_square = integer(length(num))

system.time(for (i in 1:length(num)){  #index num, square it
  num_square[i] = num[i]^2
})

# Vectorized ## this is faster - becasue above, they are using precompiled c code and running a loop
system.time(num_squared <- num^2)

# Use built in functions 
#Many R functions and packages are implemented in compiled languages like C/C++ These will always run faster than functions written in R, an interpreted language

data = sample(1:10,size = 1e7,replace=T)
dim(data) = c(100000,100)

#use built in fuctions (also precompiled)
system.time(apply(X=data,MARGIN = 2,FUN = sum))

system.time(colSums(data))

library(microbenchmark)
microbenchmark(apply = apply(X=data,MARGIN = 2,FUN = sum),colSums = colSums(data))



library(ggplot2)
library(dplyr)
str(diamonds)

system.time(aggregate(price~cut, diamonds, mean))
system.time(tapply(diamonds$price, diamonds$cut, mean))
system.time(diamonds %>%
              group_by(cut) %>%
              summarize(price = mean(price)))

#dont over optimze 

library(data.table)
dt = data.table(data)
system.time(dt[,mean(price),by='cut'])

microbenchmark(aggregate = aggregate(price~cut,data,mean),
               tapply = tapply(data$price,data$cut,mean),
               dplyr = data%>% group_by(cut)%>% summarize(price=mean(price)),
               data.table = dt[,mean(price),by='cut'],times = 5)


## preallocate memory
# In programming languages like C, C++, or Java, a vector (or array) has to be declared prior to its use. Declaring in effect preallocates memory space.

#In R, this happens automatically. But, if the memory allocated is not large enough, then R will have to create a larger space and move the data to the larger memory space. This reallocation based on need can slow things down, especially if it is done repeatedly for each additional element. Manually allocating memory can save R some time.


# not preallocated
numbers = sample(1:10,size=1e6,replace=T)
num_square = 
  function(num){
    num_square = integer()  #this basically copies each vector or some shit
    for (i in 1:length(num)){
      num_square[i] = num[i]^2
    }
  }
system.time(num_square(numbers))


#allocated 
num_square_preallocated = 
  function(num){
    num_square = integer(length(num))
    for (i in 1:length(num)){
      num_square[i] = num[i]^2
    }
  }
system.time(num_square_preallocated(numbers))


microbenchmark(num_square(numbers),num_square_preallocated(numbers),times = 5)

numbersX10 = sample(1:10,size=1e7,replace=T)
numbersX50 = sample(1:10,size=5e7,replace=T)

microbenchmark(num_square(numbers),num_square_preallocated(numbers),
               num_square(numbersX10),num_square_preallocated(numbersX10),
               num_square(numbersX50),num_square_preallocated(numbersX50), times = 5)

s3 = function(n) {
  sum = numeric()
  for (i in 1:n){
    sum = sum+i
  }
  sum
}
system.time(s3(1e6))

s3 = function(n) {
  sum = numeric()
  for (i in 1:n){
    sum = sum+i
  }
  sum
}
system.time(s3(1e6))


microbenchmark(s3(1e6),s3_preallocate(1e6),times = 10)



### Use simpler data structures 
# For instance, if all the data is of the same class, it is better to use a matrix rather than data.frame. Also, data.table may not always be the fastest
mat = sample(1:10,size = 1e8,replace=T)
dim(mat) = c(100000,1000)
system.time(colSums(mat))

df = data.frame(mat)
system.time(colSums(df))

microbenchmark(matrix = colSums(mat),data.frame = colSums(df),times = 10)

microbenchmark(matrix = mat[,100],   data.frame = df[,100],times=10) # Subsetting col 100


microbenchmark(matrix = mat[100,],   data.frame = df[100,],times=10) # Subsetting row 100

microbenchmark(matrix = mat[100,100],data.frame = df[100,100],times=10) # Subsetting row 100, col 100

library(dplyr)
library(data.table)
dt = data.table(mat)
system.time(colSums(dt))

microbenchmark(matrix = colSums(mat),data.frame = colSums(df),data.table = colSums(dt), dplyr = colSums(dplyr_df),times = 10)
microbenchmark(matrix = mat[100,100],data.frame = df[100,100],data.table = dt[100,100],dplyr = dplyr_df[100,100],times=10) # Subsetting row 100, col 100

## use hash tables 
#For frequent lookups on large data, it is better to use Hash tables


data = rnorm(1E4)
data_ls = as.list(data)
names(data_ls) = paste("V", c(1:1E4), sep="")
index_rand = sample(1:1E4, size=1000, replace=T)
index = paste("V", index_rand, sep="")
list_comptime = sapply(index, FUN=function(x){
  system.time(data_ls[[x]])[3]})
sum(list_comptime)

library(hash)
data_h = hash(names(data_ls), data)
hash_comptime = sapply(index, FUN=function(x){
  system.time(data_h[[x]])[3]})
sum(hash_comptime)


### use faster more efficent pacakges

#In this example, you will note that fastcluster gives better performance than the clustering algorithm in the stats package. On the other hand, Rcppeigen did not do any better than lm.

data = rnorm(1e4*100)
dim(data) = c(1e4,100)
dist_data = dist(data)

system.time(hclust(dist_data))

library(fastcluster)
system.time(hclust(dist_data))

data = rnorm(10000*100)
dim(data) = c(10000,100)
#princomp(data)
#prcomp(data)
microbenchmark(princomp(data),prcomp(data),times=5)
library(RcppEigen)
x = model.matrix(price~.-1,data)
y = data$price

fast = function(dataframe){
  library(RcppEigen)
  x = model.matrix(price~.-1,data)
  y = data$price
  fastLm(x,y)
}

microbenchmark(lm(price~.,data),fast(data),times=5)


#precomplie
model = function(x, y){
  data = data.frame(x,y)
  model = lm(y~x, data)
}

a = sample(1:10, 1e6, replace= T)
b = sample(1:10, 1e6, replace= T)

install.packages("compiler")
library(compiler)


library(Rcpp)

sumR <- function(x){
  total <- 0
  for (i in x) {
    total <- total+i
    
  }
  return(total)
}
0p;


#Mod 10: SVM

#Support vector machines

#Linearly separable
#The following simulated data illustrated a case of linearly seperable classes
set.seed(0617)
data = data.frame(x1=rnorm(100),x2=rnorm(100))
data$y = factor(ifelse(data$x1>data$x2,0,1))
data = data[abs(data$x1-data$x2)>0.2,]

library(ggplot2)
ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)

#Which such data, it is possible to find a linear classifer or a hyperplane
ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)

#n fact, there are a very large number of hyperplanes that can separate the classes. So, the decision boundary chosen is the one that has the biggest margin and is accordingly called Maximum Margin Classifier.
ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)


#Not linearly seperable
#in practice, classes are selom linearly seperable 
set.seed(0617)
data = data.frame(x1=rnorm(100),x2=rnorm(100))
data$y = factor(ifelse(data$x1>data$x2,0,1))
data$y[abs(data$x1-data$x2)<0.5] = factor(sample(c(0,1),size = length(data$y[abs(data$x1-data$x2)<0.5]),replace = T))
ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()

#So, the requirement of a hard margin is relaxed. Instead, the support vector classifier maximizes a soft margin.

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)

#Of course, many soft margins are possible. HEre is another


ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = 0.4,color='violet', size=1)


#The soft margin used depends on the Cost (C). Higher the cost, narrower the margins. The cost can be determined by the analyst but in practice an SVM model is tuned to determine the optimal cost parameter.

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = -0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = 0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = -0.8,color='purple', size=1)+
  geom_abline(slope = 1,intercept = 0.8,color='purple', size=1)


#Support vector machine models (linear)
#Let us now examine the type of support vectors fitted by an SVM model to the simulated data we were using above. However, this time we will train the model on a subset of the data.

set.seed(0617)
data = data.frame(x1=rnorm(100),x2=rnorm(100))
data$y = factor(ifelse(data$x1>data$x2,0,1))
set.seed(0617)
split = sample(1:nrow(data),0.7*nrow(data))
train = data[split,]
test = data[-split,]


#SVM: Cost = 1
#Fit an SVM model using the default cost of 1. In practice, variables are scaled, but here it has been set to F so that the upcoming graphs are more meaningful.

library(e1071)
svmLinear = svm(y~.,train,kernel='linear',scale=F,type='C-classification') # if outcome is a factor, default type='C-classification'
summary(svmLinear)

#Plot the Decision boundary. Note, svm comes with a plot function that constructs a decision boundary but we are manually constructing it here to draw a comparison with the charts above.
beta = t(svmLinear$coefs) %*% svmLinear$SV
slope = -beta[1]/beta[2]
intercept = svmLinear$rho/beta[2]
ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)

#plot with margins 
ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)+
  geom_abline(slope = slope,intercept = intercept-1/beta[2],color='rosybrown', size=1)+  
  geom_abline(slope = slope,intercept = intercept+1/beta[2],color='rosybrown', size=1)

#here is the plot that comes with svm()
plot(svmLinear, train)

#Finally, let us examine the performance of the svm on train and test samples

pred = predict(svmLinear)
table(pred,train$y)
pred = predict(svmLinear,newdata=test)
table(pred,test$y)


## SVM: cost = 100
library(e1071)
svmLinear = svm(y~.,train,kernel='linear',scale=F,type='C-classification',cost=100) # if outcome is a factor, default type='C-classification'
beta = t(svmLinear$coefs) %*% svmLinear$SV
slope = -beta[1]/beta[2]
intercept = svmLinear$rho/beta[2]

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)
summary(svmLinear)


#higher the cost, narrower the margins
ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)+
  geom_abline(slope = slope,intercept = intercept-1/beta[2],color='rosybrown', size=1)+  
  geom_abline(slope = slope,intercept = intercept+1/beta[2],color='rosybrown', size=1)

#Now, we can compare performance of this SVM with the one with a lower cost
pred = predict(svmLinear)
table(pred,train$y)

pred = predict(svmLinear,newdata=test)
table(pred,test$y)

## SVM: tune for best cost
#The best way to figure out the cost parameter is to tune the model.

svmTune = tune(method = svm,y~.,data=train,kernel='linear', type='C-classification', scale=F, ranges = list(cost=c(0.01,0.1,1, 10, 100)))
svmTune$best.model

pred = predict(svmTune$best.model,newdata=test)
table(pred,test$y)
plot(svmTune$best.model,test)

##Note, the examples above looked at binary classification. SVM can easily be extended to more than two categories and can also be used for regression-type problems.


## Support vector machines: Polynomial:
#In pracice, a linear boundary may fail, in which case support vector machines are able to function by enriching and enlarging the feature space to make separation possible. This is also known as the “Kernel Trick”

#Let us first examine a dataset where a linear classifier is unlikely to succeed. 

set.seed(0617)
data = data.frame(x1=runif(200,-1,1),x2=runif(200,-1,1))
radius = .8
radius_squared = radius^2
data$y <- factor(ifelse(data$x1^2+data$x2^2<radius_squared, 0, 1))
split = sample(1:nrow(data),0.7*nrow(data))
train = data[split,]
test = data[-split,]

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = -1,intercept = 0.64,color='cadetblue', size=1)

library(e1071)
svmLinear = svm(y~.,data = train, kernel='linear',scale=F,type='C-classification')
pred = predict(svmLinear)
mean(pred==train$y)

pred = predict(svmLinear,newdata=test)
mean(pred==test$y)

plot(svmLinear,train)



# now let us try a SVM with a polynomial kernel
svmPoly = svm(y~.,data = train, kernel='polynomial',scale=F,type='C-classification',degree=2)
pred = predict(svmPoly)
mean(pred==train$y)

pred = predict(svmPoly,newdata=test)
mean(pred==test$y)

plot(svmPoly,train)

#But, it is possible that the default parameters chosen aren’t optimal, so let us tune this model

tune_svmPoly = tune(method = svm,y~.,data = train,kernel='polynomial',
                    ranges= list(degree=c(2,3), cost = c(0.01, 0.1, 1), gamma=c(0,1,10), coef0=c(0,0.1,1,10)))
summary(tune_svmPoly)

pred = predict(tune_svmPoly$best.model)
mean(pred==train$y)

pred = predict(tune_svmPoly$best.model,newdata=test)
mean(pred==test$y)

plot(tune_svmPoly$best.model,train)


# Support vector machines -radial basis function

#In practice, the Radial Basis function performs better than either Linear or Polynomial kernels as it can fit a variety of decision boundaries.

svmRadial = svm(y~.,data = train, kernel='radial',scale=F,type='C-classification')
pred = predict(svmRadial)
mean(pred==train$y)

pred = predict(svmRadial,newdata=test)
mean(pred==test$y)
plot(svmRadial,train)


#lets tune the model
tune_svmRadial = tune(method='svm',y~.,data=train,kernel='radial', type='C-classification',
                      ranges=list(cost=c(0.1,10,100), gamma=c(1,10), coef0 = c(0.1,1,10)))
summary(tune_svmRadial$best.model)

pred = predict(tune_svmRadial$best.model)
mean(pred==train$y)

pred = predict(tune_svmRadial$best.model,newdata=test)
mean(pred==test$y)

plot(tune_svmRadial$best.model,test)

## illustration with wine dataset

#Now, let’s use SVM to predict wine quality. First, we are going to convert the quality variable from the Wine data into a binary variable.

#Read in the wine quality data using the following code: data = read.csv(‘winequality-white.csv,sep=’;’)

data = read.csv('winequality-white.csv', sep = ';')
data$quality = factor(ifelse(data$quality>mean(data$quality), 1, 0),labels = c('high','low'))

library(caTools)
set.seed(1706)
split = sample.split(data$quality,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

#We are now going to compare performance of Tree and a set of SVMs to predict wine quality using only alcohol and volatile.acidity

#tree model
library(rpart)
tree = rpart(quality~alcohol+volatile.acidity,train,method='class')
pred = predict(tree,newdata=test,type = 'class')
table(pred,test$quality)
mean(pred==test$quality)

#Linear SVM
svmLinear = svm(quality~alcohol+volatile.acidity,data = train,kernel='linear',type='C-classification')
summary(svmLinear)

pred = predict(svmLinear,newdata=test)
table(pred,test$quality)

mean(pred==test$quality)

plot(svmLinear,test[,c('quality','alcohol','volatile.acidity')]) # decision boundary looks non-linear because original variables were scaled

#Linear SVM Tuned
svmLinearTune = tune(method = svm,quality~alcohol+volatile.acidity,data=train,kernel='linear',type='C-classification',ranges = list(cost=c(0.01, 0.1,1,10,100)))
summary(svmLinearTune$best.model)
pred = predict(svmLinearTune$best.model,newdata=test)
table(pred,test$quality)
mean(pred==test$quality)
plot(svmLinearTune$best.model,test[,c('quality','alcohol','volatile.acidity')])

#polynomial SVM
svmPolynomial = svm(quality~alcohol+volatile.acidity,data = train,kernel='polynomial',degree=2,type='C-classification')
summary(svmPolynomial)
pred = predict(svmPolynomial,newdata=test)
table(pred,test$quality)
mean(pred==test$quality)
plot(svmPolynomial,test[,c('quality','alcohol','volatile.acidity')])

#polynomial SVM - tuned
svmPolynomialTune = tune(method = svm,quality~alcohol+volatile.acidity,data=train,kernel='polynomial',
                         ranges=list(cost=c(0.01,1,100),degree=c(2,3)))
summary(svmPolynomialTune$best.model)
svmPolynomialTune$best.parameters

pred = predict(svmPolynomialTune$best.model,newdata=test)
table(pred,test$quality)
mean(pred==test$quality)
plot(svmPolynomialTune$best.model,test[,c('quality','alcohol','volatile.acidity')])


#Radial SVM
svmRadial = svm(quality~alcohol+volatile.acidity,data = train,kernel='radial',type='C-classification')
summary(svmRadial)
pred = predict(svmRadial,newdata=test)
table(pred,test$quality)
mean(pred==test$quality)
plot(svmRadial,test[,c('quality','alcohol','volatile.acidity')])

#Radial SVM - tuned
svmRadialTune = tune(method=svm,quality~alcohol+volatile.acidity,data=train,kernel='radial',type='C-classification',
                     ranges = list(cost=c(0.1,10,100), gamma=c(1,10), coef0 = c(0.1,1,10)))
summary(svmRadialTune$best.model)
svmRadialTune$best.parameters
pred = predict(svmRadialTune$best.model,newdata=test)
table(pred,test$quality)
mean(pred==test$quality)
plot(svmRadialTune$best.model,test[,c('quality','alcohol','volatile.acidity')])






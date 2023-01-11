#### Assignment 2 Section 1 ####  2/25/2021

baby = read.csv("baby_reviews.csv")
head(baby)


#Question 1: How many reviews does this dataset contain
nrow(baby)
#4978



#Question 2: What is the average review rating
mean(baby$review_rating)
#4.2276


#Question 3: nchar() is a handy function for counting the number of characters in text.
#What is the average number of characters in a review?


ch = nchar(baby$review)
mean(ch)
#441.8248
mean(nchar(baby$review))


#Question 4: Examine the relationship between review length (measured by number of characters) and rating. 
#Now, indicate whether the following statement is True or False.
#Greater the length of the review, better the rating.

r_char = cor.test(nchar(baby$review), baby$review_rating)
corr = data.frame(r = c(r_char$estimate))
rownames(corr) = c('Charact')
corr

# -0.036 - FALSE


#Question 5:  The stringr library has a number of handy text search functions capable of both literal search and pattern matching. 
#The sample code that follows specifies a pattern to identify a word and str_count to count the number of such words in a set of text.
#find median number of words in review
library(stringr)

str_count(string = 'Hmm, how many words are in this sentence?',pattern = '\\S+')
summary(str_count(string = baby$review,pattern = '\\S+'))
#57



#Question 6:  how many words are in the longest review
summary(str_count(string = baby$review,pattern = '\\S+'))

#1041
#Question 7: how many words are in the shortest review
summary(str_count(string = baby$review,pattern = '\\S+'))
#2


#Question 8:   Which of the following words are in the Top 10 list?
library(tidytext)
library(tidyverse)
library(magrittr)
baby%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

#the, and

#Question 9 Now, let us construct a Top 10 list after excluding stop words. 
 

baby%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

#baby, easy, love


###############################################################################
#Section 2

#Question 1: What is the total number of words in all the reviews?



baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output = word,input=review)%>%
  ungroup()%>%
  count()

#421790


#Question 2:  Now, let us explore valence of the words used in reviews.
#Use the 'bing' dictionary to classify words in reviews into positive and negative. 
#The bing dictionary of words can be accessed using tidytext::get_sentiments('bing')
#or bing.csv file accompanying this assignment. What is the total number of positive words in all the reviews?


baby%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
#24462

#Question 3: what propostion of words are postive
totalbing = 8739 + 24461
positive = 24461
positive/totalbing
#0.736771

#or 
baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))



#Question 4:  of the 5 posssible review ratings, which had the highest prop of postive words


d <- 
  baby%>%
  group_by(id, review_rating)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(id, review_rating)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()

d %>%
  group_by(review_rating) %>%
summary(d)

#5

d %>%
  group_by(review_rating, id)%>%
  summarise(mean = proportion_positive)

five <- d %>%
  filter(review_rating==5)
mean(five$proportion_positive)

four <- d %>% 
filter(review_rating ==4)
mean(four$proportion_positive)

three <- d %>% 
  filter(review_rating ==3)
mean(three$proportion_positive)

two <- d %>% 
  filter(review_rating ==2)
mean(two$proportion_positive)

one <- d %>% 
  filter(review_rating ==1)
mean(one$proportion_positive)


# Question 5: Next, let us examine the emotions expressed in the reviews using the "nrc" dictionary.
#You can access the "nrc" dictionary from nrc.csv file accompanying this assignment. How many words reflect surprise?

library(lexicon)

hash_sentiment_nrc %>%
  group_by(y)%>%
  summarize(count= n())


nrc = get_sentiments('nrc')

nrc%>%
  group_by(sentiment)%>%
  count()

 baby%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  arrange(desc(n))

#3815

#question 6: anticpation
 baby%>%
   group_by(id)%>%
   unnest_tokens(output = word, input = review)%>%
   inner_join(nrc)%>%
   group_by(sentiment)%>%
   count()%>%
   arrange(desc(n))
 
 #8429
 
#Question 7 What is the minimum sentiment score? using affin
afinn = get_sentiments('afinn')

baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))
 
#min -3

#Question 8: Which of the following review ids have the lowest sentiment score?
baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==91)%>%
  summarize(reviewSentiment = mean(value))


baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==146)%>%
  summarize(reviewSentiment = mean(value))


baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==238)%>%
  summarize(reviewSentiment = mean(value))

baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==1432)%>%
  summarize(reviewSentiment = mean(value))

baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id==2598)%>%
  summarize(reviewSentiment = mean(value))

#91, 238, 2598

#Question 9: avg sent score for afinn

baby %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))

#1.38

#########################################################3
#Section 3
##Preprocess the corpus of reviews using functions from library(tm). Specifically,

#Create a corpus from the variable 'review'
#Use tm_map to
#(a) transform text to lower case,
#(b) remove punctuation,
#(c) remove English stopwords using the following dictionary tm::stopwords('english)
#(d) remove whitespace
#Create a dictionary
#Use tm_map to stem words
#Create a DocumentTermMatrix

library(tm)
corpus <- VCorpus(VectorSource(baby$review))
original_corpus <- corpus
corpus <- tm_map(corpus,FUN = content_transformer(tolower))
corpus <- tm_map(corpus, FUN = removePunctuation)
corpus <- tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus <- tm_map(corpus, FUN = stripWhitespace)

dtm <- DocumentTermMatrix(original_corpus)
dict <- findFreqTerms(dtm, lowfreq = 0)
## new corpus with single words
dict_corpus <- VCorpus(VectorSource(dict))
corpus <- tm_map(corpus,FUN = stemDocument)

dtm <- DocumentTermMatrix(corpus)



#Question 1: How many terms does the document term matrix contain?
(dtm)

#11730

#Question 2: Insepect document 100 of the doc term matrix. how many times does amazon appear in this doc
inspect(dtm[100, 'amazon'])

#1

#Question 3 Now, let us reduce the number of terms to a more reasonable number by only keeping terms that appear in at least 10% of documents. Save the result as 'xdtm'. How many terms remain after removing sparse terms?
xdtm <- removeSparseTerms(dtm,sparse = 0.90)
dim(xdtm)
xdtm

#47 

#Question 4: Transform the document term matrix, xdtm created in the previous question into a data frame. Use stemCompletion() to complete stemmed words by selecting the most prevalent match. In the resulting data frame, which term appears most frequently? (use, babies, one, like, or love)
xdtm <- as.data.frame(as.matrix(xdtm))
sort(colSums(xdtm),decreasing = T)
colnames(xdtm) <- stemCompletion(x = colnames(xdtm),
                                 dictionary = dict_corpus,
                                 type='prevalent')
colnames(xdtm) <- make.names(colnames(xdtm))
head(sort(colSums(xdtm),decreasing = TRUE))

#use

#Question 5: Attach the column containing the review rating to the dataframe created in the previous question. Which is the third (3rd) most frequently occurring word among reviews with a rating of 5?

review_data <- cbind(review_rating = baby$review_rating,xdtm)
head(sort(colSums(review_data %>% filter(review_rating == 5)),decreasing = TRUE))

#love

####################################################################3

#Now, let us use data on word frequencies to predict review rating. Split the dataset containing review rating and term frequencies into train and test samples. Use sample() to create a train sample with 70% of the data and a test sample with the remaining 30%. Use a seed of 1031. For a dataset called, baby_data, the following code will create the train and test samples.


#Section 4:  

#Question 1 #Now, let us use data on word frequencies to predict review rating. Split the dataset containing review rating and term frequencies into train and test samples. Use sample() to create a train sample with 70% of the data and a test sample with the remaining 30%. Use a seed of 1031. For a dataset called, baby_data, the following code will create the train and test samples.

#how many rows in the test sample

set.seed(1031)
split = sample(1:nrow(review_data),size = 0.7*nrow(review_data))
train = review_data[split,]
test = review_data[-split,]
nrow(test)

#1494

#Question 2:  Use a CART model to predict review_rating using all other variables, i.e., term frequencies. For the CART model, use rpart(). Now, indicate whether the following statement is True or False.

#Based on results of the CART model (and all else being equal), reviews that contain the term 'love' are rated higher than those that don't contain the term 'love'.'

library(rpart)
library(rpart.plot)

tree_tdf <- rpart(review_rating~.,train)
#par(mfrow=c(1,2))
rpart.plot(tree_tdf)

#True


#Question 3:  Based on results of the CART model, reviews that contain the term 'easier' are rated lower than those that don't contain the term 'easier'.

#False

#Question 4: Based on results of the CART model, reviews that contain the term 'perfect' are rated lower than those that don't contain the term 'perfect'.

#False

#Question 5:Use a linear regression to predict review_rating using all other variables, i.e., term frequencies.

#Examine the results. From an earlier section, recall the most frequently occurring term in the document term matrix after removing sparse terms. Locate the most frequently occurring term in review in the regression results. Is this term predictive of review_rating?
reg_tdf <- lm(review_rating~.,train)
summary(reg_tdf) 

#no  

#Question 6: what is the RSME of the cart model on the test set?
sqrt(mean((predict(tree_tdf,newdata=test) - test$review_rating)^2))

#1.114328

#Question 7:  What is the RMSE of the linear regression on the test set?
sqrt(mean((predict(reg_tdf, newdata=test)-test$review_rating)^2))

#1.11626


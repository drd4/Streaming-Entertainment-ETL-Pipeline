

library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(plyr)
library(dplyr)
library(RColorBrewer)


api_key <- '3vyA6Z1us82mgvRoYUgHpRcZR'
api_secret_key <- 'lWyu9vlE3FAFbDLIxyE1DjVG7TElCdZmExpkiQPb2ZnW2pCk6u'
access_token <- '3338481857-PsrafNt90CqcfLcItlR2pGL6q8rU3bSA1k5PCqR'
access_token_secret <- 'ieOst0ZCmRYZU3pviOQ8zeql2TTegmno7q6EfnqjbB0Ty'   


### Set API Keys 
setup_twitter_oauth(api_key, api_secret_key, access_token,access_token_secret)


###Grab Latest Tweets 


#Pulling ElonMusk Tweets from Twitter API
tweets_elon <- searchTwitter("ElonMusk", n = 3000)
#Convert Tweets into DF
tweets.text_elon = twListToDF(tweets_elon)
tweets.text_elon = tweets.text_elon[,1]


#Pulling Gamestop Tweets from Twitter API
tweets_gamestop <- searchTwitter("Gamestop", n = 3000)
#Convert Tweet into DF
tweets.text_gamestop = twListToDF(tweets_gamestop)
tweets.text_gamestop = tweets.text_gamestop[,1]


#Pulling BItcoin Tweets from Twitter API
tweets_Bitcoin <- searchTwitter("Bitcoin", n = 3000)
#Convert Tweet into DF
tweets.text_Bitcoin = twListToDF(tweets_Bitcoin)
tweets.text_Bitcoin = tweets.text_Bitcoin[,1]


#Pulling Wallstreetbets buzz word Tweets from Twitter API
tweets_wallstreetbets <- searchTwitter("wallstreetbets", n = 3000)
#Convert Tweet into DF
tweets.text_wallstreetbets = twListToDF(tweets_wallstreetbets)
tweets.text_wallstreetbets = tweets.text_wallstreetbets[,1]


#Pulling tothemoon buzz word Tweets from Twitter API
tweets_tothemoon <- searchTwitter("tothemoon", n = 3000)
#Convert Tweet into DF
tweets.text_tothemoon = twListToDF(tweets_tothemoon)
tweets.text_tothemoon = tweets.text_tothemoon[,1]

############### Does not work ########################
#Pulling Shortingstock buzz word Tweets from Twitter API
#tweets_shortingstock <- searchTwitter("Shortingstock", n = 3000)
#Convert Tweet into DF
#tweets.text_shortingstock = twListToDF(tweets_shortingstock)
#tweets.text_shortingstock = tweets.text_shortingstock[,1]


#Pulling Reddit Tweets from Twitter API
tweets_reddit <- searchTwitter("reddit", n = 3000)
#Convert Tweet into DF
tweets.text_reddit = twListToDF(tweets_reddit)
tweets.text_reddit = tweets.text_reddit[,1]


#Pulling Dave Tweets from Twitter API
tweets_daveportnoy <- searchTwitter("stoolpresidente", n = 3000)
#Convert Tweet into DF
tweets.text_daveportnoy = twListToDF(tweets_daveportnoy)
tweets.text_daveportnoy = tweets.text_daveportnoy[,1]


#Pulling GME Tweets from Twitter API
tweets_GME <- searchTwitter("$GME", n = 3000)
#Convert Tweet into DF
tweets.text_GME = twListToDF(tweets_GME)
tweets.text_GME = tweets.text_GME[,1]

#Pulling Trump Tweets from Twitter API
tweets_trump <- searchTwitter("TheRealDonaldTrump", n = 37)

#Comments 
#3k tweets requested but API can only return 37 

#Convert Tweet into DF
tweets.text_trump = twListToDF(tweets_trump)
tweets.text_trump = tweets.text_trump[,1]

###Create a corpus
#Text Corpus Elon 
tweet.corpus_elon = VCorpus(VectorSource(tweets.text_elon))
#Text Corpus Bitcoin
tweet.corpus_Bitcoin = VCorpus(VectorSource(tweets.text_Bitcoin))
#Text Corpus Gamestop
tweet.corpus_gamestop = VCorpus(VectorSource(tweets.text_gamestop))
#Text Corupus To The Moon
tweet.corpus_tothemoon = VCorpus(VectorSource(tweets.text_tothemoon))
#Text Corupus Wall Street Bets
tweet.corpus_wallstreetbets = VCorpus(VectorSource(tweets.text_wallstreetbets))
#Text Corupus Dave Portnoy 
tweet.corpus_daveportnoy = VCorpus(VectorSource(tweets.text_daveportnoy))
#Text Corupus $GME
tweet.corpus_GME = VCorpus(VectorSource(tweets.text_GME))
#Text Corupus Reddit
tweet.corpus_reddit = VCorpus(VectorSource(tweets.text_reddit))
#Text Corpus Trump
tweet.corpus_trump = VCorpus(VectorSource(tweets.text_trump))




###Create a corpus
#Text Corpus Elon 
tweet.corpus_elon = Corpus(VectorSource(tweets.text_elon))
#Text Corpus Bitcoin
tweet.corpus_Bitcoin = Corpus(VectorSource(tweets.text_Bitcoin))
#Text Corpus Gamestop
tweet.corpus_gamestop = Corpus(VectorSource(tweets.text_gamestop))
#Text Corupus To The Moon
tweet.corpus_tothemoon = Corpus(VectorSource(tweets.text_tothemoon))
#Text Corupus Wall Street Bets
tweet.corpus_wallstreetbets = Corpus(VectorSource(tweets.text_wallstreetbets))
#Text Corupus Dave Portnoy 
tweet.corpus_daveportnoy = Corpus(VectorSource(tweets.text_daveportnoy))
#Text Corupus $GME
tweet.corpus_GME = Corpus(VectorSource(tweets.text_GME))
#Text Corupus Reddit
tweet.corpus_reddit = Corpus(VectorSource(tweets.text_reddit))
#Text Corpus Trump
tweet.corpus_trump = Corpus(VectorSource(tweets.text_trump))



###Remove Certain Characters and Words 
#https://github,com/raredd/regex
#http://www.gnu.org/sogtware/grep/manual/html_node/Character-Classes-and-Bracket-Express.html
tweet.removeURL = function(x)gsub("http[^[:space:]]*","",x)
tweet.removeATUser = function(x) gsub("@[a-z,A_Z]*","",x)
tweet.removeEmoji = function(x)gsub("\\p{So}|\\p{Cn}","",x,perl = TRUE)
tweet.removeSpecialChar = function(x)gsub("[^[:alnum:]///']","",x)


#Bitcoin
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, content_transformer(tweet.removeURL))
inspect(tweet.corpus_Bitcoin[1:10])
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_Bitcoin[1:10])
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_Bitcoin[1:10])
##tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, content_transformer(tweet.removeSpecialChar))
inspect(tweet.corpus_Bitcoin[1:10])

tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, removePunctuation, preserve_intra_word_dashes = TRUE)
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, content_transformer(tolower))


#words like "AND: or "the" are removed.- Bitcoin
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, removeWords, c(stopwords("english"), "RT", "rt", "and", "a", "the"))
tweet.corpus_Bitcoin= tm_map(tweet.corpus_Bitcoin, removeNumbers)
tweet.corpus_Bitcoin = tm_map(tweet.corpus_Bitcoin, stripWhitespace)
inspect(tweet.corpus_Bitcoin[1:10])



#Bitcoin
bit.ap <- TermDocumentMatrix(tweet.corpus_Bitcoin)
Bitcoin <- as.matrix(bit.ap)
dim(Bitcoin)
Bitcoin.v <- sort(rowSums(Bitcoin),decreasing = TRUE)
Bitcoin.d <- data.frame(word=names(Bitcoin.v),freq = Bitcoin.v)

View(Bitcoin)   #Word Count per entry
View(Bitcoin.d) #Word frequency -> Data needed word frequency



#Dave
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, content_transformer(tweet.removeURL))
inspect(tweet.corpus_daveportnoy[1:10])
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_daveportnoy[1:10])
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_daveportnoy[1:10])

##tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, content_transformer(tweet.removeSpecialChar))
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_daveportnoy[1:10])
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, content_transformer(tolower))
inspect(tweet.corpus_daveportnoy[1:10])


#words like "AND: or "the" are removed.- daveportnoy
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, removeWords, c(stopwords("english"), "RT", "rt","and","a", "the"))
tweet.corpus_daveportnoy= tm_map(tweet.corpus_daveportnoy, removeNumbers)
tweet.corpus_daveportnoy = tm_map(tweet.corpus_daveportnoy, stripWhitespace)
inspect(tweet.corpus_daveportnoy[1:10])



#Dave
dave.ap <- TermDocumentMatrix(tweet.corpus_daveportnoy)
Dave <- as.matrix(dave.ap)
dim(Dave)
Dave.v <- sort(rowSums(Dave),decreasing = TRUE)
Dave.d <- data.frame(word=names(Dave.v),freq = Dave.v)

View(Dave)
View(Dave.d)


#Elon
tweet.corpus_elon = tm_map(tweet.corpus_elon, content_transformer(tweet.removeURL))
inspect(tweet.corpus_elon[1:10])
tweet.corpus_elon = tm_map(tweet.corpus_elon, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_elon[1:10])
tweet.corpus_elon = tm_map(tweet.corpus_elon, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_elon[1:10])

##tweet.corpus_elon = tm_map(tweet.corpus_elon, content_transformer(tweet.removeSpecialChar))
tweet.corpus_elon = tm_map(tweet.corpus_elon, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_elon[1:10])
tweet.corpus_elon = tm_map(tweet.corpus_elon, content_transformer(tolower))
inspect(tweet.corpus_elon[1:10])
##############
################
#####
####
###
##
##
#
#
#
#
#words like "AND: or "the" are removed.- elon ############3
tweet.corpus_elon = tm_map(tweet.corpus_elon, removeWords, c(stopwords("english"), "RT", "rt", "a", "and", "the", "Ö", ))
tweet.corpus_elon= tm_map(tweet.corpus_elon, removeNumbers)
tweet.corpus_elon = tm_map(tweet.corpus_elon, stripWhitespace)
inspect(tweet.corpus_elon[1:10])


#Elon
elon.ap <- TermDocumentMatrix(tweet.corpus_elon)
Elon <- as.matrix(elon.ap)
dim(Elon)
elon.v <- sort(rowSums(Elon),decreasing = TRUE)
elon.d <- data.frame(word=names(elon.v),freq = elon.v)

View(Elon)
View(elon.d)




#Gamestop
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, content_transformer(tweet.removeURL))
inspect(tweet.corpus_gamestop[1:10])
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_gamestop[1:10])
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, content_transformer(tweet.removeEmoji))
##tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, content_transformer(tweet.removeSpecialChar))
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_gamestop[1:10])
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, content_transformer(tolower))
inspect(tweet.corpus_gamestop[1:10])



#words like "AND: or "the" are removed.- gamestop
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, removeWords, c(stopwords("english"), "RT", "rt", "a", "and", "the"))
tweet.corpus_gamestop= tm_map(tweet.corpus_gamestop, removeNumbers)
tweet.corpus_gamestop = tm_map(tweet.corpus_gamestop, stripWhitespace)
inspect(tweet.corpus_gamestop[1:10])


#Gamestop
gamestop.ap <- TermDocumentMatrix(tweet.corpus_gamestop)
Gamestop <- as.matrix(gamestop.ap)
dim(Gamestop)
gamestop.v <- sort(rowSums(Gamestop),decreasing = TRUE)
gamestop.d <- data.frame(word=names(gamestop.v),freq = gamestop.v)


View(Gamestop)
View(gamestop.d)




#GME
tweet.corpus_GME = tm_map(tweet.corpus_GME, content_transformer(tweet.removeURL))
inspect(tweet.corpus_GME[1:10])
tweet.corpus_GME = tm_map(tweet.corpus_GME, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_GME[1:10])
tweet.corpus_GME = tm_map(tweet.corpus_GME, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_GME[1:10])
##tweet.corpus_GME = tm_map(tweet.corpus_GME, content_transformer(tweet.removeSpecialChar))
tweet.corpus_GME = tm_map(tweet.corpus_GME, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_GME[1:10])
tweet.corpus_GME = tm_map(tweet.corpus_GME, content_transformer(tolower))
inspect(tweet.corpus_GME[1:10])


#words like "AND: or "the" are removed.- GME
tweet.corpus_GME = tm_map(tweet.corpus_GME, removeWords, c(stopwords("english"), "RT", "rt", "a", "and", "the"))
tweet.corpus_GME= tm_map(tweet.corpus_GME, removeNumbers)
tweet.corpus_GME = tm_map(tweet.corpus_GME, stripWhitespace)
inspect(tweet.corpus_GME[1:10])



#GME DocumentMatrix
gme.ap <- TermDocumentMatrix(tweet.corpus_GME)
GME <- as.matrix(gme.ap)
dim(GME)
gme.v <- sort(rowSums(GME),decreasing = TRUE)
gme.d <- data.frame(word=names(gme.v),freq = gme.v)

View(GME)
View(gme.d)




#Reddit
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, content_transformer(tweet.removeURL))
inspect(tweet.corpus_reddit[1:10])
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_reddit[1:10])
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_reddit[1:10])
##tweet.corpus_reddit = tm_map(tweet.corpus_reddit, content_transformer(tweet.removeSpecialChar))
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_reddit[1:10])
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, content_transformer(tolower))
inspect(tweet.corpus_reddit[1:10])


#words like "AND: or "the" are removed.- reddit
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, removeWords, c(stopwords("english"), "RT", "rt", "a", "and","the"))
tweet.corpus_reddit= tm_map(tweet.corpus_reddit, removeNumbers)
tweet.corpus_reddit = tm_map(tweet.corpus_reddit, stripWhitespace)
inspect(tweet.corpus_reddit[1:10])
  




#Reddit
reddit.ap <- TermDocumentMatrix(tweet.corpus_reddit)
Reddit <- as.matrix(reddit.ap)
dim(Reddit)
reddit.v <- sort(rowSums(Reddit),decreasing = TRUE)
reddit.d <- data.frame(word=names(reddit.v),freq = reddit.v)


View(Reddit)
View(reddit.d)  
  
  
#ToTheMoon
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, content_transformer(tweet.removeURL))
inspect(tweet.corpus_tothemoon[1:10])
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_tothemoon[1:10])
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_tothemoon[1:10])
##tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, content_transformer(tweet.removeSpecialChar))
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_tothemoon[1:10])
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, content_transformer(tolower))
inspect(tweet.corpus_tothemoon[1:10])


#words like "AND: or "the" are removed.- tothemoon
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, removeWords, c(stopwords("english"), "RT", "rt", "a","and", "the"))
tweet.corpus_tothemoon= tm_map(tweet.corpus_tothemoon, removeNumbers)
tweet.corpus_tothemoon = tm_map(tweet.corpus_tothemoon, stripWhitespace)
inspect(tweet.corpus_tothemoon[1:10])



#ToTheMoon
tothemoon.ap <- TermDocumentMatrix(tweet.corpus_tothemoon)
tothemoon <- as.matrix(tothemoon.ap)
dim(tothemoon)
tothemoon.v <- sort(rowSums(tothemoon),decreasing = TRUE)
tothemoon.d <- data.frame(word=names(tothemoon.v),freq = tothemoon.v)

View(tothemoon)
View(tothemoon.d)


#Trump
tweet.corpus_trump = tm_map(tweet.corpus_trump, content_transformer(tweet.removeURL))
inspect(tweet.corpus_trump[1:10])
tweet.corpus_trump = tm_map(tweet.corpus_trump, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_trump[1:10])
tweet.corpus_trump = tm_map(tweet.corpus_trump, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_trump[1:10])
##tweet.corpus_trump = tm_map(tweet.corpus_trump, content_transformer(tweet.removeSpecialChar))
tweet.corpus_trump = tm_map(tweet.corpus_trump, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_trump[1:10])
tweet.corpus_trump = tm_map(tweet.corpus_trump, content_transformer(tolower))
inspect(tweet.corpus_trump[1:10])


#words like "AND: or "the" are removed.- trump
tweet.corpus_trump = tm_map(tweet.corpus_trump, removeWords, c(stopwords("english"), "RT", "rt", "realdonaldtrump", "a","and","the"))
tweet.corpus_trump= tm_map(tweet.corpus_trump, removeNumbers)
tweet.corpus_trump = tm_map(tweet.corpus_trump, stripWhitespace)
inspect(tweet.corpus_trump[1:10])



#Trump
trump.ap <- TermDocumentMatrix(tweet.corpus_trump)
Trump <- as.matrix(trump.ap)
dim(Trump)
trump.v <- sort(rowSums(Trump),decreasing = TRUE)
trump.d <- data.frame(word=names(trump.v),freq = trump.v)

View(Trump)
View(trump.d)


#WallstreetBets
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, content_transformer(tweet.removeURL))
inspect(tweet.corpus_wallstreetbets[1:10])
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, content_transformer(tweet.removeATUser))
inspect(tweet.corpus_wallstreetbets[1:10])
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, content_transformer(tweet.removeEmoji))
inspect(tweet.corpus_wallstreetbets[1:10])

##tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, content_transformer(tweet.removeSpecialChar))
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, removePunctuation, preserve_intra_word_dashes = TRUE)
inspect(tweet.corpus_wallstreetbets[1:10])
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, content_transformer(tolower))
inspect(tweet.corpus_wallstreetbets[1:10])


#words like "AND: or "the" are removed.- wallstreetbets
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, removeWords, c(stopwords("english"), "RT", "rt", "a","and","the"))
tweet.corpus_wallstreetbets= tm_map(tweet.corpus_wallstreetbets, removeNumbers)
tweet.corpus_wallstreetbets = tm_map(tweet.corpus_wallstreetbets, stripWhitespace)
inspect(tweet.corpus_wallstreetbets[1:10])


#WallStreetBets
wallstreetbets.ap <- TermDocumentMatrix(tweet.corpus_wallstreetbets)
wallstreetbets <- as.matrix(wallstreetbets.ap)
dim(wallstreetbets)
wallstreetbets.v <- sort(rowSums(wallstreetbets),decreasing = TRUE)
wallstreetbets.d <- data.frame(word=names(wallstreetbets.v),freq = wallstreetbets.v)


View(wallstreetbets)
View(wallstreetbets.d)

#####################################################################

# Time Series Analysis #
library(tidyverse) 
library(corrplot)
rawdata = read.csv("rawbtc.csv")

btc_df = rawdata %>%
  select( PriceUSD, AdrActCnt, BlkCnt, BlkSizeByte,BlkSizeMeanByte, 
         CapMrktCurUSD, CapRealUSD, DiffMean, FeeMeanUSD, FeeMedUSD, 
         FeeTotUSD, HashRate, IssContPctAnn, IssContUSD, IssTotUSD, 
         ROI1yr, ROI30d, SplyCur, SplyFF, TxCnt, TxTfrCnt,
         TxTfrValAdjUSD, TxTfrValMeanUSD,TxTfrValMedUSD, TxTfrValUSD,
         VtyDayRet180d, VtyDayRet30d, VtyDayRet60d)



btc_df <- na.omit(btc_df)
str(btc_df)

numericVars <- which(sapply(btc_df, is.numeric)) #index numeric variables

all_numVar <- btc_df[, numericVars]


cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

cor_sorted <- as.matrix(sort(cor_numVar[,'PriceUSD'], decreasing = TRUE))

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.8)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

corrplot
btcc = btc_df %>%
  select(PriceUSD, AdrActCnt, BlkCnt, BlkSizeByte,BlkSizeMeanByte, 
         CapMrktCurUSD, CapRealUSD, DiffMean, FeeMeanUSD, FeeMedUSD, 
         FeeTotUSD, HashRate, IssContPctAnn, IssContUSD, IssTotUSD, 
         ROI1yr, ROI30d, SplyCur, SplyFF, TxCnt, TxTfrCnt,
         TxTfrValAdjUSD, TxTfrValMeanUSD,TxTfrValMedUSD, TxTfrValUSD,
         VtyDayRet180d, VtyDayRet30d, VtyDayRet60d)

round(cor(btcc), 3)


library(psych)
cortest.bartlett(cor(btcc), n= 30)

#Extract ElonMusk Tweets from Twitter API
tweets_elon <- searchTwitter("ElonMusk", n = 2430)
#Convert Tweets into DF
tweets.text_elon <- twListToDF(tweets_elon)


elon_df = tweets.text_elon

head(elon_df)

str(elon_df)

e_df = elon_df %>%
  select(id, text, created)

e_df

head(e_df)

str(e_df)

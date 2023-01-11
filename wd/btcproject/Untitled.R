data <- read.csv('rawbtc.csv', stringsAsFactors = FALSE)


#Extract only relevant data
btc_data <- data %>% 
  select(date, PriceUSD)

#Convert the date column into date data type
btc_data$date <- as.Date(btc_data$date)

#Convert the data into time-series data d d
btc_xts <- xts(btc_data[,'PriceUSD'], btc_data[,'date'])

#The price started to be volatile in 2017
btc_2017 <- btc_xts['20170101/20171231']
btc_2017_mon <- split(btc_2017, f = 'months')

#Find when the variance in price increased dramatically for the first time
lapply(btc_2017_mon,FUN=var) 

#Aug 2017 = starting point
btc_ts <- as.ts(btc_xts)
btc_ts_data <- window(btc_ts, start=2017-08-01)

#Check NA values in the selected time window
length(btc_ts_data) #2430 observations
sum(is.na(btc_ts_data)) #0
plot(btc_ts_data)

class(btc_ts_data)

#########


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

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#select only the high correlated variables 
reduced_df = btc_df %>%
  select(PriceUSD, CapMrktCurUSD, TxTfrValAdjUSD, IssContUSD, IssTotUSD,
         CapRealUSD, TxTfrValUSD)


#Split data




set.seed(617)
split <- sample(1:nrow(btc_df),size = 0.7*nrow(btc_df))
train <-btc_df[split,]
test<- btc_df[-split,]


linear = lm(PriceUSD ~ ., train)
summary(linear)

library(car)
vif(linear)

head(train)


library(mice)


clust = btc_df 

clust = scale(clust)

#Hierarchical Cluster via Euclidean Distance
d = dist(x = clust, method = 'euclidean')



clusters = hclust(d = d, method = 'ward.D2')



cor(cophenetic(clusters), d)
#0.542 moderate fit


#display trees over 20

plot(cut(as.dendrogram(clusters), h = 10)$upper)

library(factoextra)
library(gridExtra)
library(dendextend)

library(dendextend) #4 clusters
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))


#break up into 4 clusters
h_segments = cutree(tree= clusters, k =4)


#express clusters on scatterplot 
library(psych)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(clust,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(clust,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()
table(h_segments)


# PCA Visualizatoin 
library(cluster)
clusplot(clust,
         h_segments,
         color=T,shade=T,labels=4,lines=0,main='Hierarchical Cluster Plot')




#### K Means ###


#determine number of centers

#Total Within Sum of Squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = clust,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


#recommend 2-4 clusters


#Silhouette Plot

library(cluster)
silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = clust,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

set.seed(617)

km = kmeans(x = clust, centers = 4, iter.max = 10000, nstart = 25)

k_segments = km$cluster
table(k_segments)


#Visualize
library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(clust,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(clust,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()



library(cluster)
clusplot(clust,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')



#Model based clustering

library(mclust)



mclust_bic = sapply(1:10,FUN = function(x) -Mclust(clust,G=x)$bic)
mclust_bic


ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#going to stick with 4 clusters
m_clusters= Mclust(clust, G = 4)


m_segments = m_clusters$classification
table(m_segments)


#viZ

library(cluster)
clusplot(clust,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')



#profile clusters

data2 = cbind(btc_df, h_segments,k_segments, m_segments)
data2 %>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()





library(jsonlite)
library(tidyverse)


btc = fromJSON('bitcoin.json') %>% as.data.frame

btc

head(btc)[1:2]


#Read Raw data
d = read.csv("rawbtc.csv")
head(d)

#Select Only Data and Price
data = d %>%
  select(date, PriceUSD)

#Remove NA

data = na.omit(data)
z <- read.zoo(text = Lines, header = TRUE, )

data$date
data$date <- as.Date(data$Date)


start = as.Date('2011-07-18')
end = as.Date('2021-02-21')
btc_ts = ts(data = data, start = start, end = end, frequency = 12)

btc_ts
class(btc_ts)

data

library(zoo)
z <- read.zoo(data, format ="%Y-%m-%d" )
zz = as.ts(z)
zz
#Create TS 
library(tseries)

btc.ts = ts(as.vector(data$PriceUSD), start = c(2010, 718), frequency = 1)

#View Price Plot
plot(btc.ts, xlim=  c(2010,2021), xlab = 'Time', ylab = 'Price')


#
library(forecast)
library(TSA)
plot(y=btc.ts,x=zlag(btc.ts), ylab="Bitcoin Price of Today (in $)", 
     xlab="Bitcoin Price of Yesterday (in $)", main="Plot of Autocorrelation for Lag 1")




library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test

## Plot Autocorrelation
ggAcf(x = btc.ts)
start(btc.ts)
end(btc.ts)
btc.ts

time(btc_ts)



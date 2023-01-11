#Clustering
data=read.csv('/Users/zhenyang/Desktop/exam/fastfood_survey_exam.csv')

sum(is.na(data))

fastfood_std=scale(data)
view(fastfood_std)

d=dist(x=fastfood_std,method='euclidean')
clusters=hclust(d=d,method='ward.D2')
plot(clusters)

cor(cophenetic(clusters),d)

h_clusters=cutree(tree=clusters,k=3)
table(h_clusters)

data2=cbind(data,h_clusters)
library(dplyr)
data2 %>%
  select(everything())%>%
  group_by(h_clusters)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#Recommendation
library(recommenderlab)
data(MovieLense)
MovieLense

nratings(MovieLense['115',])   

mean(getRatings(MovieLense['115',]))

set.seed(1706)
split = sample(nrow(MovieLense),size = 0.8*nrow(MovieLense))
train = MovieLense[split,]
test = MovieLense[-split,]

dim(train)

recom_ubcf=Recommender(train,method='UBCF',parameter=list(method='cosine',nn=10,normalize='center'))
pred_ubsf_topN=predict(recom_ubcf,newdata=test,method='topNList',n=3)
getList(pred_ubsf_topN)['115']

pred_ubsf=predict(recom_ubcf,newdata=test,type="ratings")
pred_ubsf_topr=predict(recom_ubcf,newdata=test,method='ratings',n=3)
getRatings(pred_ubsf_topN)['115']

recom_popular = Recommender(train,
                            method='POPULAR',
                            parameter=list(normalize='center'))

pred_popular_topN = predict(recom_popular,newdata=test,type='topNList',n=3)
getList(pred_popular_topN)['115']

#ts
ausbeer=readRDS('/Users/zhenyang/Desktop/exam/ausbeer.RDS')
class(ausbeer)

train = window(ausbeer,start=c(1956,01),end=c(2004,04))
test = window(ausbeer,start=c(2005,01),end=c(2010,02))
length(test)

average_model = meanf(train,h = 22) #34 means 34 months
average_model
accuracy(average_model,x=ausbeer)

auto_arima_model=auto.arima(train)
auto_arima_model 

auto_arima_model_forecast = forecast(auto_arima_model,h=24)
auto_arima_model_forecast

accuracy(auto_arima_model_forecast,x = ausbeer)


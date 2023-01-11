# Cluster then predict
#part 2 advnaced clustering


wine = read.table('wine.csv',header=TRUE,sep=';')

str(wine)

#Predict using regression
library(caret)
set.seed(1706)
split = createDataPartition(y=wine$quality,p = 0.7,list = F,groups = 100)
train = wine[split,]
test = wine[-split,]

#predict
linear = lm(quality~., train)
summary(linear)
sseLinear = sum(linear$residuals^2); sseLinear  #testing of sum of square errors
predLinear = predict(linear,newdata=test)
#out of sample error (test)
sseLinear = sum((predLinear-test$quality)^2); sseLinear

#cluster then predict using regression 
#Let us cluster first and then run a model to see if there is an improvement. To cluster the wines, we have to remove the outcome variable

trainMinusDV = subset(train,select=-c(quality))
testMinusDV = subset(test,select=-c(quality))

#prepare data for clustering,  need to normalize 

library(caret)
preproc = preProcess(trainMinusDV)
trainNorm = predict(preproc,trainMinusDV)
testNorm = predict(preproc,testMinusDV)
mean(trainNorm$chlorides)

mean(testNorm$chlorides)

#hierarchial cluster analysis
distances = dist(trainNorm,method = 'euclidean')
clusters = hclust(d = distances,method = 'ward.D2')
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))
clusterGroups = cutree(clusters,k=2)
#To express the clusters on a scatterplot, we flatten the data with eleven variables into 2 dimensions by conducting a factor analysis with varimax rotation. This is done because it is not possible to visualize 11-dimensional data.
library(psych)
temp = data.frame(cluster = factor(clusterGroups),
                  factor1 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#k means
set.seed(1706)
km = kmeans(x = trainNorm,centers = 2,iter.max=10000,nstart=100)
#km$centers
mean(km$cluster==clusterGroups) # %match between results of hclust and kmeans


#total withi sum of squares plot
within_ss = sapply(1:10,FUN = function(x) kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#ratio plot
ratio_ss = sapply(1:10,FUN = function(x) {km = kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


#silhouette plot
library(cluster)
silhoette_width = sapply(2:10,FUN = function(x) pam(x = trainNorm,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#Both, the elbow plot and silhoette plot recommend a two-cluster solution, so we will group the data into two clusters.
set.seed(1706)
km = kmeans(x = trainNorm,centers = 2,iter.max=10000,nstart=100)


#### assign a test point to a cluster
install.packages("proxy")
library(proxy)

proxy::dist(testNorm[1:5,], km$centers)

#computing pairwise distances from the 5 selected points to the two centers

#or use flexclust
#apply clustering solution from train to test
library(flexclust)
km_kcca = as.kcca(km,trainNorm) # flexclust uses objects of the classes kcca
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=testNorm)

#distribution of wines across clusters in train
table(clusterTrain)


#train a model per cluster
sses <- sapply(1:k,
function(i){
  traini <- subset(train,clusterTrain==i)
  testi <- subset(test, clusterTest==i)
  lm <- lm(quality~., traini)
  predi <- predict(lm, newdata=testi)
  sum((testi$quality-predi)^2)
})
sses

##combined error
sum(sses)
#vs#
ssLinear
## very prone to overfitting


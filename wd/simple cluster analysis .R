
#simple cluster analysis 
#1/19/2021

#generate random data

set.seed(1031)
data <- data.frame(x1 = sample(1:10, 10, replace=T),
                   x2 = sample(1:10, 10, replace= T))
rownames(data) <- 1:10
head(data)

#scatter plot
plot(data$x1, data$x2, type = 'n')
text(data$x1, data$x2, labels=1:nrow(data))

library(ggplot2)
ggplot(data=data,aes(x=x1,y=x2))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F)+
  geom_text(aes(label=rownames(data)),hjust=-1.5,vjust=0.5)

## Hierarchial Clustering
#compute distance

#distance- euclidean distance is a straight line between two points

eucl_dist <- function(x, y){
  sqrt(sum((x - y)^2))
}

eucl_dist(data[9,], data[3,])
eucl_dist(data[3,], data[9,])

# manhatan distance (taxi cap distance) -- think of it like a grid

man_dist <- function(x, y) {
  sum(abs(x-y))
}
man_dist(data[9,], data[3,]) #looking at the points 9 and 3

## all pairwise eucl distance
distances = round(dist(data, method = "euclidean"), 2)
distances



mdistances <- round(dist(data, method = "manhattan"),2)
mdistances

#in order to subset distacne matrix
#convert to matrix
as.matrix(distances)[2,3]  #looking at points 2 and 3

# or use this package where you dont have to convert to matrix 
install.packages("usedist")
library(usedist)
dist_get(distances, 2, 3)

## application: retrieve all objects within a distance
dist_get(distances, 5, 1:nrow(data))  #row 5
#subset with a distance of 3
which(dist_get(distances, 5, 1:nrow(data)) <=3)


##clustering 
# hierchical clusting

clust <- hclust(distances, method = "ward.D2")

plot(clust)
par(mfrow=c(1,2))
plot(data$x1, data$x2, type = 'n') ; text(data$x1, data$x2, labels = 1:nrow(data))

#dendrogram: tree presentation of cluster results

co_distances <- cophenetic(clust)
co_distances
#cophenetic distance is the sum of the branch length of each tree which is trying to approximate the euclidian distance



#how closely does this tree show euclicean distance
# use tree to get clustering solution 
dist_get(co_distances, 3, 9)
dist_get(distances, 3, 9)

#Unlike predictive modeling techniques, clusters anlayses do not yield a unique solution. The number of clusters chosen is based on, (a) Distance: Clusters should be far apart relative to within cluster distances (b) Meaningfulness: Clusters should be meaningful within the domain of inquiry.
#Trying out a 2-cluster solution
plot(clust)
rect.hclust(tree=clust, k = 2) #only 2 subgroups
#identify rectanges that split the group into 2 groups

clusters <- cutree(clust, k =2)
clusters

install.packages("dendextend")
library(dendextend)
plot(color_branches(as.dendrogram(clust), k =2))

#cut based on tree height
clust$height #height of each internal nodes

#now let us cut the data at two clusters, all clustering algos are similar in 
#that they yield a fairly boring vector of cluster embershisp




#visualize the clustering solution by coloring the scatter plot
data2 = cbind(data,clusters)
(p1 <- ggplot(data=data2,aes(x=x1,y=x2,color=factor(clusters)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F,color=F)+
  geom_text(aes(label=rownames(data2)),hjust=-1.5,vjust=0.5)
)

install.packages("ggthemes")
library(ggthemes)
p1 + scale_colour_colorblind()


# K means clustering
install.packages("animation")
library(animation)
kmeans.ani(data , centers = 4, pch=1:4, col=1:4) 

#Hierarchical clustering requires computing distances between every pair of observations. For large datasets, this can be a very expensive process, (for n observations have n(n-1)/2 distances). K-means clustering begins by arbitrarily placing centroids in the data and then iterating from that point to the final solution. This disorganized approach to clustering produces similar quality of clusters to hierarchical clustering but much faster.

#It is worth noting that K-Means is sensitive to starting point, so the seed can influence final solution. Also, for k-means, one has to provide the number of centers.

set.seed(100)
km = kmeans(data, centers = 4, iter.max = 1000)
km
summary(km)

#picking 4 clusters (centers) is arbitrary, how do we know what the optimal k?

#metrics to do model selection
km$withinss #within cluster, sum of squares
km$betweenss
km$totss

xmean <- colMeans(data)
apply(data, 1, function(x) eucl_dist(xmean, x))^2

#proprotion of between cluster variation
km$betweenss/km$totss
sum(km$withinss)
km$tot.withinss

#use these metrics to pick the number of clusters
library(tidyverse)
set.seed(100)
kms <- lapply(1:9, kmeans, x = data, iter.max = 100, nstart = 10)
within_ss = sapply(X = 1:9, 
                   FUN = function(x) kmeans(data,centers = x,iter.max = 100)$tot.withinss)

ratio_ss = sapply(X = 1:9, 
                  FUN = function(x) {
                    km = kmeans(data,centers = x,iter.max = 100)
                    ratio = km$betweenss/km$totss
                    return(ratio)
                  })
dat = data.frame(clusters=1:9,within_ss, ratio_ss)

# Elbow in Within_ss
library(ggplot2)
ggplot(dat,aes(x=clusters,y=within_ss))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept=4)

## elbow method: point where additional clustering doesnt further reduce total within cluster
# variation and/or ration of between cluster to total variation


# average silhouette width
#Another method used for determining the optimal number of cluster is the Silhoette method. Here we use the pam function to determine the average silhoeuette width. Note, pam runs an algorithm like k-means but not exactly the same as k-means. Since we are only using it to determine ideal number of cluster, it should be okay.
install.packages("cluster")
library(cluster)

silhouette(kms[[2]]$cluster, distances)

# a: the average distance of a point to all observations within its cluster
# b: the average distance of a point to all observations its nearest cluster
# sil = (b-a)/max(a,b) = [-1, 1]
mean(silhouette(kms[[3]]$cluster, distances)[,3])
#number between o and 

sil <- sapply(kms[-1], function(km) mean(silhouette(km$cluster, distances)[,3]))
plot(2:9, sil, type = "b", xlab = "k")

## model based clustering
install.packages("mclust")
library(mclust)
clus <- Mclust(data, G = 4)
clus
summary(clus)

plot(clus, what = "density", cex = 0.5)
points(data) #plot means, each density things represent a glaussian model
mcls <- lapply(1:9, FUN=function(k) Mclust(data, G=k, modelNames = c("EII")))

par(mfrow=c(1,2))
plot(1:9, sapply(mcls, getElement, 'loglik'), type = "b", xlab = 'k')
#df - number of mixtaure components, + distribution paramwters ( guassian means and covariances)
plot(1:9, sapply(mcls, getElement, 'df'), type = "b", xlab = "k")

bic <- sapply(mcls, function(mc) mc$df*log(mc$n) - 2*(mc$loglik))
plot(1:9, bic, type = 'b', xlab = 'k')


plot(mcls[[4]], what = c('classification'))
plot(clus, what = c('classification'))



## iiris
dat <- iris[1:100, c(1,2,5)]
head(dat)

library(ggplot2)
ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point()

#last semester - looked at classification models, this time, take unsupervised view

#kmeans
set.seed(617)
km <- kmeans(x = dat[,1:2], centers = 2, iter.max = 1000, nstart=25)

k_cluster <- km$cluster

ggplot(data=cbind(dat,clusters = k_cluster), 
       aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters),
           shape = Species)) + 
  geom_point()
  
  
mcls <- lapply(1)

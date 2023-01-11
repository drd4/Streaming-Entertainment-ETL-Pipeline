## Simple illustration of cluster analysis
#
#
# @author Zachary Kurtz
# @date 1/19/2020

library(ggplot2)

## generate some random data
set.seed(1031)
data <- data.frame(x1=sample(1:10, 10, replace = T),
                   x2=sample(1:10, 10, replace = T))
rownames(data) <- 1:10
head(data)


## simple scatter plots for the data
plot(data$x1, data$x2, type='n')
text(data$x1, data$x2, labels=1:nrow(data))


ggplot(data=data,aes(x=x1,y=x2))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F)+
  geom_text(aes(label=rownames(data)),hjust=-1.5,vjust=0.5)

## Hierarchial Clustering
#compute distance

#distance- euclidean distance is a straight line between two points
### Distances ###

## we need a distance metric.
# euclidean distance is a straight line between two points:
eucl_dist <- function(x, y) {
  sqrt( sum((x - y)^2) )
}
# manhattan distance straight line if you could only travel along lines parallel to the axes
# manhatan distance (taxi cap distance) -- think of it like a grid

man_dist <- function(x, y) {
  sum( abs(x - y) )
}

eucl_dist(data[9,], data[3,])  #looking at the points 9 and 3
eucl_dist(data[3,], data[9,])

man_dist(data[9,], data[3,])
man_dist(data[9,], data[4,])
eucl_dist(data[9,], data[4,])

## R has a special data structure for distances
distances <- round(dist(data, method = "euclidean"),2)
distances


mdistances <- round(dist(data, method = "manhattan"),2)
mdistances
#in order to subset distacne matrix
#convert to matrix

## how to use dist: convert to matrix or usedist package
as.matrix(distances)[9,3] 
library(usedist)
dist_get(distances, 9, 3)

## application: retrieve all objects within a distance t
## of your target customer
dist_get(distances, 5, 1:nrow(data))
which(dist_get(distances, 5, 1:nrow(data)) <= 3)
## application: retrieve all objects within a distance
dist_get(distances, 5, 1:nrow(data))  #row 5
#subset with a distance of 3
which(dist_get(distances, 5, 1:nrow(data)) <=3)

## choice of reference point and 'threshold' is arbitrary.
## how do we cluster points to get all reasonable subgroups


### Hierchical clustering ###
clust <- hclust(distances, method = "ward.D2")

par(mfrow=c(1,2))
plot(data$x1, data$x2, type='n') ; text(data$x1, data$x2, labels=1:nrow(data))
plot(clust)
#dendrogram: tree presentation of cluster results


co_distances <- cophenetic(clust)
#cophenetic distance is the sum of the branch length of each tree which is trying to approximate the euclidian distance

dist_get(co_distances, 3, 9)
dist_get(co_distances, 3, 6)

#how closely does this tree show euclicean distance
# use tree to get clustering solution 
dist_get(distances, 3, 9)
dist_get(distances, 3, 6)


#Unlike predictive modeling techniques, clusters anlayses do not yield a unique solution. The number of clusters chosen is based on, (a) Distance: Clusters should be far apart relative to within cluster distances (b) Meaningfulness: Clusters should be meaningful within the domain of inquiry.
#Trying out a 2-cluster solution
## How to get a 'hard' clustering solution
dev.off()
plot(clust)
rect.hclust(tree=clust,k = 2) #only 2 subgroups
#identify rectanges that split the group into 2 groups

## see the solution
clusters = cutree(clust,k = 2)
plot(clust,label=clusters)

## plot with colored branches
library(dendextend)
plot(color_branches(as.dendrogram(clust),k=2))

#cut based on tree height
clust$height #height of each internal nodes

#now let us cut the data at two clusters, all clustering algos are similar in 
#that they yield a fairly boring vector of cluster embershisp


## cut based on height
## each entry is the height of the internal nodes
clust$height ## lets try height of ~6
clusters <- cutree(clust, h=6)


par(mfrow=c(1,2))
plot(clust)
rect.hclust(tree=clust, h=6)
plot(color_branches(as.dendrogram(clust),k=4))


## Visualize the clustering solution by coloring the scatter plot
data2 <- cbind(data,clusters)
(p1 <- ggplot(data=data2,aes(x=x1,y=x2,color=factor(clusters)))+
        geom_point(aes(size=1.2))+
        scale_x_continuous(limits=c(1,10),breaks=1:10)+
        scale_y_continuous(limits=c(1,10),breaks=1:10)+
        guides(size=F,color=F)+
        geom_text(aes(label=rownames(data2)),hjust=-1.5,vjust=0.5)
)

## lets add a colorblind friendly palette for our friends
library(ggthemes)
p1 + scale_colour_colorblind()



### K means clustering ###
library(animation)
kmeans.ani(data, centers = 4, pch=1:4, col=1:4)

#Hierarchical clustering requires computing distances between every pair of observations. For large datasets, this can be a very expensive process, (for n observations have n(n-1)/2 distances). K-means clustering begins by arbitrarily placing centroids in the data and then iterating from that point to the final solution. This disorganized approach to clustering produces similar quality of clusters to hierarchical clustering but much faster.

#It is worth noting that K-Means is sensitive to starting point, so the seed can influence final solution. Also, for k-means, one has to provide the number of centers.

## sensitive to starting point - seed is important for reproducability
set.seed(100)
km <- kmeans(data, centers = 4, iter.max = 1000)
print(km)

# How to choose optimal k?
# total within-cluster variation &
## proportion of between-cluster variation

#picking 4 clusters (centers) is arbitrary, how do we know what the optimal k?

#metrics to do model selection
km$withinss #within cluster, sum of squares
km$withinss
km$betweenss/km$totss

## totss: sum of squared euclidean distances to the data 'mean'
xmean <- colMeans(data)
sum(apply(data, 1, function(x) eucl_dist(xmean, x))^2)
## withinss is the sum of squared distances to the cluster centroid:


#use these metrics to pick the number of clusters 
## compute within SS
set.seed(100)
kms <- lapply(1:9, kmeans, x=data, iter.max = 100, nstart = 10)

## kmeans minimizes tot.withinss (total within-cluster variation)
within_ss <- sapply(kms, function(km) km$tot.withinss)
ratio_ss <- sapply(kms, function(km) km$betweenss/km$totss)

dat <- data.frame(clusters=1:9, within_ss, ratio_ss)
## within cluster variation will go to zero as points get solo-clusters
library(ggplot2)
library(gridExtra)

grid.arrange(
  ggplot(dat,aes(x=clusters,y=within_ss))+
    geom_line(color='steelblue',size=1.4)+
    scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
    geom_vline(xintercept=4),


  ggplot(dat,aes(x=clusters,y=ratio_ss))+
    geom_line(color='steelblue',size=1.4)+
    scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
    geom_vline(xintercept=4)
)
## find the "elbow" in either of these two metrics (e.g.) ->
# point where additional clustering doesn't further reduce total within-cluster variation and/or ratio of between cluster to total variation

## elbow method: point where additional clustering doesnt further reduce total within cluster
# variation and/or ration of between cluster to total variation


# Average silhouette width
library(cluster)
## a = the average distance of a point to all observations within its cluster
## b = the average distance of a point to all observations it's nearest cluster
## sil = (b-a) / max(a,b)
silhouette(kms[[2]]$cluster, distances)
silhouette(kms[[3]]$cluster, distances)

# a: the average distance of a point to all observations within its cluster
# b: the average distance of a point to all observations its nearest cluster
# sil = (b-a)/max(a,b) = [-1, 1]
mean(silhouette(kms[[3]]$cluster, distances)[,3])
#number between o and 

sil <- sapply(kms[-1], function(km) mean(silhouette(km$cluster, distances)[,3]))
plot(2:9, sil, type='b')



## PAM (paritioning around mediods) - like kmean but use median not mean
pams <- lapply(2:9, function(k) pam(data, k))
sil <- sapply(pams, function(x) x$silinfo$avg.width)
plot(2:9, sil, type='b')



## Model based clustering:
## assume data from each cluster is comes from it's own distribution (e.g. Gaussian [normal] distrubtions with different means)
## mbc finds the component mixture distributions
library(mclust)
clus <- Mclust(data, G=4)
summary(clus)
plot(clus, what = "density", cex = 0.5) 
points(data)#plot means, each density things represent a glaussian model

## How to find optimal number of mixture components:
mcls <- lapply(1:9,FUN=function(k) Mclust(data,G=k, modelNames=c("EII")))

par(mfrow=c(1,2))
plot(1:9, sapply(mcls, getElement, 'loglik'), type='b', xlab='k')
## df - number of mixture components + distribution parametrs (Gaussian mean and co-variances)
plot(1:9, sapply(mcls, getElement, 'df'), type='b', xlab='k')

## Bayesian information criteria - maximize the likelihood and penalize too many parameters
bic <- sapply(mcls, function(mc) mc$df*log(mc$n) - 2*(mc$loglik))
plot(1:9, bic, type='b', xlab='k')

## looking for a locally minimal BIC
plot(mcls[[4]], what=c('classification'))


## in practice, let mclust choose the model type for you:
mcls <- lapply(1:9,FUN=function(k) Mclust(data,G=k))
bic <- -sapply(mcls, getElement, 'bic')
plot(1:9, bic, type='b')
plot(mcls[[4]], what=c('classification'))

#IRIS
dat = iris[1:100,c(1,2,5)]
head(dat)
library(ggplot2)
ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point()
#Hierarchical Cluster Analysis

#We are going to use Sepal.Width and Sepal.Length to cluster the 100 flowers. To conduct a hierarchical cluster analysis, we standardize the two columns, and then use Ward’s method to cluster the flowers based on their euclidean distance. The dendrogram illustrates a nice separation into two groups.
dat[,1:2] = scale(dat[,1:2])
clusters = hclust(d = dist(dat[,1:2],method = 'euclidean'),method = 'ward.D2')
library(factoextra)
fviz_dend(clusters,k = 2)

#next, lets extract the cluster assignments and plot them. Hopefully, our clusters clearly distinguish the two flower species.

h_clusters = cutree(clusters,k = 2)
ggplot(data=cbind(dat,clusters = h_clusters), aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters)))+
  geom_point()

#k-means

#Let’s try k-means algorithm, specifically asking it to give us two clusters. Again, we are hoping the cluster assignments clearly distinguish the two species of flowers.
set.seed(617)
km = kmeans(x = dat[,1:2],centers = 2,iter.max = 1000,nstart = 25)
k_cluster = km$cluster
ggplot(data=cbind(dat,clusters = k_cluster), aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters)))+
  geom_point()

#Model-based

#Lastly, let us try a mixture model to see if we can generate clusters that map well to the original flower species.
library(mclust)
m_cluster = Mclust(dat[,1:2])$classification
ggplot(data=cbind(dat,clusters = m_cluster), aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters)))+
  geom_point()
#Results

#The three algorithms yielded identical results. All three made one error in classifying the two flowers.

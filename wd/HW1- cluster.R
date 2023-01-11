#assignment 1
#section 1

library(tidyverse)

data = read.csv("fastfood_survey.csv")
head(data)


#Question 1  How many variables are in the dataset
data = read.csv("fastfood_survey.csv")
#in the top right environment, we can see 21 

#Question 2:  subset data to include first eleven columns, how many variables

library(tidyverse)
data_cluster = data %>% select(1:11)   #this selects the first 11 columns 
head(data_cluster)
#11 variables - this is a stupid question


#Question 3: how many missing variables are in cleanliness?

#so we need to select the variable cleanliness from data_cluster, and find out many missing values there are
# we can use the is.na() to see how many missing values there are, it will return true/false values
is.na(data_cluster$cleanliness)

#this doesnt help, as we want the total number of true values, so we can get the sum
sum(is.na(data_cluster$cleanliness))
#23

#Question 4:  How many rows of data would be left if rows corresponding to missing values on any of the eleven variables were removed

#so again, we are subsetting the df to remove rows with N/a

random = na.omit(data_cluster) #so we dont overwrite data

#in environment we see 556 rows of data 

#Question 5: use mice to impute missing values, what is new value for obs 10 for cleanliness
library(mice)
set.seed(1706)
data_cluster = complete(mice(data_cluster, use.matcher = T))
head(data_cluster)


#data_cluster$cleanliness[10]
#6
# data_clustertest = data_cluster
# data_clustertest = complete(mice(data_clustertest, use.matcher = TRUE))
# data_clustertest = scale(data_clustertest)

print(data_clustertest)

#Question 6: use scale, what is value of obs 10 for cleanliness
data_cluster_sc = scale(data_cluster)
head(data_cluster)
#cleanliness is the 4th variable in the matrix, so we can select the column and row number
#data_cluster[,10:4]  #for a matrix
#or could just make it a dataframe
#data_cluster <- as.data.frame(data_cluster)
# data_cluster$cleanliness[10]

#0.348

###################################

#Section 2

#Question 1: Compute the euclidean distance between all observations in data_cluster- how many elements are in the distance matrix
library(cluster)

d = dist(data_cluster_sc, method = 'euclidean')
d
#its 193131


# a = d
# as.matrix(d)[1:12, 1:11]
# data_cluster

#Question 2: Conduct a Hierarchical cluster analysis using the method, 'ward.D2'. Plot the dendrogram from this process. Let us see how well the dendrogram matches true distances. What is the Cophenetic correlation coefficient?
clusters = hclust(d = d, method = 'ward.D2')

plot(clusters, cex = .2)
cor(cophenetic(clusters),d)
#.7903926

#Question 3Based on the distances shown in the dendrogram alone, which is the best cluster solution?
plot(cut(as.dendrogram(clusters),h=5)$upper)

dim(data_cluster)

dim(data_cluster_sc)
dim(d)

rect.hclust(tree=clusters, k = 2, border = 'tomato')
rect.hclust(tree=clusters, k = 3, border = 'yellow')
rect.hclust(tree=clusters, k = 4, border = 'blue')
rect.hclust(tree=clusters, k = 4, border = 'blue')
library(gridExtra)
library(factoextra)
library(dendextend)


grid.arrange(fviz_dend(x = clusters,k=2),
             fviz_dend(x = clusters,k=3),
             fviz_dend(x = clusters,k=4)
)

head(data)
fviz_cluster()

#not sure, would say 2 clusters 

#Question 4: if you went with 2 cluster, how many obs would be in the small cluster?

h_segments = cutree(tree = clusters, k =2)
table(h_segments)

#41

#Question 5: went with 3 cluster
h_segments_3 = cutree(tree= clusters, k = 3)
table(h_segments_3)
#41 

#Question 6: k-means 2 cluster soluution, set seed 1706, max itertations to 100,
set.seed(1706)
km = kmeans(x = data_cluster_sc, centers = 2, iter.max = 100)
km_seg = km$cluster

#43

#Question 7: 
set.seed(1706)
km_3 = kmeans(x = data_cluster_sc, centers = 3, iter.max = 100)
table(km_3$cluster)

#41

#Question 8: In the above k-means analyses, we set the number of clusters expected. Now, let us examine a data driven approach to determining the number of clusters. Compute the total within cluster sum of squares for cluster solutions from 2 to 10. Use a seed of 1706. Do not set nstart. What is the total within cluster sum of squares for a three-cluster solution?

paste(km_3$totss,'=',km_3$betweenss,'+',km_3$tot.withinss,sep = ' ')
#6831
km_3$totss
km_3$tot.withinss

### think its 3801.308

within_ss = sapply(2:10,FUN = function(x){
  set.seed(1706)
  kmeans(x = data_cluster_sc,centers = x,iter.max = 100)$tot.withinss})

ggplot(data=data.frame(clusters = 2:10,within_ss),aes(x=clusters,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Questin 9:
km_3$betweenss/km_3$totss
#0.4435209
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(1706)
  km = kmeans(x = data_cluster,centers = x,iter.max = 100)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))



#Question 10:Construct a line graph of clusters (on x-axis) against total within cluster sum of squares (on y-axis). Based on this chart, which of the following are good cluster solutions?


set.seed(1706)
within_ss = sapply(1:10,FUN = function(x){
  kmeans(x = data_cluster_sc,centers = x,iter.max = 100)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#3, 2

#35 min into lecture
#Question 11 What is the average silhouette width for a 2 cluster solution? Use pam() from library(cluster) to compute silhouette width.
library(cluster)
pam(data_cluster_sc,k = 2)$silinfo$avg.width
#0.5869224

#Question 12 What is the average silhouette width for a 3 cluster solution?

library(cluster)
pam(data_cluster_sc,k = 3)$silinfo$avg.width
#0.1722977

#Question 13
library(cluster)
silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = data_cluster_sc,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
#looking for high silhouette width - 2

#Question 14: Now, we will make use of a Model-based clustering technique. Use Mclust() from library(mclust) to cluster the data. How many clusters has the model decided to group the data into?

library(mclust)
clusters_mclust = Mclust(data_cluster_sc)
summary(clusters_mclust)

#3

#Question 15: Now, use model-based clustering to force a two-cluster solution. (Save the cluster memberships in an object as you will need it in a later question). How many observations are in the smaller cluster?
clusters_mclust_2 = Mclust(data_cluster_sc,G=2)
summary(clusters_mclust_2)
#171

#Question 16:
m_clusters = Mclust(data_cluster_sc, G = 2)
m_segments = m_clusters$classification
xtab <- table(h_segments, km_seg)
xtab
table(h_segments)
table(km_seg)

# add two intersections, 3+1 = 4

#question 17
table(m_segments)
table(km_seg)
xxtab <- table(m_segments, km_seg)
xxtab

#171-43 128

########
#Section 3

#Question 1:  k-means 3 clusters, 1706 seed, iterations 100, combine KM with original data, profile resulting clusters
#Compared to other clusters, cluster 1 had the lowest value 

## drive through, pop with children 
set.seed(1706)
kmm_3 = kmeans(x = data_cluster_sc, centers = 3, iter.max = 100)
table(kmm_3$cluster)
km3_seg = kmm_3$cluster


data2 = cbind(data, km3_seg)
head(data2)
str(data2)

library(dplyr)
data2 %>%
  select(speed_of_service:taste_burgers,km3_seg)%>%
  group_by(km3_seg)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()


library(dplyr); library(ggplot2); library(tidyr)
data2 %>%
  select(speed_of_service:taste_burgers,km3_seg)%>%
  group_by(km3_seg)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,speed_of_service:taste_burgers)%>%
  ggplot(aes(x=var,y=value,fill=factor(km3_seg)))+
  geom_col(position='dodge')+
  coord_flip()

#Question 2:  cluster 2 had highest value for  (look at same graph)
## all but drive throguh

#Question 3: cluster 3 highest value
# drive through 

#Question 4:  cluster 3 had lowest value 
#all but pop with children and drive through 

str(data2)

lapply(13:21,function(x) round(prop.table(table(data2$km3_seg,data2[,x]),1),2)*100)

library(RColorBrewer)
lapply(13:21,function(x) {
  dat = round(prop.table(table(data2$km3_seg,data2[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=6)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
}) 


(round(prop.table(table(data2$km3_seg,data2$dollars_avg_meal),1),2)*100)
#cluster 3 spends the most 


(round(prop.table(table(data2$km3_seg,data2$marital_status),1),2)*100)
#cluster 1 does not have the smallest percetage of single

(round(prop.table(table(data2$km3_seg,data2$gender),1),2)*100)
#cluster 2 has most females

(round(prop.table(table(data2$km3_seg,data2$number_children),1),2)*100)
#cluster one does not have the most number of children

(round(prop.table(table(data2$km3_seg,data2$own_rent),1),2)*100)
#***********cluster 1 DOES have the lowest ownership******####

(round(prop.table(table(data2$km3_seg,data2$occupation),1),2)*100)
#cluster 3 has largest perentage of prof, and stay at home moms

(round(prop.table(table(data2$km3_seg,data2$education),1),2)*100)
#cluster 2 has least amount of education 

(round(prop.table(table(data2$km3_seg,data2$age),1),2)*100)
#*************cluster 1 is the youngest


#Question 6  cluster 2


(round(prop.table(table(data2$km3_seg,data2$marital_status),1),2)*100)
#****cluster 2 has lowest percentage of singles

(round(prop.table(table(data2$km3_seg,data2$gender),1),2)*100)
#*****cluster 2 has most females

(round(prop.table(table(data2$km3_seg,data2$number_children),1),2)*100)
#cluster 2 does have the most children 


(round(prop.table(table(data2$km3_seg,data2$occupation),1),2)*100)
#cluster 2 stay at home moms

(round(prop.table(table(data2$km3_seg,data2$education),1),2)*100)
#cluster 2 has least amount of education 


#Question 7 cluster 3
(round(prop.table(table(data2$km3_seg,data2$dollars_avg_meal),1),2)*100)
#****cluster 3 spends the most 


(round(prop.table(table(data2$km3_seg,data2$occupation),1),2)*100)
#cluster 3 has largest perentage of prof, 


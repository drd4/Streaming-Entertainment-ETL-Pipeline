## 2/2/2021
#Mod 4

#PCA Lesson

wine = read.table('wine-1.csv',header=TRUE,sep=';')

library(caret)
set.seed(1706)
split = createDataPartition(y=wine$quality,p = 0.7,list = F,groups = 100)
train_wine = wine[split,]
test_wine = wine[-split,]

#drop outcome

train = train_wine[,1:11]
test = test_wine[,1:11]

#PCA is used to reduce the dimensionality of the data by representing a large number of variables with a fewer number of components. Similar variables get grouped into the same component while dissimilar variables are placed in different components.

#correlations 
#Similarity is typically judged by the correlation. Presence of large correlations in the data indicates similarity in the data. From the perspective of supervised learning techniques, correlated variables indicate redundancy.

round(cor(train,use='complete.obs'), 3)

library(tidyverse); library(ggplot2)
corMatrix = as.data.frame(cor(train))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))


library(corrplot)
corrplot(cor(train),type = 'lower',col = c('red','white','green'),method = 'pie',diag = F,order='hclust')
library(ggcorrplot)
ggcorrplot(cor(train),colors = c('red','white','green'),hc.order = T,type = 'lower')

# Barletts test of sphericity
library(psych)
cortest.bartlett(cor(train),n = nrow(train))
# cannot reject null that cor is negative 

I <- diag(ncol(train))
I


#KMO measure of sampling adequacy (MSA)
#Compares partial correlation matrix to pairwise correlation matrix. A partial correlation is a correlation after partialing out all other correlations. If the variables are strongly related, partial correlations should be small and MSA close to 1. If MSA > 0.5, data is suitable for factor analysis.

#KMO  cut off is 0.5 if data is suitable for factor analysis
# if MSA =1, variables are perfectly correlated
KMO(cor(train))


install.packages("FactoMineR")
library(FactoMineR)

## optimal number of components
pca <- prcomp(train, scale. = T)

install.packages("factoextra")
library(factoextra)
fviz_eig(pca,ncp=11,addlabels = T)
pvar_explained <- pca$sdev^2/sum(pca$sdev^2)
pvar_explained*100
sum(pvar_explained*100)

# total var explained by cumlatively 70%
cumsum(pvar_explained)

#Eigen Value

(eV <- eigen(cor(train))$values)
pca$sdev^2

which(eV>1)

#parallel analysis

library(psych)
dev.off()

fa.parallel(train,fa='pc') #accept pc sum greater than 1


## Assess contribution of each vairbale to the PCs
#refit using PCA from factominer package
library(FactoMineR)
pca_facto = PCA(train,graph = F)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(
  fviz_contrib(pca_facto, choice ="var", axes = 1),
  fviz_contrib(pca_facto, choice ="var", axes = 2)
)

#missed some stuff

#apply a learned pca to out of sample points
prep <- caret::preProcess(train)

#apply out of sample
testComponents <- predict(prep, as.matrix(test)) %*% pca$rotation


trainComponents <- pca$x
testComponents <- predict(pca, newdata = test)

trainComponents <- pca_facto$ind$coord

testComponents <- predict(pca_facto, newdata = test)$coord

plot(testComponents, col = test_wine$quality)




#

########

data <- read.csv("toothpaste.csv")


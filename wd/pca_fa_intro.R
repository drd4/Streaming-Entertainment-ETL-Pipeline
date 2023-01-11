X <- scale(iris[,1:4])
pairs(X)

pca <- prcomp(X)

loadings <- pca$rotation

## recall matrix multiplication:
pc_1 <- X[,1]*loadings[1,1] + X[,2]*loadings[2,1] + X[,3]*loadings[3,1] + X[,4]*loadings[4,1]
pc_2 <- X[,1]*loadings[1,2] + X[,2]*loadings[2,2] + X[,3]*loadings[3,2] + X[,4]*loadings[4,2]
## etc... up to p=4

## equivilently:
pc_1_mm <- X %*% loadings[,1]
pc_2_mm <- X %*% loadings[,2]

all.equal(as.matrix(pc_1), pc_1_mm)
all.equal(as.matrix(pc_2), pc_2_mm)

## collect all principal components:
scores <- pca$x

all.equal(
  X %*% loadings,
  scores
)

## the goal:
# for the 1st pc: 'loadings' (e.g. just _p_ numbers that maximize the "variance")
# with a 'special' constraint:
sum(loadings[,1]^2) ## sum of squared loadings is 1 (aka - "normal")

## is this the maximally variant?
var(X %*% loadings[,1])

## lets compare with 1000 simulated "loadings" (normalized coefficients drawn at random)
replicate(1000, {
  a <- rnorm(4)
  a <- a/sqrt(sum(a^2))
  var(X %*% a) <= var(X %*% loadings[,1])
})

# indeed:
sqrt(var(scores[,1]))
pca$sdev[1]

## For every principal component beyond the first one, loadings maximize the variance
# with an additional constraint: the new loadings must be "orthogonal" to all other ones
loadings[1,1] * loadings[1,2] +
  loadings[2,1] * loadings[2,2] +
  loadings[3,1] * loadings[3,2] +
  loadings[4,1] * loadings[4,2] # == 0
# or:
t(loadings) %*% loadings ## off diagonals are 0
# or:
crossprod(loadings)


## covariation between PCs are zero (project data onto a smaller set of 'uncorrelated' axes)
diag(cov(scores))
pca$sdev^2

## the variances of the score matrix is always given by the covariance of the origin data itself:
eigen(cov(X))$val


plot(pc_1, pc_2, col=iris[,5])

## Lets compare the PCA loadings try an artibrary orthonormal projection
P <- cbind(PC1=c(.5, .5, .5, .5),
           PC2=c(-.7071068, .7071068, 0, 0))
colSums(P^2)
crossprod(P)
Xproj <- X %*% P
par(mfrow=c(1,2))
plot(pc_1, pc_2, col=iris[,5], main='PCA')
plot(Xproj[,1], Xproj[,2], col=iris[,5], main='An orthogonal projection')

diag(cov(scores))[1:2]
## not variance maximizing
diag(cov(Xproj))

## One more cool thing about PCA:
loadings %*% diag(pca$sdev^2) %*% t(loadings)
## is ##
cov(X)

## this implies covariances between the features determines loadings
## e.g. features get linearly grouped as factor proportional to the magnitude of the loading coefficient
loadings

# peeking at the cross correlation can help explain contribution
cor(X, scores)


## Factor analysis
library(psych)
facta <- fa(X, nfactors = 4, fm='ols', rotate = 'none')
## notice how the diagram draws a causal arrow from the latent factor to the variable
## in FA the latent variables are considered an intrinsic feature of the data -
## some hidden variable (e.g. for iris, genes regulation?? that's causing the features)
## therefore FA separates out the unexplainable part of the variation (like OLS regression)
fa.diagram(facta, simple=FALSE, cut=.1)
facta$loadings

facta$loadings %*% t(facta$loadings) + facta$residual
cor(X)

## FA breaks each the data down factors (scores matrix) and loadings
var1_reconstruct <- facta$scores[,1]*loadings[1,1] + facta$scores[,2]*loadings[1,2] + facta$scores[,3]*loadings[1,3] + facta$scores[,4]*loadings[1,4]

## Note scores x loadings is close to but not exactly equal to the original data.
head(var1_reconstruct)
head(X[,1])

## or more compactly using matrix multiplication:
all.equal(
  X,
  X_reconstruct <- facta$scores %*% t(facta$loadings) ## + error
)
## The variance in each variable that cannot be accounted for by the factors:
facta$uniqueness
## equivilently:
1-diag(cov(X, X_reconstruct))


## alternative view of FA as a data "projection" like in PCA
all.equal(
  X %*% facta$weights,
  facta$scores
)

## weights are simply lm coefficients
(weights <- coef(lm(facta$scores[,1] ~ X))[-1])
# check that these are the same as:
facta$weights[,1]


# contribution of each original variable to the factor
rowSums(facta$loadings^2)

## Loadings can be orthogonal or oblique (by design) but need not by "orthonormal" like in PCA
t(facta$loadings) %*% facta$loadings

## compare results by visualization
par(mfrow=c(1,2))
plot(pca$x[,1:2], col=iris[,5], main='PCA')
plot(facta$scores[,1:2], col=iris[,5], main='FA', xlab="F1", ylab="F2")

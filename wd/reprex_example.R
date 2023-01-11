install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
library(tidyverse)
library(ggplot2)
library(lubridate)
install.packages("car")
install.packages("randomForest")
install.packages("caret")
install.packages("glmnet")

library(car)
library(randomForest)
library(caret)
library(glmnet)

install.packages("reprex")
library(reprex)

#reprex helps ask questions, reproducible 

y <- 1:4
mean(y)
#want to show output from console

reprex()

reprex(
  {
    y <- c(1:3, 'a')
    mean(y)
  }
)  

#this output, allows you to copy from clipboard onto email

# Enter your code here. Read input from STDIN. Print output to STDOUT
L<-scan("stdin")
N<-L[1]
L<-L[2:length(L)]

k<-1
j<-0
while(k<N){
  if(any(is.na(L[k+2]), L[k+2]==1)){
    k<-k+1
  }else{
    k<-k+2
  }
  j<-j+1
}
cat(j)




#share data as well as code
data(iris)
head(iris)

subiris <- iris[sample(1:nrow(iris), 50),]
saveRDS(subiris, 
        file = sprintf('subiris_%s.rds', format(Sys.time(), "%Y%m%d")))
load('subiris_20210112.rdata')


f <- scan("stdin")
a=f[-1]
b=unique(a)
sum=0
for(i in 1:length(b)){
  sum=sum+sum(a==b[i])%/%2
}
cat(sum)


L<-scan("stdin")
N<-L[1]
L<-L[2:length(L)]

k<-1
j<-0
while(k<N){
  if(any(is.na(L[k+2]), L[k+2]==1)){
    k<-k+1
  }else{
    k<-k+2
  }
  j<-j+1
}
cat(j)

sum(('50') + 20)

day <- "sat"
if(day == "sat") {
  print("It is Saturday")
  
}


x <- c(10, 20, 30)

class(x) <- "x_class"

x <- function(x, ...) {
  list(
    mean = mean(x), 
    median = median(x), 
    geomean = exp(mean(log(x)))
  )
}

summary(x)


add_numbers <- function(x, y) {
  x + y
}

add_numbers(3, 3)


x <- matrix(1:4, nrow = 2)
<- 1000
x

x <- NA
is.na(x)


calculate_bmi <- function(height, weight) {
  weight/(height^2) * 10000
}

calculate_bmi(173, 63)


calculate_subsidy <- function(n, d, l, min_efficiency, subsidy_factor) {
  efficiency = n*d/l
  if(efficiency < min_efficiency) {
    while(0)
  }
  efficiency * subsidy_factor
}

subsidy = calculate_subsidy(4, 75, 11, 25, 0.20)
cat("Subsidy $:", subsidy)
subsidy = calculate_subsidy(2, 60, 10.5, 25, 0.20)
cat("Subsidy $:", subsidy)

x <- c(-4, 9, 7, -2)
ifelse(x>0, "Positve", "Neg")
  
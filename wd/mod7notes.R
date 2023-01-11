####
# Association rules (market basket analysis)

#2/23

items <- readLines("transactions.csv")

items



library(arules)
library(arulesViz)

# digressin of R classes

attr(items, "class") <- 'transaction_list'
class(items)

#
items <- structure(items, class = "transaction_list")
list(items)

S3Class(items)


# fucntion

myprint <- function(items) {
  UseMethod("myprint", items)
}

myprint.transaction_list <- function(items){
  cat(paste(items, "\n", collapse = "\n"))
}

myprint.default <- function(items){
  print(items)

}

myprint(items)
myprint(as.character(items))


## what generic exists


### arules package 
items = read.transactions('transactions.csv',format='basket',sep=',')
as(items,'matrix')
class(items)
dim(items)[1]
dim(items)[2]

isS4(items)
slotNames(items)

itemFrequencyPlot(items, support = 0.0, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue2", las = 1, topN=5,
                  xlab = paste("Proportion of Market Baskets Containing Item"))





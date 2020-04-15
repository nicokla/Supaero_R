# K-nearest-neighboor
library(class)
#source('loadData.R')

knnDo <- function(train.in, train.out, test.in, k){
  test.out.estim <- knn(train.in, test.in, train.out, k=k)
  return(test.out.estim)
}





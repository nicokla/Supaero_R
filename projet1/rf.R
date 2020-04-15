library(randomForest)

#Classification
rf <- function(train, test.in){
  rf.cl<- randomForest(as.factor(V1) ~ ., train)
  pred.rf.cl=predict(rf.cl, test.in)
  return (pred.rf.cl)
}
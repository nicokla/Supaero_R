library(e1071)

naiveBayesDo <- function(train, test.in){
  classifieur<-naiveBayes(as.factor(V1)~. , train)
  test.out.estim=predict(classifieur, test.in)
  return (test.out.estim)
}



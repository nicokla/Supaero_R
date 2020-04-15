#install.packages('rpart')
library("adabag")
library("rpart")

boost <- function(train.in, train.out, test.in, mfinal){
  data.train=data.frame(chiffre=as.character(train.out), Pixels = train.in)
  data.test=data.frame(chiffre = as.character(test.out), Pixels = test.in)
  
  model=boosting(chiffre~., data=data.train, boos=TRUE, mfinal=mfinal)
  pred=predict(model, data.test);
#   tab=pred$confusion
#   erreur=(pred$error)*100
  return (pred$class)
}

#############
#install.packages('ada')
#library("ada")
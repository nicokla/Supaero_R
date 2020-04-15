#library('MASS')
# library('grid')
#install.packages('neuralnet')
library('neuralnet')
# library('class')
library('nnet')

#Inspiré de 
#http://stackoverflow.com/questions/21827195/unexpected-output-while-using-neuralnet-in-r

#setwd("~/Desktop/Cours Rachelson/6) BE_Machine_Learning")
# source('loadData.R')

nn1 <- function(trainEquilibre.2, test.in, nHidden){
  output.names = colnames(trainEquilibre.2[,1:10])
  input.names = colnames(trainEquilibre.2[,11:266])
  myFormula = as.formula(paste0(paste0(output.names,collapse='+')," ~ ", 
                                 paste0(input.names, collapse="+")))
  NN1 = neuralnet(formula = myFormula, data = trainEquilibre.2, hidden = c(nHidden), 
  algorithm='rprop+', linear.output=FALSE,learningrate = 0.01, threshold=0.01,
  err.fct="ce", act.fct="logistic", lifesign="full", lifesign.step=20,
  stepmax=dim(trainEquilibre.2)[1])
  
  test.out.2.estim = compute(NN1, test.in)
  test.out.2.estim=test.out.2.estim[[2]]
  test.out.estim=(0:9)[apply(test.out.2.estim,1,which.max)]
  return (test.out.estim)
}


##############

nn2 <- function(trainEquilibre.2, test.in, nHidden){
  NN2 = nnet(trainEquilibre.2[,11:266], 
  trainEquilibre.2[,1:10], size=70,
  softmax=TRUE, MaxNWts=100000000,decay = 5e-7, maxit = 200)
  test.out.estim2=predict(NN2, test.in, type = "class")
  return (test.out.estim2)
}




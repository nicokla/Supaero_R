library('MASS')
#source('loadData.R')

qdaDo <- function(trainEquilibre.in,trainEquilibre.out, test.in, dimension){
  acp = prcomp(trainEquilibre.in, center = FALSE, scale. = FALSE)
  b=solve(acp$rotation)
  trainEquilibre.in.simplifie = acp$x[,1:dimension];
  
  z <- qda(trainEquilibre.in.simplifie, factor(trainEquilibre.out))
  test.in.simplifie=t(b%*%t(test.in))
  test.in.simplifie=test.in.simplifie[,1:dimension]
  test.out.estim=predict(z,test.in.simplifie)$class
  return (test.out.estim)
}
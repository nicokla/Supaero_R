#source('loadData.R')

acp = princomp(trainEquilibre.in, cor = T, scores = T)
summary(acp)
#38 70 100 --> 80 90 95
# biplot(acp)
plot.new()
plot(acp$scores[,1:2],type='n')
couleurOfDigit <- function(digit){
  #   answer=digit+1;
  if(digit==0){
    answer='blue';
  }else if (digit==1){
    answer='red'
  }else if (digit==2){
    answer='orange'
  }else if (digit==3){
    answer='green';
  }else if (digit==4){
    answer='brown';
  }else if (digit==5){
    answer='cyan';
  }else if (digit==6){
    answer='grey'
  }else if (digit==7){
    answer='black'
  }else if (digit==8){
    answer='yellow'
  }else {#if (digit==9){
    answer='purple'
  }
  return(answer)
}

colors=sapply(trainEquilibre.out,couleurOfDigit)
text(acp$scores[,c(1,2)],labels=trainEquilibre.out,col=colors)

# install.packages('rgl')
library(rgl)
x=acp$scores[,1]->x
y=acp$scores[,2]->y
z=acp$scores[,3]->z
text3d(x,y,z, text=trainEquilibre.out, col=colors)



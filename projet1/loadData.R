
library('nnet')

# http://statweb.stanford.edu/~tibs/ElemStatLearn/
# See data section for this data set and others
# or here : http://cran.r-project.org/web/packages/ElemStatLearn/index.html

train.all = read.table("zip.train", header=FALSE,sep=" ");
train.all=train.all[,-258]
nbExampleMax = dim(train.all)[1]
nbExample = nbExampleMax;
nbExample=min(nbExample,nbExampleMax)
order.all = sample(1:nbExampleMax)
order = order.all[1:nbExample]
train = train.all[order,]
train.in  = train[,-1];
m=dim(train.in)[1]
n=dim(train.in)[2]
train.out = train[,1];
train.out.2 = class.ind(train.out);
colnames(train.out.2)<-paste0('out.',colnames(train.out.2))
train.2 <-cbind(train.out.2,train.in)

test = read.table("zip.test", header=FALSE,sep=" ");
test.out = test[,1];
test.in  = test[,-1];

M=list()
for ( i in 0:9){
  M[[i+1]]=train.in[train.out==i,]
  M[[i+1]]=as.matrix(M[[i+1]]);
}

Nombre=seq(0,0,length=10)
for ( i in 1:10){
  Nombre[i]=dim(M[[i]])[1]
}

mini=min(Nombre)
trainEquilibre=as.data.frame(matrix(0,nrow=10*mini,ncol=n+1))
for (i in 0:9){
  trainEquilibre[seq(i+1,10*mini,by=10),-1]=M[[(i%%10)+1]][1:mini,]
  trainEquilibre[seq(i+1,10*mini,by=10),1]=i;
}
trainEquilibre.in=trainEquilibre[,-1];
trainEquilibre.out=trainEquilibre[,1];
trainEquilibre.out.2 = class.ind(trainEquilibre.out);
colnames(trainEquilibre.out.2)<-paste0('out.',colnames(trainEquilibre.out.2))
trainEquilibre.2 <-cbind(trainEquilibre.out.2,trainEquilibre.in)

# displayResult <- function (test.out, test.out.estim){
#   table(test.out, test.out.estim)
#   sum(test.out.estim!=test.out)/length(test.out)*100
# }



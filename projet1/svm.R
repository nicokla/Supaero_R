## SVM
#install.packages('e1071')
library(e1071)
#source('loadData.R')

##########################
# One vs one

svm.1V1 <- function(train, test.in, facteur, marginInverse) {
  model = svm(as.factor(V1) ~ ., train, 
                     gamma=facteur/256, cost=marginInverse, type='C-classification')
  test.out.estim= predict(model, test.in)
  return (test.out.estim)
}


##########################
# One vs all

svm.1VA <- function(train, test.in, facteur, marginInverse) {
  
pred=matrix(0,nrow=dim(test)[1],ncol=10)

for (i0 in 0:9){
  N_total=4000;
  nn=round(N_total/2);
  nn=min(nn,dim(M[[i0+1]])[1])  # M[[1+i]] 
  mm=round(N_total/18)
  mm=min(mm,min(Nombre))
  
  Nombre2=seq(0,0,length=10)
  M2=list()
  for(i in 0:9){
    M2[[i+1]]=M[[i+1]][sample(1:Nombre[i+1]),]
    if (i== i0) {
      M2[[i+1]]=M2[[i+1]][1:nn,]
      Nombre2[i+1]=nn
    }
    else{ 
      M2[[i+1]]=M2[[i+1]][1:mm,]
      Nombre2[i+1]=mm
    }
  }
  
  train.in.i0=t(as.matrix(seq(0,0, length=dim(M2[[1]])[2])))
  train.out.i0=as.matrix(0)
  
  for ( i in 0:9){
    train.in.i0=rbind(train.in.i0,as.matrix(M2[[i+1]]));
    if(i==i0){
      v=as.matrix(seq(1,1,length=dim(M2[[i+1]])[1]))
    }
    else{
      v=as.matrix(seq(-1,-1,length=dim(M2[[i+1]])[1]))
    }
    train.out.i0=rbind(train.out.i0,v);
  }
  train.in.i0=train.in.i0[-1,]
  train.out.i0=train.out.i0[-1]
  
  data_svm=cbind(train.out.i0,train.in.i0)
  data_svm=as.data.frame(data_svm)
  colnames(data_svm)
  colnames(data_svm)[1]='V1'
  
  model = svm(V1 ~ .,data=data_svm,gamma=facteur/256, cost=marginInverse)
  
  pred[,i0+1]= as.matrix(predict(model, test.in))
}


test.out.estim=apply(pred,1,which.max)-1
return (test.out.estim)
}



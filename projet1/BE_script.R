##################
# Load data
setwd('/Users/nicolas/Desktop/programmation/R/3A/bes/BE_MachineLearning/code/')
source('loadData.R')

#################
# Data visualisation with ACP
source('acp.R')


################
###############
# Classifications

source('naiveBayes.R')
source('svm.R')
source('nn.R')
source('rf.R')
source('boosting.R')
source('knn.R')
source('qda.R')
source('plotDigit.R')

perfo <- function (test.out, test.out.estim){
  confusionMatrix=table(test.out,test.out.estim)
  errorRate = sum(test.out.estim!=test.out)/length(test.out)*100
  return(list("confusionMatrix"=confusionMatrix,
              "errorRate"=errorRate))
}

###############
# Naive Bayes

#test.out.estim.naiveBayes = naiveBayesDo(train, test.in)
#save(test.out.estim.naiveBayes, file="sauvegardes/test.out.estim.naiveBayes")
load("sauvegardes/test.out.estim.naiveBayes")
perfo.naiveBayes = perfo(test.out, test.out.estim.naiveBayes)


#####################
# SVM one-vs-one and SVM one-vs-all

facteur=0.65 #if margin ==1 : facteur==0.65 --> 5.78%,  0.68 --> 5.82, 0.63 --> 5.78, 0.58 --> 5.88
# if marginInverse == 5 : facteur==0.65 --> 4.83, 0.5 --> 5.13, 0.8 --> 4.93
marginInverse=5 # if facteur==0.65 : marginInverse==1.2 --> 5.53%, 1.5 -->5.43
# 3 --> 4.93, 6--> 4.88, 12 --> 5.13, 4.5 --> 4.88, 5 --> 4.83

#test.out.estim.svm.1V1 = svm.1V1(train, test.in, facteur, marginInverse)
#save(test.out.estim.svm.1V1, file="sauvegardes/test.out.estim.svm.1V1")
load("sauvegardes/test.out.estim.svm.1V1")
perfo.svm.1V1 = perfo(test.out, test.out.estim.svm.1V1)

#test.out.estim.svm.1VA = svm.1VA(train, test.in, facteur, marginInverse)
#save(test.out.estim.svm.1VA, file="sauvegardes/test.out.estim.svm.1VA")
load("sauvegardes/test.out.estim.svm.1VA")
perfo.svm.1VA = perfo(test.out, test.out.estim.svm.1VA)


###################
# Neural network

nHidden=400
#test.out.estim.nn1 = nn1(trainEquilibre.2, test.in, nHidden)
#save(test.out.estim.nn1, file="sauvegardes/test.out.estim.nn1")
load("sauvegardes/test.out.estim.nn1")
perfo.nn1 = perfo(test.out, test.out.estim.nn1)


####################
# Random forest

#test.out.estim.rf = rf(train, test.in)
#save(test.out.estim.rf, file="sauvegardes/test.out.estim.rf")
load("sauvegardes/test.out.estim.rf")
perfo.rf = perfo(test.out, test.out.estim.rf)


###################
# Tree boosting

mfinal = 10
#test.out.estim.boost = boost(train.in, train.out, test.in, mfinal)
#save(test.out.estim.boost, file="sauvegardes/test.out.estim.boost")
load("sauvegardes/test.out.estim.boost")
perfo.boost = perfo(test.out, test.out.estim.boost)


######################
# K-nearest neighboor

k=3
#test.out.estim.knn = knnDo(train.in, train.out, test.in, k)
#save(test.out.estim.knn, file="sauvegardes/test.out.estim.knn")
load("sauvegardes/test.out.estim.knn")
perfo.knn = perfo(test.out, test.out.estim.knn)


#####################
# Quadratic discriminant analysis

dimension=100
#test.out.estim.qda = qdaDo(trainEquilibre.in,trainEquilibre.out, test.in, dimension)
#save(test.out.estim.qda, file="sauvegardes/test.out.estim.qda")
load("sauvegardes/test.out.estim.qda")
perfo.qda = perfo(test.out, test.out.estim.qda)


#################
# Vote of all methods 
# with weights higher for better methods
# Weights proportionnal to 1/errorRate
# But in theory we don't know the errorRates in advance --> moyenne non pond?r?e.
# Ici on prend juste les 4 meilleures m?thodes : les 2 svms, random forest et knn.

# errors = c(perfo.svm.naiveBayes, perfo.svm.1V1, perfo.svm.1VA, 
#                perfo.nn1, perfo.rf, perfo.boost,
#                perfo.knn, perfo.qda);
# predAll = cbind(test.out.estim.naiveBayes,
# test.out.estim.svm.1V1,
# test.out.estim.svm.1VA,
# test.out.estim.nn1,
# test.out.estim.rf,
# test.out.estim.boost,
# test.out.estim.knn,
# test.out.estim.qda);
# # predAll=as.data.frame(predAll)
# weights=1:(length(errors)/2)
# for (i in 1:(length(errors)/2)){
#   weights[i]=1/(errors[2*i]$errorRate)
# }
# pred=matrix(0,nrow=dim(test)[1],ncol=10)
# for (i in 1:dim(pred)[1]){
#   for (j in 1:10){
#     for (m in c(2,3,5,7)){
#       pred[i,j]=pred[i,j]+weights[m]*(predAll[i,m]==j)
#     }
#   }
# }
# test.out.estim.vote = matrix(0,nrow=dim(test)[1],ncol=1)
# for (i in 1:length(test.out.estim.vote)){
#   test.out.estim.vote[i]=which.max(pred[i,])-1
# }

#save(test.out.estim.vote, file="sauvegardes/test.out.estim.vote")
load("sauvegardes/test.out.estim.vote")
perfo.vote = perfo(test.out, test.out.estim.vote)
# 4.73 %

###############
# Plot images difficult to recognize
readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

nbErreurs = sum(test.out != test.out.estim.vote)
k=0
for(i in 1:length(test.out)){
  if(test.out[i] != test.out.estim.vote[i]){
    k=k+1;
    cat("Erreur numero", k, "sur", nbErreurs,'\n')
    cat("True class : ", test.out[i],'\n')
    cat("Supposed class : ", test.out.estim.vote[i],'\n\n')
    display(test.in, i)
    readkey();
  }  
}





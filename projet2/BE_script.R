
###
# Load data

data<-read.table("body.csv",sep=";",dec=",",header=TRUE)
n_old=ncol(data)
data2=as.matrix(data[,-n_old])
m=nrow(data2) # number of samples
sexesLettres=as.matrix(data[,n_old])
namesVar=as.matrix(names(data))

########
# Acp
acp = princomp(data2, cor = T, scores = T)
summary(acp)

plot(acp)

colors=rep("blue",times=m)
colors[sexesLettres=="F"]="red"
plot.new()
plot(acp$scores[,1:2],type='n')
text(acp$scores[,c(1,2)],labels=1:m,col=colors)
abline(h=0,v=0,lty=2)

directions=acp$loadings[,1:2]
for(i in 1:24){
  x1=acp$loadings[i,1]
  y1=acp$loadings[i,2]
  directions[i,1]=10*x1;
  directions[i,2]=10*y1;
}
interestingParameters4=c(10,11,14,16,22,23,24);
for (i in interestingParameters4){
  arrows(0,0,directions[i,1],directions[i,2],lwd=4,cex=1)
}
text(directions[interestingParameters4,],
     labels = namesVar[interestingParameters4], lwd=8,cex=1.2, col="green")

biplot(acp)
abline(h=0,v=0,lty=2)

####################"

plot.new()
par(mfrow=c(3,1))
boxplot(data$height, horizontal=TRUE);
boxplot(data$height[sexesLettres=="F"],horizontal=TRUE);
boxplot(data$height[sexesLettres=="M"],horizontal=TRUE);

#####################
par(mfrow=c(3,1)) 
hist(data$height,xlim=c(150,200));
hist(data$height[sexesLettres=="F"],xlim=c(150,200));
hist(data$height[sexesLettres=="M"],xlim=c(150,200));


#########################
sexesNb=rep(0,times=m)
sexesNb[sexesLettres=="F"]=1
#247 hommes, 260 femmes
sexesNb2 = matrix(sexesNb)
data4 =cbind(data2,sexesNb2)
correlations_all=cor(data4);
correlations_all[,n_old]


#######################
correlations_all2=correlations_all;
correlations_all2[correlations_all==1]=0
which(correlations_all2 == max(correlations_all2), arr.ind = TRUE)
#forearm.girth et bicep.girth (16 et 17)
correlations_all2[16,17] #--> 0.9423755


################
interestingParameters=c(10,11,14,16,23,24);
data_select=data[,interestingParameters]
data3=cbind(data_select,sexesNb2)
pairs(data3);



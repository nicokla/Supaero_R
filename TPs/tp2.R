### R code from vignette source 'tp2.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp2.Rnw:23-24
###################################################
data = read.table("body.csv",header=T,sep=";",dec=",")
m=nrow(data) # number of samples
n=ncol(data) # number of features
data2=as.matrix(data[,-n])
sexesLettres=as.matrix(data[,n])
sexesNb=rep(0,times=m)
sexesNb[sexesLettres=="F"]=1
sexesNb = matrix(sexesNb)
data.body=cbind(data2,sexesNb)
namesVar=names(data)


###################################################
### code chunk number 2: tp2.Rnw:29-30
###################################################
res.acp.body = prcomp(data.body)


###################################################
### code chunk number 3: tp2.Rnw:35-38
###################################################
class(res.acp.body)
mode(res.acp.body)
is.list(res.acp.body)


###################################################
### code chunk number 4: tp2.Rnw:42-43
###################################################
names(res.acp.body)


###################################################
### code chunk number 5: tp2.Rnw:46-47
###################################################
res.acp.body$sdev


###################################################
### code chunk number 6: tp2.Rnw:53-54
###################################################
summary(res.acp.body)


###################################################
### code chunk number 7: tp2.Rnw:59-60
###################################################
plot(res.acp.body)


###################################################
### code chunk number 8: tp2.Rnw:65-66
###################################################
biplot(res.acp.body)


###################################################
### code chunk number 9: tp2.Rnw:71-75
###################################################
sexe = rep(c(1,2),each=10)
plot(res.acp.body$x,type="n")
abline(h=0,v=0,lty=2)
text(res.acp.body$x,rownames(data.body),col=sexe)


###################################################
### code chunk number 10: tp2.Rnw:86-87
###################################################
data.athle <- read.table("Data/athle.csv",header=T,dec=",",row.names=1)


###################################################
### code chunk number 11: tp2.Rnw:92-95
###################################################
res.acp.athle <- prcomp(data.athle)
plot(res.acp.athle)
#biplot(res.acp.athle)


###################################################
### code chunk number 12: tp2.Rnw:106-107
###################################################
M <- matrix(sample(1:15,size=15),ncol=3)


###################################################
### code chunk number 13: tp2.Rnw:113-115
###################################################
res.acp <- prcomp(M)
biplot(res.acp)


###################################################
### code chunk number 14: tp2.Rnw:119-121
###################################################
res.acp
summary(res.acp)


###################################################
### code chunk number 15: tp2.Rnw:126-128
###################################################
eigen(cov(M))
eigen(cov(M))$values**0.5


###################################################
### code chunk number 16: tp2.Rnw:132-134
###################################################
res.acp$rotation
eigen(cov(M))$vectors


###################################################
### code chunk number 17: tp2.Rnw:139-142
###################################################
cov(M)
Mc <- scale(M,scale=F)
1/(nrow(Mc)-1)*t(Mc)%*%Mc


###################################################
### code chunk number 18: tp2.Rnw:147-148
###################################################
s = svd(Mc) ; s


###################################################
### code chunk number 19: tp2.Rnw:153-156
###################################################
s$v
res.acp$rotation
eigen(cov(M))$vectors


###################################################
### code chunk number 20: tp2.Rnw:161-164
###################################################
s$d / sqrt(nrow(M)-1)
res.acp$sdev
sqrt(eigen(cov(M))$values)


###################################################
### code chunk number 21: tp2.Rnw:168-169
###################################################
res.acp$x


###################################################
### code chunk number 22: tp2.Rnw:173-175
###################################################
Mc%*%eigen(cov(M))$vectors
Mc%*%res.acp$rotation



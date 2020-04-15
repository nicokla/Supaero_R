### R code from vignette source 'tp1.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp1.Rnw:12-14
###################################################
data(USArrests)
help(USArrests)


###################################################
### code chunk number 2: tp1.Rnw:19-20
###################################################
dim(USArrests)


###################################################
### code chunk number 3: tp1.Rnw:24-25
###################################################
summary(USArrests)


###################################################
### code chunk number 4: tp1.Rnw:29-30
###################################################
boxplot(USArrests)


###################################################
### code chunk number 5: tp1.Rnw:34-35
###################################################
apply(USArrests,2,var)


###################################################
### code chunk number 6: tp1.Rnw:39-40
###################################################
pairs(USArrests)


###################################################
### code chunk number 7: tp1.Rnw:44-48
###################################################
var(USArrests)
cov(USArrests)
cor(USArrests)
cor(USArrests,method="spearman")


###################################################
### code chunk number 8: tp1.Rnw:52-54
###################################################
library(corrplot)
corrplot(cor(USArrests))


###################################################
### code chunk number 9: tp1.Rnw:61-63
###################################################
data(iris)
data(iris3)


###################################################
### code chunk number 10: tp1.Rnw:70-71
###################################################
apply(iris3,c(2,3),mean)


###################################################
### code chunk number 11: tp1.Rnw:74-75
###################################################
apply(iris[,1:4], 2, function(x){tapply(x,iris$Species,mean)})


###################################################
### code chunk number 12: tp1.Rnw:79-82
###################################################
par(mfrow=c(1,4))
for (i in 1:4) boxplot(iris[,i]~iris$Species,main=colnames(iris)[i])
par(mfrow=c(1,1))


###################################################
### code chunk number 13: tp1.Rnw:87-89
###################################################
plot(iris[,3],iris[,4],pch=16,col=iris$Species,xlab="Petal.Length",ylab="Petal.Width")
legend("bottomright",as.character(unique(iris$Species)), pch=16, col=1:3)



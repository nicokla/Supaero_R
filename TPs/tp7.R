### R code from vignette source 'tp7.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp7.Rnw:22-23
###################################################
help(cars)


###################################################
### code chunk number 2: tp7.Rnw:28-30
###################################################
res.lm <- lm(dist~speed,data=cars)
summary(res.lm)


###################################################
### code chunk number 3: tp7.Rnw:35-36
###################################################
cov(cars$dist,cars$speed)/var(cars$speed)


###################################################
### code chunk number 4: tp7.Rnw:41-44
###################################################
X <- cbind(rep(1,50),cars$speed)
Y <- cars$dist
solve(t(X)%*%X)%*%t(X)%*%Y


###################################################
### code chunk number 5: tp7.Rnw:49-51
###################################################
cor(cars$dist,cars$speed)**2
var(res.lm$fitted.values)/var(cars$dist)


###################################################
### code chunk number 6: tp7.Rnw:57-59
###################################################
plot(cars,pch=16,cex=2,col=rainbow(50))
abline(res.lm,lty=4,lwd=6,col="lightgrey")


###################################################
### code chunk number 7: tp7.Rnw:69-70
###################################################
help(swiss)


###################################################
### code chunk number 8: tp7.Rnw:75-76
###################################################
res.lm2 <- lm(Fertility ~ ., data=swiss)


###################################################
### code chunk number 9: tp7.Rnw:80-83
###################################################
X <- as.matrix(cbind(rep(1,47),swiss[,-1]))
Y <- swiss$Fertility
solve(t(X)%*%X)%*%t(X)%*%Y


###################################################
### code chunk number 10: tp7.Rnw:88-90
###################################################
select.res.lm2 <- step(res.lm2)
summary(select.res.lm2)


###################################################
### code chunk number 11: tp7.Rnw:104-110
###################################################
par(mfrow=c(2,2))
boxplot(Sepal.Length~Species,data=iris,col=c("magenta","orange","cyan"))
boxplot(Sepal.Width~Species,data=iris,col=c("magenta","orange","cyan"))
boxplot(Petal.Length~Species,data=iris,col=c("magenta","orange","cyan"))
boxplot(Petal.Width~Species,data=iris,col=c("magenta","orange","cyan"))
par(mfrow=c(1,1))


###################################################
### code chunk number 12: tp7.Rnw:117-118
###################################################
summary(aov(lm(Sepal.Length~Species,data=iris)))


###################################################
### code chunk number 13: tp7.Rnw:121-124
###################################################
summary(aov(lm(Sepal.Width~Species,data=iris)))
summary(aov(lm(Petal.Length~Species,data=iris)))
summary(aov(lm(Petal.Width~Species,data=iris)))



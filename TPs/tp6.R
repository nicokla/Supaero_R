### R code from vignette source 'tp6.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp6.Rnw:21-23
###################################################
sample.1 <- rnorm(10)
sample.2 <- rnorm(10)


###################################################
### code chunk number 2: tp6.Rnw:28-29
###################################################
result.test <- t.test(sample.1,sample.2,var.equal=TRUE)


###################################################
### code chunk number 3: tp6.Rnw:34-35
###################################################
result.test


###################################################
### code chunk number 4: tp6.Rnw:44-46
###################################################
s.X1X2 <- sqrt((var(sample.1)+var(sample.2))/2)
t.X1X2 <- (mean(sample.1)-mean(sample.2))/(s.X1X2*sqrt(2/10))


###################################################
### code chunk number 5: tp6.Rnw:51-52
###################################################
result.test$p.value


###################################################
### code chunk number 6: tp6.Rnw:57-58
###################################################
pt(t.X1X2,18)*2
(1-pt(t.X1X2,18))*2


###################################################
### code chunk number 7: tp6.Rnw:63-82
###################################################
xx <- seq(-3,3,length=100)
plot(xx,dt(xx,18),type="l",lwd=2)
if (t.X1X2 < 0)
{
xx1 <- seq(-3,t.X1X2,length=50)
yy1 <- dt(xx1,18)
xx2 <- seq(-t.X1X2,3,length=50)
yy2 <- dt(xx2,18)
polygon(c(xx1,rev(xx1)),c(yy1,rep(0,50)),col="red")
polygon(c(xx2,rev(xx2)),c(yy2,rep(0,50)),col="red")
} else
{
xx1 <- seq(t.X1X2,3,length=50)
yy1 <- dt(xx1,18)
xx2 <- seq(-3,-t.X1X2,length=50)
yy2 <- dt(xx2,18)
polygon(c(xx1,rev(xx1)),c(yy1,rep(0,50)),col="red")
polygon(c(xx2,rev(xx2)),c(yy2,rep(0,50)),col="red")
}  


###################################################
### code chunk number 8: tp6.Rnw:92-99
###################################################
matrice <- matrix(nrow=100,ncol=21)
for (i in 1:100)
{
  matrice[i,1:10] <- rnorm(10)
  matrice[i,11:20] <- rnorm(10)
  matrice[i,21] <- t.test(matrice[i,1:10], matrice[i,11:20],var.equal=TRUE)$p.value
}


###################################################
### code chunk number 9: tp6.Rnw:104-105
###################################################
sum(matrice[,21]<0.05)


###################################################
### code chunk number 10: tp6.Rnw:110-111
###################################################
indice <- which.min(matrice[,21])


###################################################
### code chunk number 11: tp6.Rnw:121-122 (eval = FALSE)
###################################################
## plot(dnorm,-3,3,type="l")


###################################################
### code chunk number 12: tp6.Rnw:126-132
###################################################
plot(dnorm,-3,3,type="l",lwd=2)
rug(matrice[indice,1:10],col="blue",lwd=2,ticksize=0.1)
rug(matrice[indice,11:20],col="red",lwd=2,ticksize=0.1)
rug(mean(matrice[indice,1:10]),col="blue",ticksize=0.2,lwd=3,lty=2)
rug(mean(matrice[indice,11:20]),col="red",ticksize=0.2,lwd=3,lty=2)
legend("topleft",c("Sample 1","Sample 2"),col=c("blue","red"),lty=1)



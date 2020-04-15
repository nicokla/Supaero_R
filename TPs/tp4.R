### R code from vignette source 'tp4.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp4.Rnw:20-23
###################################################
# install.packages("pixmap")
library(pixmap)
MonImage <- read.pnm("Lena.ppm")


###################################################
### code chunk number 2: tp4.Rnw:28-29
###################################################
plot(MonImage)


###################################################
### code chunk number 3: tp4.Rnw:34-36
###################################################
slotNames(MonImage)
MonImage@channels


###################################################
### code chunk number 4: tp4.Rnw:44-45
###################################################
MonImageRouge <- as.vector(MonImage@red)


###################################################
### code chunk number 5: tp4.Rnw:48-51
###################################################
MonImageVert <- as.vector(MonImage@green)
MonImageBleu <- as.vector(MonImage@blue)
MonImageCanaux <- cbind(MonImageRouge,MonImageVert,MonImageBleu)


###################################################
### code chunk number 6: tp4.Rnw:59-60
###################################################
summary(MonImageCanaux)


###################################################
### code chunk number 7: tp4.Rnw:65-66
###################################################
boxplot(MonImageCanaux)


###################################################
### code chunk number 8: tp4.Rnw:71-78
###################################################
par(mfrow=c(1,3))
hist(MonImageRouge,prob=TRUE,col="red")
lines(density(MonImageRouge),col="red",lwd=2)
hist(MonImageVert,prob=TRUE,col="green")
lines(density(MonImageVert),col="green",lwd=2)
hist(MonImageBleu,prob=TRUE,col="blue")
lines(density(MonImageBleu),col="blue",lwd=2)


###################################################
### code chunk number 9: tp4.Rnw:89-93
###################################################
par(mfrow=c(1,3))
plot(MonImageRouge,MonImageVert,pch=".")
plot(MonImageRouge,MonImageBleu,pch=".")
plot(MonImageVert,MonImageBleu,pch=".")


###################################################
### code chunk number 10: tp4.Rnw:99-100
###################################################
round(cor(MonImageCanaux),3)


###################################################
### code chunk number 11: tp4.Rnw:111-115 (eval = FALSE)
###################################################
## MonImageRouge <- as.vector(MonImage@red)
## MonImageVert <- as.vector(MonImage@green)
## MonImageBleu <- as.vector(MonImage@blue)
## MonImageCanaux <- cbind(MonImageRouge,MonImageVert,MonImageBleu)


###################################################
### code chunk number 12: tp4.Rnw:124-127
###################################################
res.km.2 <- kmeans(MonImageCanaux,centers=2)
res.km.4 <- kmeans(MonImageCanaux,centers=4)
res.km.8 <- kmeans(MonImageCanaux,centers=8)


###################################################
### code chunk number 13: tp4.Rnw:132-133
###################################################
MonImage.km.grey.2 <- pixmapGrey(res.km.2$cluster,nrow=512)


###################################################
### code chunk number 14: tp4.Rnw:135-137
###################################################
MonImage.km.grey.4 <- pixmapGrey(res.km.4$cluster,nrow=512)
MonImage.km.grey.8 <- pixmapGrey(res.km.8$cluster,nrow=512)


###################################################
### code chunk number 15: tp4.Rnw:142-146
###################################################
par(mfrow=c(1,3))
plot(MonImage.km.grey.2,main="2 groupes")
plot(MonImage.km.grey.4,main="4 groupes")
plot(MonImage.km.grey.8,main="8 groupes")


###################################################
### code chunk number 16: tp4.Rnw:158-160
###################################################
library(FactoMineR)
res.pca <- PCA(MonImageCanaux,graph=FALSE,scale.unit=FALSE)


###################################################
### code chunk number 17: tp4.Rnw:165-170
###################################################
rec.1cp <- reconst(res.pca, ncp=1)
ImageRec.1cp <- MonImage
ImageRec.1cp@red <- matrix(rec.1cp[,1],nrow(MonImage@red),ncol(MonImage@red))
ImageRec.1cp@green <- matrix(rec.1cp[,2],nrow(MonImage@green),ncol(MonImage@green))
ImageRec.1cp@blue <- matrix(rec.1cp[,3],nrow(MonImage@blue),ncol(MonImage@blue))


###################################################
### code chunk number 18: tp4.Rnw:175-181
###################################################
ImageRec.1cp@red[ImageRec.1cp@red<0]=0
ImageRec.1cp@green[ImageRec.1cp@green<0]=0
ImageRec.1cp@blue[ImageRec.1cp@blue<0]=0
ImageRec.1cp@red[ImageRec.1cp@red>1]=1
ImageRec.1cp@green[ImageRec.1cp@green>1]=1
ImageRec.1cp@blue[ImageRec.1cp@blue>1]=1


###################################################
### code chunk number 19: tp4.Rnw:184-185
###################################################
plot(ImageRec.1cp)


###################################################
### code chunk number 20: tp4.Rnw:190-212
###################################################
rec.2cp <- reconst(res.pca, ncp=2)
ImageRec.2cp <- MonImage
ImageRec.2cp@red <- matrix(rec.2cp[,1],nrow(MonImage@red),ncol(MonImage@red))
ImageRec.2cp@green <- matrix(rec.2cp[,2],nrow(MonImage@green),ncol(MonImage@green))
ImageRec.2cp@blue <- matrix(rec.2cp[,3],nrow(MonImage@blue),ncol(MonImage@blue))
ImageRec.2cp@red[ImageRec.2cp@red<0]=0
ImageRec.2cp@green[ImageRec.2cp@green<0]=0
ImageRec.2cp@blue[ImageRec.2cp@blue<0]=0
ImageRec.2cp@red[ImageRec.2cp@red>1]=1
ImageRec.2cp@green[ImageRec.2cp@green>1]=1
ImageRec.2cp@blue[ImageRec.2cp@blue>1]=1
rec.3cp <- reconst(res.pca, ncp=3)
ImageRec.3cp <- MonImage
ImageRec.3cp@red <- matrix(rec.3cp[,1],nrow(MonImage@red),ncol(MonImage@red))
ImageRec.3cp@green <- matrix(rec.3cp[,2],nrow(MonImage@green),ncol(MonImage@green))
ImageRec.3cp@blue <- matrix(rec.3cp[,3],nrow(MonImage@blue),ncol(MonImage@blue))
ImageRec.3cp@red[ImageRec.3cp@red<0]=0
ImageRec.3cp@green[ImageRec.3cp@green<0]=0
ImageRec.3cp@blue[ImageRec.3cp@blue<0]=0
ImageRec.3cp@red[ImageRec.3cp@red>1]=1
ImageRec.3cp@green[ImageRec.3cp@green>1]=1
ImageRec.3cp@blue[ImageRec.3cp@blue>1]=1


###################################################
### code chunk number 21: tp4.Rnw:215-220
###################################################
par(mfrow=c(1,4))
plot(MonImage,main="Image initiale")
plot(ImageRec.1cp,main="Image rec. 1 CP")
plot(ImageRec.2cp,main="Image rec. 2 CP")
plot(ImageRec.3cp,main="Image rec. 3 CP")



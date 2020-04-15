### R code from vignette source 'tp3.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tp3.Rnw:25-26
###################################################
data.body = read.table("body.csv",header=T,sep=";",dec=",",row.names=1)


###################################################
### code chunk number 2: tp3.Rnw:31-32
###################################################
distances <- dist(data.body)


###################################################
### code chunk number 3: tp3.Rnw:39-40
###################################################
hc.indiv <- hclust(distances,method="ward")


###################################################
### code chunk number 4: tp3.Rnw:45-46
###################################################
plot(hc.indiv)


###################################################
### code chunk number 5: tp3.Rnw:51-52
###################################################
indiv.4groupes <- cutree(hc.indiv,k=4)


###################################################
### code chunk number 6: tp3.Rnw:55-57
###################################################
plot(hc.indiv)
rect.hclust(hc.indiv,k=4,border=1:4)


###################################################
### code chunk number 7: tp3.Rnw:62-63
###################################################
res.acp.body = prcomp(data.body)


###################################################
### code chunk number 8: tp3.Rnw:68-73
###################################################
par(pty="s") # pour avoir une zone graphique carrée
plot(res.acp.body$x,type="n")
abline(h=0,v=0,lty=2)
text(res.acp.body$x,rownames(data.body))
par(pty="m") # pour revenir à la configuration par défaut


###################################################
### code chunk number 9: tp3.Rnw:78-83
###################################################
par(pty="s")
plot(res.acp.body$x,type="n")
abline(h=0,v=0,lty=2)
text(res.acp.body$x,rownames(data.body),col=indiv.4groupes,font=2)
par(pty="m")


###################################################
### code chunk number 10: tp3.Rnw:94-96
###################################################
res1.kmeans <- kmeans(data.body,centers=4)
res2.kmeans <- kmeans(data.body,centers=4)


###################################################
### code chunk number 11: tp3.Rnw:101-102
###################################################
table(res1.kmeans$cluster,res2.kmeans$cluster,dnn=c("kmeans1","kmeans2"))


###################################################
### code chunk number 12: tp3.Rnw:107-108
###################################################
table(res1.kmeans$cluster,indiv.4groupes,dnn=c("kmeans1","CAH"))


###################################################
### code chunk number 13: tp3.Rnw:119-122
###################################################
mat.centres.init <- matrix(nrow=4,ncol=5)
for (i in 1:4) mat.centres.init[i,]=apply(data.body[indiv.4groupes==i,],2,mean)
km.body.init <- kmeans(data.body,centers=mat.centres.init)


###################################################
### code chunk number 14: tp3.Rnw:126-127
###################################################
table(km.body.init$cluster,indiv.4groupes,dnn=c("kmeans","cah"))


###################################################
### code chunk number 15: tp3.Rnw:130-137
###################################################
par(mfrow=c(1,2))
plot(res.acp.body$x,type="n",main="ACP - CAH")
abline(h=0,v=0,lty=2)
text(res.acp.body$x,rownames(data.body),col=indiv.4groupes,font=2)
plot(res.acp.body$x,type="n",main="ACP - k-means")
abline(h=0,v=0,lty=2)
text(res.acp.body$x,rownames(data.body),col=km.body.init$cluster,font=2)


###################################################
### code chunk number 16: tp3.Rnw:150-151
###################################################
DataBody <- read.table("Data/data-body.csv",sep=";",dec=",",header=TRUE)


###################################################
### code chunk number 17: tp3.Rnw:157-159
###################################################
hc.DataBody <- hclust(dist(DataBody[,1:24]),method="ward")
plot(hc.DataBody)


###################################################
### code chunk number 18: tp3.Rnw:164-165
###################################################
plot(hc.DataBody$height[506:495],type="b")


###################################################
### code chunk number 19: tp3.Rnw:170-173
###################################################
table(cutree(hc.DataBody,k=2),DataBody$gender)
table(cutree(hc.DataBody,k=3),DataBody$gender)
table(cutree(hc.DataBody,k=6),DataBody$gender)


###################################################
### code chunk number 20: tp3.Rnw:178-194
###################################################
indiv.3groupes <- cutree(hc.DataBody,k=3)
mean.groupe1 <- apply(DataBody[indiv.3groupes==1,1:24],2,mean)
mean.groupe2 <- apply(DataBody[indiv.3groupes==2,1:24],2,mean)
mean.groupe3 <- apply(DataBody[indiv.3groupes==3,1:24],2,mean)
mean.3groupes <- rbind(mean.groupe1,mean.groupe2,mean.groupe3)
barplot(mean.3groupes,beside=TRUE,las=3,col=1:3)
barplot(mean.3groupes[c(3,1,2),],beside=TRUE,las=3,col=1:3)
legend("topleft",paste("Groupe",1:3),text.col=c(2,3,1))
pca.body <- prcomp(DataBody[,1:24])
biplot(pca.body)
par(pty="s")
plot(pca.body$rotation,type="n",xlim=c(-0.7,0.7),ylim=c(-0.7,0.7))
text(pca.body$rotation,rownames(pca.body$rotation),font=3)
abline(h=0,v=0,lty=2)
plot(pca.body$x,pch=16,col=indiv.3groupes)
abline(h=0,v=0,lty=2)



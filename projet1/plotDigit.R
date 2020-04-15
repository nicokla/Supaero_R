
#install.packages("ElemStatLearn")
library("ElemStatLearn")
# data(zip.train)
# data(zip.test)

#line should have 257 elements (not logic but that's how zip2image works)
display<-function(mat, lineNb){
  #digit=0
  #a=sample( which(test.out==0), 1 )
  #im=zip2image(as.matrix(test), a)
  #im=zip2image(as.matrix(test[a,]), 1)
  ligne = cbind(0,as.matrix(mat[lineNb,]))
  im=zip2image(ligne, 1)
  image(z=im, col=gray(256:0/256), zlim=c(-1,1), xlab="", ylab="",axes = FALSE )
}
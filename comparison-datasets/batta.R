batta<- function(d1,d2){
  library("MASS");library(fpc)

  uniquelengthd1 <- sapply(d1,function(x) length(unique(x)))
  d1 <- subset(d1, select=uniquelengthd1>1)
  d2 <- subset(d2, select=uniquelengthd1>1)
  
  uniquelengthd2 <- sapply(d2,function(x) length(unique(x)))
  d2 <- subset(d2, select=uniquelengthd2>1)
  d1<- subset(d1, select=uniquelengthd2>1)
  
  D1<-as.matrix(d1)
  D2<-as.matrix(d2)
  m1<-colMeans(D1)
  m2<-colMeans(D2)
  s1<-as.matrix(cov(D1))
  s2<-as.matrix(cov(D2))
  S<-(s1+s2)/2
  D<-(1/8)*(m1-m2)%*%ginv(S)%*%(m1-m2)+(1/2)*(log(det(S)/(sqrt(det(s1)*det(s2)))))
  return(D)
}
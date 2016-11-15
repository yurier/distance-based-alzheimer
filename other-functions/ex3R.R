library(mvtnorm)
library(ggplot2)
library(corrplot))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(1)
CN<-ex3[which(ex3$DXCHANGE==1),]
MCI<-ex3[which(ex3$DXCHANGE==2),]
AD<-ex3[which(ex3$DXCHANGE==3),]

min_size_class<-min(nrow(CN),nrow(MCI),nrow(AD))

CN<-CN[sample(nrow(CN), min_size_class), ] #sampling for normal lesser size 
MCI<-MCI[sample(nrow(MCI), min_size_class), ] #sampling for MCI lesser size
AD<-AD[sample(nrow(AD), min_size_class), ] #sampling for Alzheimer lesser size

smp_size <- floor(0.75 *min_size_class)

index <- sample(seq_len(min_size_class), size = smp_size)

ADtest <-AD[-index, ]
AD <- AD[index, ]

MCItest <- MCI[-index, ]
MCI <- MCI[index, ]

CNtest <- CN[-index, ]
CN <- CN[index, ]


muCN<-c(mean(CN[,2]),mean(CN[,3]))
SigmaCN<-cov(CN[,2:3])

muMCI<-c(mean(MCI[,2]),mean(MCI[,3]))
SigmaMCI<-cov(MCI[,2:3])

muAD<-c(mean(AD[,2]),mean(AD[,3]))
SigmaAD<-cov(AD[,2:3])

data.grid=expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50))

q.samp1 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muCN, sigma = SigmaCN))
q.samp2 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muMCI, sigma = SigmaMCI))
q.samp3 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muAD, sigma = SigmaAD))

q.samp1$class<-factor('CN')
q.samp2$class<-factor('MCI')
q.samp3$class<-factor('AD')

qsamp<-rbind(q.samp3,q.samp2,q.samp1)

cairo_ps(width=6,height = 3.1 ,file='ex31.eps')
ggplot()+geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+theme_bw()+scale_colour_manual(values=cbPalette)+xlab('ABETA')+ylab('TAU')
dev.off()

##############################################
  
X<-svd(SigmaCN)
SigmaCN<-diag(c(sum(X$d)/2,sum(X$d)/2))

X<-svd(SigmaMCI)
SigmaMCI<-diag(c(sum(X$d)/2,sum(X$d)/2))

X<-svd(SigmaAD)
SigmaAD<-diag(c(sum(X$d)/2,sum(X$d)/2))

data.grid=expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50))

q.samp1 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muCN, sigma = SigmaCN))
q.samp2 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muMCI, sigma = SigmaMCI))
q.samp3 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muAD, sigma = SigmaAD))

q.samp1$class<-factor('CN')
q.samp2$class<-factor('MCI')
q.samp3$class<-factor('AD')

qsamp<-rbind(q.samp3,q.samp2,q.samp1)

cairo_ps(width=6,height = 3.1 ,file='ex32.eps')
ggplot()+geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+theme_bw()+scale_colour_manual(values=cbPalette)+xlab('ABETA')+ylab('TAU')
dev.off()
###########################################3
  
%AD[,2:3]<-rmvnorm(nrow(AD),muAD,SigmaAD)
%CN[,2:3]<-rmvnorm(nrow(CN),muCN,SigmaCN)
%MCI[,2:3]<-rmvnorm(nrow(MCI),muMCI,SigmaMCI)

points<-rbind(data.frame(AD,class='AD'),data.frame(MCI,class='MCI'),data.frame(CN,class='CN'))

d1 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))

d2 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1])+(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN)))

d3 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN))-log(determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))

cairo_ps(width=6,height = 3.1 ,file='ex33.eps')
ggplot()+xlab('ABETA')+ylab('TAU')+geom_point(data=points, aes(x=ABETA142,y=TAU,color=class))+theme_bw()+scale_colour_manual(values=cbPalette)+geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+annotate("text", x = c(290,290,290), y = c(90,137,210), label = c("MCI=CN","AD=CN","AD=MCI"))+xlim(90, 300)
dev.off()
############################################

points<-rbind(data.frame(AD,class='AD'),data.frame(MCI,class='MCI'),data.frame(CN,class='CN'))

q.val <- cbind(points[,2:3], prob = mvtnorm::dmvnorm(points[,2:3], mean = muCN, sigma = SigmaCN,log=FALSE))
q.val <- cbind(q.val, prob = mvtnorm::dmvnorm(points[,2:3], mean = muMCI, sigma = SigmaMCI,log=FALSE))
q.val <- cbind(q.val, prob = mvtnorm::dmvnorm(points[,2:3], mean = muAD, sigma = SigmaAD,log=FALSE))

points$validation<-NULL

for (i in 1:nrow(q.val)){
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==1){
    points$validation[i]<-"CN"
  }
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==2){
    points$validation[i]<-"MCI"
  }
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==3){
    points$validation[i]<-"AD"
  }
}
cairo_ps(width=6,height = 3.1 ,file='ex34.eps')
ggplot()+xlab('ABETA')+ylab('TAU')+geom_point(data=points, aes(x=ABETA142,y=TAU,color=validation))+theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))+geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+annotate("text", x = c(290,290,290), y = c(90,137,210), label = c("MCI=CN","AD=CN","AD=MCI"))+xlim(90, 300)
dev.off()
##########################################

pointests<-rbind(data.frame(ADtest,class='AD'),data.frame(MCItest,class='MCI'),data.frame(CNtest,class='CN'))

q.val <- cbind(pointests[,2:3], prob = mvtnorm::dmvnorm(pointests[,2:3], mean = muCN, sigma = SigmaCN,log=FALSE))
q.val <- cbind(q.val, prob = mvtnorm::dmvnorm(pointests[,2:3], mean = muMCI, sigma = SigmaMCI,log=FALSE))
q.val <- cbind(q.val, prob = mvtnorm::dmvnorm(pointests[,2:3], mean = muAD, sigma = SigmaAD,log=FALSE))

pointests$validation<-NULL

for (i in 1:nrow(q.val)){
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==1){
    pointests$validation[i]<-"CN"
  }
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==2){
    pointests$validation[i]<-"MCI"
  }
  if (which(q.val[i,3:5]==max(q.val[i,3:5]), arr.ind=TRUE)[1,2]==3){
    pointests$validation[i]<-"AD"
  }
}



pointests$validation<-as.factor(pointests$validation)
cairo_ps(width=6,height = 3.1 ,file='ex35.eps')
ggplot()+xlab('ABETA')+ylab('TAU')+geom_point(data=pointests, aes(x=ABETA142,y=TAU,color=class))+theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+annotate("text", x = c(280,280,280), y = c(90,137,210), label = c("MCI=CN","AD=CN","AD=MCI"))+xlim(90, 300)
dev.off()
cairo_ps(width=6,height = 3.1 ,file='ex36.eps')
ggplot()+xlab('ABETA')+ylab('TAU')+geom_point(data=pointests, aes(x=ABETA142,y=TAU,color=validation))+theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9",))+geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+annotate("text", x = c(280,280,280), y = c(90,137,210), label = c("MCI=CN","AD=CN","AD=MCI"))+xlim(90, 300)
dev.off()

conf <- data.frame()

for (i in 0:2){
index<-summary(pointests[(26*i+1):(26*(i+1)),4:5])
for (j in 1:3){
conf[(i+1),j]<-unique(na.omit(as.numeric(unlist(strsplit(unlist(index[j+3]), "[^0-9]+")))))
}}



colnames(conf)<-c("AD","MCI","CN")
rownames(conf)<-c("AD","MCI","CN")

aux<-conf[,2]
conf[,2]<-conf[,3]
conf[,3]<-aux
M<-as.matrix(conf)/26
library(corrplot)
cairo_ps(width=5,height = 5 ,file='ex37.eps')
corrplot(M,is.corr = FALSE, order ="AOE",method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
dev.off()

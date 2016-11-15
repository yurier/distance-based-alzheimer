library(mvtnorm)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)
library(class)
library(MASS)

cbPalette <- c("#999999", "#56B4E9", "#E69F00","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(1)


ex3<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)

{
CN<-ex3[which(ex3$DX=='NL'),]
MCI<-ex3[which(ex3$DX=="MCI"),]
AD<-ex3[which(ex3$DX=="Dementia"),]

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

  EX1<-ggplot()+
    stat_contour(data=qsamp, geom="polygon",aes(x=x, y=y, z=prob, fill=class,colour=class),binwidth = 0.00004,alpha=.35)+
    theme_bw()+scale_fill_manual(values=cbPalette)+
    scale_colour_manual(values=cbPalette)+xlab('ABETA')+
    ylab('PTAU')+theme(legend.text = element_text(size = 15),
             axis.text=element_text(size=15),
             legend.title = element_text(size = 15),
             axis.title.y=element_text(size=15),
             axis.title.x=element_text(size=15),
             axis.line = element_line(colour = "black"),
             panel.border = element_blank(),
             panel.background = element_blank())

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

EX2<-ggplot()+
  stat_contour(data=qsamp, geom="polygon",aes(x=x, y=y, z=prob, fill=class,colour=class),binwidth = 0.000045,alpha=.35)+
  theme_bw()+scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+xlab('ABETA')+
  ylab('PTAU')+theme(legend.text = element_text(size = 15),
                     axis.text=element_text(size=15),
                     legend.title = element_text(size = 15),
                     axis.title.y=element_text(size=15),
                     axis.title.x=element_text(size=15),
                     axis.line = element_line(colour = "black"),
                     panel.border = element_blank(),
                     panel.background = element_blank())


mylegend2<-g_legend(EX1)

cairo_ps(width=13,height = 3.5,file='ex3.eps')
grid.arrange(arrangeGrob(EX1 + theme(legend.position="none"),
                         EX2 + theme(legend.position="none"),
                         nrow=1),
             mylegend2,
             widths = c(5,0.5))
dev.off()

###########################################3
  
AD[,2:3]<-rmvnorm(nrow(AD),muAD,SigmaAD)
CN[,2:3]<-rmvnorm(nrow(CN),muCN,SigmaCN)
MCI[,2:3]<-rmvnorm(nrow(MCI),muMCI,SigmaMCI)

points<-rbind(data.frame(AD,class='AD'),data.frame(MCI,class='MCI'),data.frame(CN,class='CN'))

d1 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))

d2 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1])+(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN)))

d3 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN))-log(determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))

EX3<-ggplot()+xlab('ABETA')+ylab('PTAU')+
  geom_point(data=points, aes(x=ABETA,y=PTAU,color=class))+
  theme_bw()+scale_colour_manual(values=cbPalette)+
  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  xlim(90, 300)+ylim(10,110)+theme(legend.text = element_text(size = 15),
                      axis.text=element_text(size=15),
                      legend.title = element_text(size = 15),
                      axis.title.y=element_text(size=15),
                      axis.title.x=element_text(size=15),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_blank(),
                      panel.background = element_blank())

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
EX4<-ggplot()+
  xlab('ABETA')+
  ylab('PTAU')+
  geom_point(data=points, aes(x=ABETA,y=PTAU,color=validation))+
  theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+
  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  xlim(90, 300)+ylim(10,110)+theme(legend.text = element_text(size = 15),
                       axis.text=element_text(size=15),
                       legend.title = element_text(size = 15),
                       axis.title.y=element_text(size=15),
                       axis.title.x=element_text(size=15),
                       axis.line = element_line(colour = "black"),
                       panel.border = element_blank(),
                       panel.background = element_blank())

mylegend2<-g_legend(EX3)

cairo_ps(width=13,height = 3.5,file='ex31.eps')
grid.arrange(arrangeGrob(EX3 + theme(legend.position="none"),
                         EX4 + theme(legend.position="none"),
                         nrow=1),
             mylegend2,
             widths = c(5,0.5))
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

EX5<-ggplot()+xlab('ABETA')+ylab('PTAU')+
  geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=class))+
  theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9","#E69F00"))+
  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  xlim(90, 300)+ylim(10,110)+theme(legend.text = element_text(size = 15),
                                 axis.text=element_text(size=15),
                                 legend.title = element_text(size = 15),
                                 axis.title.y=element_text(size=15),
                                 axis.title.x=element_text(size=15),
                                 axis.line = element_line(colour = "black"),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())

EX6<- ggplot()+xlab('ABETA')+
  ylab('PTAU')+geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=validation))+
  theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+
  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  xlim(90, 300)+ylim(10,110)+theme(legend.text = element_text(size = 15),
                                  axis.text=element_text(size=15),
                                  legend.title = element_text(size = 15),
                                  axis.title.y=element_text(size=15),
                                  axis.title.x=element_text(size=15),
                                  axis.line = element_line(colour = "black"),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())


mylegend2<-g_legend(EX5)

cairo_ps(width=13,height = 3.5,file='ex32.eps')
grid.arrange(arrangeGrob(EX5 + theme(legend.position="none"),
                         EX6 + theme(legend.position="none"),
                         nrow=1),
             mylegend2,
             widths = c(5,0.5))
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
M75<-as.matrix(conf)/26

#library(corrplot)
#cairo_ps(width=5,height = 5 ,file='ex3ex.eps')
#corrplot(M1*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
#dev.off()
} #75

{ CN<-ex3[which(ex3$DX=='NL'),]
  MCI<-ex3[which(ex3$DX=="MCI"),]
  AD<-ex3[which(ex3$DX=="Dementia"),]
  
  
  min_size_class<-min(nrow(CN),nrow(MCI),nrow(AD))
  
  CN<-CN[sample(nrow(CN), min_size_class), ] #sampling for normal lesser size 
  MCI<-MCI[sample(nrow(MCI), min_size_class), ] #sampling for MCI lesser size
  AD<-AD[sample(nrow(AD), min_size_class), ] #sampling for Alzheimer lesser size
  
  smp_size <- floor(0.25 *min_size_class)
  
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
  
  mus<-rbind(muCN,muMCI,muAD); mus<-data.frame(mus,class=c("CN","MCI","AD"))
  
  #EX1<-ggplot()+
   # geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+
    #theme_bw()+scale_colour_manual(values=cbPalette)+xlab('ABETA')+
    #geom_point(data=mus,aes(x=mus[,1], y=mus[,2], colour=mus[,3]))+
    #ylab('PTAU')+theme(legend.text = element_text(size = 15),
    #                  axis.text=element_text(size=15),
     #                 legend.title = element_text(size = 15),
      #                axis.title.y=element_text(size=15),
       #               axis.title.x=element_text(size=15),
        #              axis.line = element_line(colour = "black"),
         #             panel.border = element_blank(),
          #            panel.background = element_blank())
  
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
  
 # qsamp<-rbind(q.samp3,q.samp2,q.samp1)
  
#  EX2<-ggplot()+
    #geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+
    #theme_bw()+scale_colour_manual(values=cbPalette)+
    #geom_point(data=mus,aes(x=mus[,1], y=mus[,2], colour=mus[,3]))+
    #xlab('ABETA')+ylab('PTAU')+theme(legend.text = element_text(size = 15),
        #                            axis.text=element_text(size=15),
       #                             legend.title = element_text(size = 15),
      #                              axis.title.y=element_text(size=15),
      #                              axis.title.x=element_text(size=15),
     #                               axis.line = element_line(colour = "black"),
    #                                panel.border = element_blank(),
   #                                 panel.background = element_blank())
  #
  
  #mylegend2<-g_legend(EX1)
  
  #cairo_ps(width=13,height = 3.5,file='ex3.eps')
  #grid.arrange(arrangeGrob(EX1 + theme(legend.position="none"),
  #                         EX2 + theme(legend.position="none"),
  #                         nrow=1),
  #             mylegend2,
   #            widths = c(5,0.5))
  #dev.off()
  
  ###########################################3
  
  AD[,2:3]<-rmvnorm(nrow(AD),muAD,SigmaAD)
  CN[,2:3]<-rmvnorm(nrow(CN),muCN,SigmaCN)
  MCI[,2:3]<-rmvnorm(nrow(MCI),muMCI,SigmaMCI)
  
  points<-rbind(data.frame(AD,class='AD'),data.frame(MCI,class='MCI'),data.frame(CN,class='CN'))
  
  d1 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))
  
  d2 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1])+(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN)))
  
  d3 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN))-log(determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))
  
  #EX3<-ggplot()+xlab('ABETA')+ylab('PTAU')+
  #  geom_point(data=points, aes(x=ABETA,y=PTAU,color=class))+
  #  theme_bw()+scale_colour_manual(values=cbPalette)+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
   #                                 axis.text=element_text(size=15),
    #                                legend.title = element_text(size = 15),
     #                               axis.title.y=element_text(size=15),
      #                              axis.title.x=element_text(size=15),
       #                             axis.line = element_line(colour = "black"),
        #                            panel.border = element_blank(),
         #                           panel.background = element_blank())
  
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
 # EX4<-ggplot()+
  #  xlab('ABETA')+
   # ylab('PTAU')+
  #  geom_point(data=points, aes(x=ABETA,y=PTAU,color=validation))+
  #  theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
   #                                 axis.text=element_text(size=15),
    #                                legend.title = element_text(size = 15),
     #                               axis.title.y=element_text(size=15),
      #                              axis.title.x=element_text(size=15),
       #                             axis.line = element_line(colour = "black"),
        #                            panel.border = element_blank(),
         #                           panel.background = element_blank())
  
#  mylegend2<-g_legend(EX3)
  
 # cairo_ps(width=13,height = 3.5,file='ex31.eps')
  #grid.arrange(arrangeGrob(EX3 + theme(legend.position="none"),
   #                        EX4 + theme(legend.position="none"),
    #                       nrow=1),
     #          mylegend2,
   #            widths = c(5,0.5))
  #dev.off()
  
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
  
  #EX5<-ggplot()+xlab('ABETA')+ylab('PTAU')+
  # geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=class))+
  #  theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
  #                                  axis.text=element_text(size=15),
  #                                  legend.title = element_text(size = 15),
   #                                 axis.title.y=element_text(size=15),
  #                                  axis.title.x=element_text(size=15),
    #                                axis.line = element_line(colour = "black"),
    #                                panel.border = element_blank(),
     #                               panel.background = element_blank())
  
  #EX6<- ggplot()+xlab('ABETA')+
  #  ylab('PTAU')+geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=validation))+
  #  theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9","#E69F00"))+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
  #                                  axis.text=element_text(size=15),
  #                                  legend.title = element_text(size = 15),
  #                                  axis.title.y=element_text(size=15),
  #                                  axis.title.x=element_text(size=15),
  #                                  axis.line = element_line(colour = "black"),
  #                                  panel.border = element_blank(),
  #                                  panel.background = element_blank())
#  
  
 # mylegend2<-g_legend(EX5)
  
  #cairo_ps(width=13,height = 3.5,file='ex32.eps')
  #grid.arrange(arrangeGrob(EX5 + theme(legend.position="none"),
  #                         EX6 + theme(legend.position="none"),
  #                         nrow=1),
   #            mylegend2,
    #           widths = c(5,0.5))
  #dev.off()
  
  
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
  M25<-as.matrix(conf)/26
  
  #library(corrplot)
  #cairo_ps(width=5,height = 5 ,file='ex3ex.eps')
  #corrplot(M1*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
  #dev.off()
} #25

{
  CN<-ex3[which(ex3$DX=='NL'),]
  MCI<-ex3[which(ex3$DX=="MCI"),]
  AD<-ex3[which(ex3$DX=="Dementia"),]
  
  min_size_class<-min(nrow(CN),nrow(MCI),nrow(AD))
  
  CN<-CN[sample(nrow(CN), min_size_class), ] #sampling for normal lesser size 
  MCI<-MCI[sample(nrow(MCI), min_size_class), ] #sampling for MCI lesser size
  AD<-AD[sample(nrow(AD), min_size_class), ] #sampling for Alzheimer lesser size
  
  smp_size <- floor(0.5 *min_size_class)
  
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
  
  mus<-rbind(muCN,muMCI,muAD); mus<-data.frame(mus,class=c("CN","MCI","AD"))
  
 # EX1<-ggplot()+
#    geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+
#    theme_bw()+scale_colour_manual(values=cbPalette)+xlab('ABETA')+
#    geom_point(data=mus,aes(x=mus[,1], y=mus[,2], colour=mus[,3]))+
#    ylab('PTAU')+theme(legend.text = element_text(size = 15),
#                      axis.text=element_text(size=15),
#                      legend.title = element_text(size = 15),
#                      axis.title.y=element_text(size=15),
#                      axis.title.x=element_text(size=15),
#                      axis.line = element_line(colour = "black"),
#                      panel.border = element_blank(),
#                      panel.background = element_blank())
  
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
  
 # EX2<-ggplot()+
#    geom_contour(data=qsamp, aes(x=x, y=y, z=prob, colour=class),binwidth = 0.00000999)+
#    theme_bw()+scale_colour_manual(values=cbPalette)+
#    geom_point(data=mus,aes(x=mus[,1], y=mus[,2], colour=mus[,3]))+
#    xlab('ABETA')+ylab('PTAU')+theme(legend.text = element_text(size = 15),
#                                    axis.text=element_text(size=15),
#                                    legend.title = element_text(size = 15),
#                                    axis.title.y=element_text(size=15),
#                                    axis.title.x=element_text(size=15),
#                                    axis.line = element_line(colour = "black"),
#                                    panel.border = element_blank(),
#                                    panel.background = element_blank())
  
  
#  mylegend2<-g_legend(EX1)
  
#  cairo_ps(width=13,height = 3.5,file='ex3.eps')
#  grid.arrange(arrangeGrob(EX1 + theme(legend.position="none"),
#                           EX2 + theme(legend.position="none"),
 #                          nrow=1),
  #             mylegend2,
  #             widths = c(5,0.5))
  #dev.off()
  
  ###########################################3
  
  AD[,2:3]<-rmvnorm(nrow(AD),muAD,SigmaAD)
  CN[,2:3]<-rmvnorm(nrow(CN),muCN,SigmaCN)
  MCI[,2:3]<-rmvnorm(nrow(MCI),muMCI,SigmaMCI)
  
  points<-rbind(data.frame(AD,class='AD'),data.frame(MCI,class='MCI'),data.frame(CN,class='CN'))
  
  d1 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))
  
  d2 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaAD[1,1])*(x^2+y^2-2*muAD[1]*x-2*muAD[2]*y+sum(muAD*muAD))-log(determinant.matrix(SigmaAD,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1])+(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN)))
  
  d3 <- transform(expand.grid(x = seq(min(ex3[,2]),max(ex3[,2]),length.out=50), y = seq(min(ex3[,3]),max(ex3[,3]),length.out=50)),z=-(1/SigmaCN[1,1])*(x^2+y^2-2*muCN[1]*x-2*muCN[2]*y+sum(muCN*muCN))-log(determinant.matrix(SigmaCN,logarithm=FALSE)$modulus[1]/determinant.matrix(SigmaMCI,logarithm=FALSE)$modulus[1])+(1/SigmaMCI[1,1])*(x^2+y^2-2*muMCI[1]*x-2*muMCI[2]*y+sum(muMCI*muMCI)))
  
#  EX3<-ggplot()+xlab('ABETA')+ylab('PTAU')+
#    geom_point(data=points, aes(x=ABETA,y=PTAU,color=class))+
#    theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))+
#    geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
 #                                   axis.text=element_text(size=15),
#                                    legend.title = element_text(size = 15),
#                                    axis.title.y=element_text(size=15),
 #                                   axis.title.x=element_text(size=15),
  #                                  axis.line = element_line(colour = "black"),
   #                                 panel.border = element_blank(),
    #                                panel.background = element_blank())
  
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
  #EX4<-ggplot()+
  #  xlab('ABETA')+
  #  ylab('PTAU')+
  #  geom_point(data=points, aes(x=ABETA,y=PTAU,color=validation))+
  #  theme_bw()+scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
   #                                 axis.text=element_text(size=15),
  #                                  legend.title = element_text(size = 15),
  #                                  axis.title.y=element_text(size=15),
   #                                 axis.title.x=element_text(size=15),
    #                                axis.line = element_line(colour = "black"),
     #                               panel.border = element_blank(),
      #                              panel.background = element_blank())
  
#  mylegend2<-g_legend(EX3)
  
 # cairo_ps(width=13,height = 3.5,file='ex31.eps')
  #grid.arrange(arrangeGrob(EX3 + theme(legend.position="none"),
   #                        EX4 + theme(legend.position="none"),
    #                       nrow=1),
     #          mylegend2,
      #         widths = c(5,0.5))
  #dev.off()
  
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
  
#  EX5<-ggplot()+xlab('ABETA')+ylab('PTAU')+
#    geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=class))+
#    theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+
#    geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
#    annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
#    xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
#                                    axis.text=element_text(size=15),
#                                    legend.title = element_text(size = 15),
#                                    axis.title.y=element_text(size=15),
#                                    axis.title.x=element_text(size=15),
#                                    axis.line = element_line(colour = "black"),
#                                    panel.border = element_blank(),
 #                                   panel.background = element_blank())
  
  #EX6<- ggplot()+xlab('ABETA')+
  #  ylab('PTAU')+geom_point(data=pointests, aes(x=ABETA,y=PTAU,color=validation))+
  #  theme_bw()+scale_colour_manual(values=c("#999999", "#E69F00","#56B4E9"))+
  #  geom_contour(data=d1,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d2,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  geom_contour(data=d3,aes(x=x,y=y,z=z),bins=1,colour='black')+
  #  annotate("text", x = c(270,270,270), y = c(85,143,210), label = c("MCI=CN","AD=CN","AD=MCI"),size=5)+
  #  xlim(90, 300)+ylim(0,120)+theme(legend.text = element_text(size = 15),
  #                                  axis.text=element_text(size=15),
  #                                  legend.title = element_text(size = 15),
  #                                  axis.title.y=element_text(size=15),
  #                                  axis.title.x=element_text(size=15),
   #                                 axis.line = element_line(colour = "black"),
    #                                panel.border = element_blank(),
     #                               panel.background = element_blank())
  
  
#  mylegend2<-g_legend(EX5)
  
 # cairo_ps(width=13,height = 3.5,file='ex32.eps')
  #grid.arrange(arrangeGrob(EX5 + theme(legend.position="none"),
   #                        EX6 + theme(legend.position="none"),
    #                       nrow=1),
     #          mylegend2,
      #         widths = c(5,0.5))
  #dev.off()
  
  
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
  M50<-as.matrix(conf)/26
  
  #library(corrplot)
  #cairo_ps(width=5,height = 5 ,file='ex3ex.eps')
  #corrplot(M1*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
  #dev.off()
} #50

cairo_ps(width=11,height = 3.5 ,file='confusion_matrix_ex3.eps')
par(mfrow = c(1,3),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(M25*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
corrplot(M50*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
corrplot(M75*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
dev.off()
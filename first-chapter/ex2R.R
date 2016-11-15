library(mvtnorm)
library(ggplot2)
library(MASS)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ex2<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)
CN<-ex2[which(ex2$DX=="NL"),]
AD<-ex2[which(ex2$DX=="Dementia"),]

points<-rbind(data.frame(CN,group='CN'),data.frame(AD,group='AD'))

muCN<-c(mean(CN[,2]),mean(CN[,3]))
SigmaCN<-cov(CN[,2:3])

muAD<-c(mean(AD[,2]),mean(AD[,3]))
SigmaAD<-cov(AD[,2:3])

data.grid=expand.grid(x = seq(min(ex2[,2]),max(ex2[,2]),length.out=50), y = seq(min(ex2[,3]),max(ex2[,3]),length.out=50))
q.samp1 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muCN, sigma = SigmaCN))
q.samp2 <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = muAD, sigma = SigmaAD))
q.samp1$class<-factor('CN')
q.samp2$class<-factor('AD')
qsamp<-rbind(q.samp2,q.samp1)
grid<-expand.grid(x = seq(min(ex2[,2]),max(ex2[,2]),length.out=50), y = seq(min(ex2[,3]),max(ex2[,3]),length.out=50))
grid$z<-0
invAD<-ginv(SigmaAD)
invCN<-ginv(SigmaCN)

for (i in 1:nrow(grid)){
  grid$z[i]<- -t((c(grid$x[i],grid$y[i])-muAD))%*%invAD%*%(c(grid$x[i],grid$y[i])-muAD)-log(determinant(SigmaAD)$modulus[1]/determinant(SigmaCN)$modulus[1])+t((c(grid$x[i],grid$y[i])-muCN))%*%invCN%*%(c(grid$x[i],grid$y[i])-muCN)
}
d<-grid
cairo_ps(width=6,height = 3.1 ,file='ex2.eps')
  ggplot()+
    stat_contour(data=qsamp, geom="polygon",aes(x=x, y=y, z=prob, fill=class,colour=class),binwidth = 0.000045,alpha=.35)+
    theme_bw()+scale_fill_manual(values=cbPalette)+xlab('ABETA')+ylab('PTAU')+ylim(0,110)+
    scale_colour_manual(values=cbPalette)+xlab('ABETA')+
  geom_point(data=points, aes(x=ABETA,y=PTAU,color=group))+
  scale_colour_manual(values=cbPalette)+
  geom_contour(data=d,aes(x=x,y=y,z=z),bins=1,colour='black')+
  theme(legend.text = element_text(size = 15),
             axis.text=element_text(size=15),
             legend.title = element_text(size = 15),
             axis.title.y=element_text(size=15),
             axis.title.x=element_text(size=15),
             axis.line = element_line(colour = "black"),
             panel.border = element_blank(),
             panel.background = element_blank())
  dev.off()
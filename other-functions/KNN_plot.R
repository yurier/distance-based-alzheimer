library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

CN<-ex2[which(ex2$DX==1),]
MCI<-ex2[which(ex2$DX==2),]
AD<-ex2[which(ex2$DX==3),]
set.seed(1)
CN<-CN[sample(nrow(CN), 100), ] 
MCI<-MCI[sample(nrow(MCI), 100), ] 
AD<-AD[sample(nrow(AD), 100), ] 


x<-rbind(CN,MCI,AD)[,2:3]
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
row.names(x) <- NULL

xnew<-expand.grid(x=seq(min(x[,1]-1), max(x[,1]+1),
                        by=1.5),
                  y=seq(min(x[,2]-1), max(x[,2]+1), 
                        by=1.5))

ans <- KNN_random_break(x,xnew,as.factor(class),5,2)

mod<-ans[[1]]
prob<-ans[[2]]
prob_CN<-ifelse(mod=="CN",prob,0)
prob_MCI<-ifelse(mod=="MCI",prob,0)
prob_AD<-ifelse(mod=="AD",prob,0)


px1<-seq(min(x[,1]-1), max(x[,1]+1),by=1.5)
px2<-seq(min(x[,2]-1), max(x[,2]+1),by=1.5)

probCN <- matrix(prob_CN, length(px1), length(px2))
probMCI<-matrix(prob_MCI, length(px1), length(px2))
probAD <- matrix(prob_AD, length(px1), length(px2))

par(mar=rep(2,4))

dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD))
colnames(dataf)<-c('x','y','probCN','probMCI','probAD')

#aux<-dataf
#for (i in 1:(nrow(dataf)-1)){
#  if(summary(as.factor(dataf[i,3:5]>dataf[i+1,3:5]))[[1]]<=2){
#   dataf[i,3:5]<-c(0.3,0.3,0.3)
#  }
#  }

#cairo_ps(width=7,height = 4,file='ex4.eps')

#ggplot(dataf)+geom_contour(aes(x=x, y=y, z=prob), bins=1,data=dataf,fill = "dark grey", colour = "black", alpha = 1,size=1)+theme_bw()+geom_point(aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/3)+scale_size(range=c(0, 3))+geom_point(aes(x=x, y=y, color=class), size=3,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+scale_colour_manual(values=c( "#999999", "#E69F00", "#56B4E9"))+labs(x='ABETA142',y='TAU')
#dev.off()
cairo_ps(width=7,height = 4 ,file='ex5_1.eps')
ggplot()+theme_bw()+geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+labs(x='ABETA142',y='TAU')
dev.off()
cairo_ps(width=7,height = 4,file='ex5_2.eps')
ggplot()+theme_bw()+geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+labs(x='ABETA142',y='TAU')
dev.off()
cairo_ps(width=7,height = 4,file='ex5_3.eps')
ggplot()+theme_bw()+geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+labs(x='ABETA142',y='TAU')
dev.off()
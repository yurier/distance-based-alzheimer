library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ex2<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
x<-rbind(CN,MCI,AD)[,2:3]
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
data<-nearmiss1(x,class,degree = 1,2)
x<-data[[1]]
class<-data[[2]]
rownames(x)<-NULL
x<-Min_max_nrmlztn(x)

scale1=(max(x[,1])-min(x[,1]))/110
scale2=(max(x[,2])-min(x[,2]))/110


xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                        by=scale1),
                  y=seq(min(x[,2]), max(x[,2]), 
                        by=scale2))

mod <- knn_random_break(x, xnew, class, k = 5,pnrm = 2)
prob <- mod[[2]]

prob_CN<-ifelse(mod[[1]]=="CN",prob,0)
prob_MCI<-ifelse(mod[[1]]=="MCI",prob,0)
prob_AD<-ifelse(mod[[1]]=="AD",prob,0)
prob_CN1<-ifelse(mod[[1]]=="CN",1,0)
prob_MCI1<-ifelse(mod[[1]]=="MCI",1,0)
prob_AD1<-ifelse(mod[[1]]=="AD",1,0)

px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
px2<-seq(min(x[,2]), max(x[,2]),by=scale2)

probCN <- matrix(prob_CN, length(px1), length(px2))
probMCI<-matrix(prob_MCI, length(px1), length(px2))
probAD <- matrix(prob_AD, length(px1), length(px2))
probCN1 <- matrix(prob_CN1, length(px1), length(px2))
probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
probAD1 <- matrix(prob_AD1, length(px1), length(px2))


par(mar=rep(2,4))

dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')


A<-ggplot()+
  theme_bw()+
  geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
  geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
  scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='CN',1], y=x[class=='CN',2], class=class[class=='CN']),alpha = 1)+
  scale_colour_manual(values=c( "#56B4E9"))+
  labs(x='ABETA',y='PTAU')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

B<-ggplot()+
  theme_bw()+
  geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
  geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
  scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='MCI',1], y=x[class=='MCI',2], class=class[class=='MCI']),alpha = 1)+
  scale_colour_manual(values=c( "#E69F00"))+
  labs(x='ABETA',y='')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

C<-ggplot()+
  theme_bw()+
  geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
  geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
  scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='AD',1], y=x[class=='AD',2], class=class[class=='AD']),alpha = 1)+
  scale_colour_manual(values=c( "#999999"))+
  labs(x='ABETA',y='')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

dataf$probility<-dataf$probMCI
D<-ggplot()+
  theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+
  scale_fill_continuous(name = "V")+geom_point(data=x,aes(x=x[,1], y=x[,2], color=class), size=2,alpha = 1)+
  scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+
  labs(x='ABETA',y='')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())


mylegend<-g_legend(D)

#cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
cairo_ps(width=12,height = 2.9,file='undersampling_nearmiss1_knn_plot.eps')


grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                         B + theme(legend.position="none"),
                         C + theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend),
             widths = c(5,0.5))
dev.off()} #NEARMISS1

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
  x<-rbind(CN,MCI,AD)[,2:3]
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  data<-nearmiss2(x,class,degree = 1,2)
  x<-data[[1]]
  class<-data[[2]]
  rownames(x)<-NULL
  x<-Min_max_nrmlztn(x)
  
  scale1=(max(x[,1])-min(x[,1]))/110
  scale2=(max(x[,2])-min(x[,2]))/110
  
  
  xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                          by=scale1),
                    y=seq(min(x[,2]), max(x[,2]), 
                          by=scale2))
  
  mod <- knn_random_break(x, xnew, class, k = 5,pnrm = 2)
  prob <- mod[[2]]
  
  prob_CN<-ifelse(mod[[1]]=="CN",prob,0)
  prob_MCI<-ifelse(mod[[1]]=="MCI",prob,0)
  prob_AD<-ifelse(mod[[1]]=="AD",prob,0)
  prob_CN1<-ifelse(mod[[1]]=="CN",1,0)
  prob_MCI1<-ifelse(mod[[1]]=="MCI",1,0)
  prob_AD1<-ifelse(mod[[1]]=="AD",1,0)
  
  px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
  px2<-seq(min(x[,2]), max(x[,2]),by=scale2)
  
  probCN <- matrix(prob_CN, length(px1), length(px2))
  probMCI<-matrix(prob_MCI, length(px1), length(px2))
  probAD <- matrix(prob_AD, length(px1), length(px2))
  probCN1 <- matrix(prob_CN1, length(px1), length(px2))
  probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
  probAD1 <- matrix(prob_AD1, length(px1), length(px2))
  
  
  par(mar=rep(2,4))
  
  dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
  colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')
  
  
  A<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='CN',1], y=x[class=='CN',2], class=class[class=='CN']),alpha = 1)+
    scale_colour_manual(values=c( "#56B4E9"))+
    labs(x='ABETA',y='PTAU')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  B<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='MCI',1], y=x[class=='MCI',2], class=class[class=='MCI']),alpha = 1)+
    scale_colour_manual(values=c( "#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  C<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='AD',1], y=x[class=='AD',2], class=class[class=='AD']),alpha = 1)+
    scale_colour_manual(values=c( "#999999"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  dataf$probility<-dataf$probMCI
  D<-ggplot()+
    theme_bw()+
    geom_point(data=dataf,aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+
    scale_fill_continuous(name = "V")+geom_point(data=x,aes(x=x[,1], y=x[,2], color=class), size=2,alpha = 1)+
    scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  
  mylegend<-g_legend(D)
  
  #cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
  cairo_ps(width=12,height = 2.9,file='undersampling_nearmiss2_knn_plot.eps')
  
  
  grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                           B + theme(legend.position="none"),
                           C + theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend),
               widths = c(5,0.5))
  dev.off()} #NEARMISS2

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
  x<-rbind(CN,MCI,AD)[,2:3]
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  data<-nearmiss3(x,class,degree = 1,2)
  x<-data[[1]]
  class<-data[[2]]
  rownames(x)<-NULL
  x<-Min_max_nrmlztn(x)
  
  scale1=(max(x[,1])-min(x[,1]))/110
  scale2=(max(x[,2])-min(x[,2]))/110
  
  
  xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                          by=scale1),
                    y=seq(min(x[,2]), max(x[,2]), 
                          by=scale2))
  
  mod <- knn_random_break(x, xnew, class, k = 5,pnrm = 2)
  prob <- mod[[2]]
  
  prob_CN<-ifelse(mod[[1]]=="CN",prob,0)
  prob_MCI<-ifelse(mod[[1]]=="MCI",prob,0)
  prob_AD<-ifelse(mod[[1]]=="AD",prob,0)
  prob_CN1<-ifelse(mod[[1]]=="CN",1,0)
  prob_MCI1<-ifelse(mod[[1]]=="MCI",1,0)
  prob_AD1<-ifelse(mod[[1]]=="AD",1,0)
  
  px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
  px2<-seq(min(x[,2]), max(x[,2]),by=scale2)
  
  probCN <- matrix(prob_CN, length(px1), length(px2))
  probMCI<-matrix(prob_MCI, length(px1), length(px2))
  probAD <- matrix(prob_AD, length(px1), length(px2))
  probCN1 <- matrix(prob_CN1, length(px1), length(px2))
  probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
  probAD1 <- matrix(prob_AD1, length(px1), length(px2))
  
  
  par(mar=rep(2,4))
  
  dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
  colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')
  
  
  A<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='CN',1], y=x[class=='CN',2], class=class[class=='CN']),alpha = 1)+
    scale_colour_manual(values=c( "#56B4E9"))+
    labs(x='ABETA',y='PTAU')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  B<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='MCI',1], y=x[class=='MCI',2], class=class[class=='MCI']),alpha = 1)+
    scale_colour_manual(values=c( "#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  C<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='AD',1], y=x[class=='AD',2], class=class[class=='AD']),alpha = 1)+
    scale_colour_manual(values=c( "#999999"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  dataf$probility<-dataf$probMCI
  D<-ggplot()+
    theme_bw()+
    geom_point(data=dataf,aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+
    scale_fill_continuous(name = "V")+geom_point(data=x,aes(x=x[,1], y=x[,2], color=class), size=2,alpha = 1)+
    scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  
  mylegend<-g_legend(D)
  
  #cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
  cairo_ps(width=12,height = 2.9,file='undersampling_nearmiss3_knn_plot.eps')
  
  
  grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                           B + theme(legend.position="none"),
                           C + theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend),
               widths = c(5,0.5))
  dev.off()} #NEARMISS3

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
  x<-rbind(CN,MCI,AD)[,2:3]
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  data<-nearmiss4(x,class,degree = 1,2)
  x<-data[[1]]
  class<-data[[2]]
  rownames(x)<-NULL
  x<-Min_max_nrmlztn(x)
  
  scale1=(max(x[,1])-min(x[,1]))/110
  scale2=(max(x[,2])-min(x[,2]))/110
  
  
  xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                          by=scale1),
                    y=seq(min(x[,2]), max(x[,2]), 
                          by=scale2))
  
  mod <- knn_random_break(x, xnew, class, k = 5,pnrm = 2)
  prob <- mod[[2]]
  
  prob_CN<-ifelse(mod[[1]]=="CN",prob,0)
  prob_MCI<-ifelse(mod[[1]]=="MCI",prob,0)
  prob_AD<-ifelse(mod[[1]]=="AD",prob,0)
  prob_CN1<-ifelse(mod[[1]]=="CN",1,0)
  prob_MCI1<-ifelse(mod[[1]]=="MCI",1,0)
  prob_AD1<-ifelse(mod[[1]]=="AD",1,0)
  
  px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
  px2<-seq(min(x[,2]), max(x[,2]),by=scale2)
  
  probCN <- matrix(prob_CN, length(px1), length(px2))
  probMCI<-matrix(prob_MCI, length(px1), length(px2))
  probAD <- matrix(prob_AD, length(px1), length(px2))
  probCN1 <- matrix(prob_CN1, length(px1), length(px2))
  probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
  probAD1 <- matrix(prob_AD1, length(px1), length(px2))
  
  
  par(mar=rep(2,4))
  
  dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
  colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')
  
  
  A<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='CN',1], y=x[class=='CN',2], class=class[class=='CN']),alpha = 1)+
    scale_colour_manual(values=c( "#56B4E9"))+
    labs(x='ABETA',y='PTAU')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  B<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='MCI',1], y=x[class=='MCI',2], class=class[class=='MCI']),alpha = 1)+
    scale_colour_manual(values=c( "#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  C<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='AD',1], y=x[class=='AD',2], class=class[class=='AD']),alpha = 1)+
    scale_colour_manual(values=c( "#999999"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  dataf$probility<-dataf$probMCI
  D<-ggplot()+
    theme_bw()+
    geom_point(data=dataf,aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+
    scale_fill_continuous(name = "V")+geom_point(data=x,aes(x=x[,1], y=x[,2], color=class), size=2,alpha = 1)+
    scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  
  mylegend<-g_legend(D)
  
  #cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
  cairo_ps(width=12,height = 2.9,file='undersampling_nearmiss4_knn_plot.eps')
  
  
  grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                           B + theme(legend.position="none"),
                           C + theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend),
               widths = c(5,0.5))
  dev.off()} #NEARMISS4

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
  x<-rbind(CN,MCI,AD)[,2:3]
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  data<-mostdistant(x,class,degree = 1,2)
  x<-data[[1]]
  class<-data[[2]]
  rownames(x)<-NULL
  x<-Min_max_nrmlztn(x)
  
  scale1=(max(x[,1])-min(x[,1]))/110
  scale2=(max(x[,2])-min(x[,2]))/110
  
  
  xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                          by=scale1),
                    y=seq(min(x[,2]), max(x[,2]), 
                          by=scale2))
  
  mod <- knn_random_break(x, xnew, class, k = 5,pnrm = 2)
  prob <- mod[[2]]
  
  prob_CN<-ifelse(mod[[1]]=="CN",prob,0)
  prob_MCI<-ifelse(mod[[1]]=="MCI",prob,0)
  prob_AD<-ifelse(mod[[1]]=="AD",prob,0)
  prob_CN1<-ifelse(mod[[1]]=="CN",1,0)
  prob_MCI1<-ifelse(mod[[1]]=="MCI",1,0)
  prob_AD1<-ifelse(mod[[1]]=="AD",1,0)
  
  px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
  px2<-seq(min(x[,2]), max(x[,2]),by=scale2)
  
  probCN <- matrix(prob_CN, length(px1), length(px2))
  probMCI<-matrix(prob_MCI, length(px1), length(px2))
  probAD <- matrix(prob_AD, length(px1), length(px2))
  probCN1 <- matrix(prob_CN1, length(px1), length(px2))
  probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
  probAD1 <- matrix(prob_AD1, length(px1), length(px2))
  
  
  par(mar=rep(2,4))
  
  dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
  colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')
  
  
  A<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='CN',1], y=x[class=='CN',2], class=class[class=='CN']),alpha = 1)+
    scale_colour_manual(values=c( "#56B4E9"))+
    labs(x='ABETA',y='PTAU')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  B<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='MCI',1], y=x[class=='MCI',2], class=class[class=='MCI']),alpha = 1)+
    scale_colour_manual(values=c( "#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  C<-ggplot()+
    theme_bw()+
    geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
    geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
    scale_size(range=c(-1, 3))+geom_point(aes(x=x, y=y, color=class), size=2,data=data.frame(x=x[class=='AD',1], y=x[class=='AD',2], class=class[class=='AD']),alpha = 1)+
    scale_colour_manual(values=c( "#999999"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  dataf$probility<-dataf$probMCI
  D<-ggplot()+
    theme_bw()+
    geom_point(data=dataf,aes(x=x, y=y, size=probability),fill = "dark grey", colour = "black", alpha = 1/9)+scale_size(range=c(-1, 3))+
    scale_fill_continuous(name = "V")+geom_point(data=x,aes(x=x[,1], y=x[,2], color=class), size=2,alpha = 1)+
    scale_colour_manual(values=c( "#999999", "#56B4E9","#E69F00"))+
    labs(x='ABETA',y='')+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  mylegend<-g_legend(D)
  
  #cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
  cairo_ps(width=13.5,height = 2.9,file='undersampling_mostdistant_knn_plot.eps')
  
  
  grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                           B + theme(legend.position="none"),
                           C + theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend),
               widths = c(5,0.5))
  dev.off()} #MOSTDISTANT
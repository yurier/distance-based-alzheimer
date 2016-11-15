library(ggplot2)
library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)
library(corrplot)

{set.seed(1)
dim_lim<-100
meaning<-as.data.frame(rnorm(dim_lim,mean=0,sd=0))
k<-1
for (j in c(1,2,3,2/3,2/5,1/7)){
  dat<-as.data.frame(runif(4000,min=0,max=1))
  dista<-NULL
  
  for (i in c(1:dim_lim)){
    dat[,i]<-runif(4000,min=0,max=1)
    query<-rep(0,ncol(dat))
    dista<-dist2(x = dat,y=query,method="minkowski",p=j)
    meaning[i,k]<-min(dista)/max(dista)
  }
  k<-k+1
}

  meang<-cbind(c(1:dim_lim),meaning,as.factor('p=1'),as.factor('p=2'),as.factor('p=3'),as.factor('p=2/3'),as.factor('p=2/5'),as.factor('p=1/7'))
  
 cairo_ps(width=7,height = 4 ,file="metricloss.eps")
  ggplot()+xlab('dimension')+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,2],colour=meang[,8]),size=1)+
  #theme_bw()
  #ggplot()+xlab('dimensions')+ylab('constrast')+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,3],colour=meang[,9]),size=1)+
  #theme_bw()
  #ggplot()+xlab('dimensions')+ylab('constrast')+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,4],colour=meang[,10]),size=1)+
  #theme_bw()
  #ggplot()+xlab('dimensions')+ylab('constrast')+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,5],colour=meang[,11]),size=1)+
  #theme_bw()
  #ggplot()+xlab('dimensions')+ylab('constrast')+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,6],colour=meang[,12]),size=1)+
  geom_line(data=meang, aes(x=meang[,1],y=meang[,7],colour=meang[,13]),size=1)+ylab(expression(paste("constrast"^{-1})))+
  theme_bw()+scale_color_manual(values=c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2"),name="p-norm",breaks=c("p=3","p=2","p=1","p=2/3",'p=2/5',"p=1/7"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) 
 dev.off()
}  #metric constrast

#must load meang_melt

matrix_aux<-NULL
levels<-c("p=1","p=2","p=3","p=2/3","p=2/5","p=1/7")

{cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
  y<-1;x<-1
  
  for (i in levels){
    for (j in levels){
      cont[x,y]<-sum(as.integer((meang_melt)[meang_melt[,1][1:10]==j,][,2]<(meang_melt)[meang_melt[,1]==i,][,2]))/nrow(meang_melt[meang_melt[,1][1:10]=="dimension",])
      y<-y+1}
    y<-1
    x<-x+1      
  }
  
  rownames(cont)<-levels<-c("1","2","3","2/3","2/5","1/7")
  colnames(cont)<-levels<-c("1","2","3","2/3","2/5","1/7")
  matrix_aux[[1]]<-floor(cont*1000)/10
}

library(ggplot2)
library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)
library(corrplot)

{set.seed(1)
  dim_lim<-100
  meaning<-as.data.frame(rnorm(dim_lim,mean=0,sd=0))
  k<-1
  for (j in c(1,2,3,2/3,2/5,1/7)){
    dat<-as.data.frame(runif(4000,min=0,max=1))
    dista<-NULL
    
    for (i in c(1:dim_lim)){
      dat[,i]<-runif(4000,min=0,max=1)
      query<-rep(0,ncol(dat))
      dista<-dist2(x = dat,y=query,method="minkowski",p=j)
      meaning[i,k]<-min(dista)/max(dista)
    }
    k<-k+1
  }
  
  meang<-cbind(c(1:dim_lim),meaning,as.factor('p=1'),as.factor('p=2'),as.factor('p=3'),as.factor('p=2/3'),as.factor('p=2/5'),as.factor('p=1/7'))
  
  cairo_ps(width=7,height = 4 ,file="metricloss.eps")
  ggplot()+xlab('dimension')+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,2],colour=meang[,8]),size=1)+
    #theme_bw()
    #ggplot()+xlab('dimensions')+ylab('constrast')+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,3],colour=meang[,9]),size=1)+
    #theme_bw()
    #ggplot()+xlab('dimensions')+ylab('constrast')+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,4],colour=meang[,10]),size=1)+
    #theme_bw()
    #ggplot()+xlab('dimensions')+ylab('constrast')+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,5],colour=meang[,11]),size=1)+
    #theme_bw()
    #ggplot()+xlab('dimensions')+ylab('constrast')+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,6],colour=meang[,12]),size=1)+
    geom_line(data=meang, aes(x=meang[,1],y=meang[,7],colour=meang[,13]),size=1)+ylab(expression(paste("constrast"^{-1})))+
    theme_bw()+scale_color_manual(values=c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2"),name="p-norm",breaks=c("p=3","p=2","p=1","p=2/3",'p=2/5',"p=1/7"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  dev.off()
}  #metric constrast

#must load meang_melt

levels<-c("p=3","p=2","p=1","p=2/3","p=2/5","p=1/7")

{cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
  y<-1;x<-1
  
  for (i in levels){
    for (j in levels){
      cont[x,y]<-sum(as.integer((meang_melt)[meang_melt[,1]==j,][,2][1:5]<(meang_melt)[meang_melt[,1]==i,][,2][1:5]))/length(1:5)
      y<-y+1}
    y<-1
    x<-x+1      
  }
  
  rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
  colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
  A<-as.matrix(floor(cont*1000)/10)
}

{cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
  y<-1;x<-1
  
  for (i in levels){
    for (j in levels){
      cont[x,y]<-sum(as.integer((meang_melt)[meang_melt[,1]==j,][,2][1:10]<(meang_melt)[meang_melt[,1]==i,][,2][1:10]))/length(1:10)
      y<-y+1}
    y<-1
    x<-x+1      
  }
  
  rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
  colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
  B<-as.matrix(floor(cont*1000)/10)
}

{cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
  y<-1;x<-1
  
  for (i in levels){
    for (j in levels){
      cont[x,y]<-sum(as.integer((meang_melt)[meang_melt[,1]==j,][,2][1:50]<(meang_melt)[meang_melt[,1]==i,][,2][1:50]))/length(1:50)
      y<-y+1}
    y<-1
    x<-x+1      
  }
  
  rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
  colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
  C<-as.matrix(floor(cont*1000)/10)
}


cairo_ps(width=11,height = 3.6 ,file='confusion_metric_loss.eps')
par(mfrow = c(1,3),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
dev.off() ###confusion matrices plot

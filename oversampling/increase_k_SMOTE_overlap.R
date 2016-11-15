library(ggplot2)
batta<-NULL
{CN<-ex2[which(ex2$DX==1),]
  MCI<-ex2[which(ex2$DX==2),]
  AD<-ex2[which(ex2$DX==3),]
  set.seed(1)
  
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  x<-rbind(CN,MCI,AD)[,2:3]
  x<-Min_max_nrmlztn(x)
  
  data<-SMOTE(x,class,1,2,7)
  
  smote<-(data[[1]])[data[[2]]=="AD",]
  ad<-x[class=="AD",]
  Ad<-data.frame(rbind(data.frame(smote,kind="synthetic"),data.frame(ad,kind="original")))
  Ad[,3]<-as.factor(Ad[,3])
  batta<-c(1/exp(batt(smote,ad)),batta)
  
  A<-ggplot(data=Ad,aes(ABETA142,TAU)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black',n=200,bins=9) + 
    scale_fill_continuous(name="density",high="black",low="white") +
    guides(alpha="none") + xlim(0.1,.8)+ylim(0.1,.8)+theme_bw()+
    geom_point(aes(color=kind),alpha=.8,size=1.5)+scale_color_manual(name="kind",values =c('black',"#999999"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15))}

{CN<-ex2[which(ex2$DX==1),]
  MCI<-ex2[which(ex2$DX==2),]
  AD<-ex2[which(ex2$DX==3),]
  set.seed(1)
  
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  x<-rbind(CN,MCI,AD)[,2:3]
  x<-Min_max_nrmlztn(x)
  
  data<-SMOTE(x,class,1,2,15)
  
  smote<-(data[[1]])[data[[2]]=="AD",]
  ad<-x[class=="AD",]
  Ad<-data.frame(rbind(data.frame(smote,kind="synthetic"),data.frame(ad,kind="original")))
  Ad[,3]<-as.factor(Ad[,3])
  batta<-c(1/exp(batt(smote,ad)),batta)
  B<-ggplot(data=Ad,aes(ABETA142,TAU)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black',n=200,bins=9) + 
    scale_fill_continuous(name="density",high="black",low="white") +
    guides(alpha="none") + xlim(0.1,.8)+ylim(0.1,.8)+theme_bw()+
    geom_point(aes(color=kind),alpha=.8,size=1.5)+scale_color_manual(name="kind",values =c('black',"#999999"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15))}

{CN<-ex2[which(ex2$DX==1),]
  MCI<-ex2[which(ex2$DX==2),]
  AD<-ex2[which(ex2$DX==3),]
  set.seed(1)
  
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  x<-rbind(CN,MCI,AD)[,2:3]
  x<-Min_max_nrmlztn(x)
  
  data<-SMOTE(x,class,1,2,30)
  
  smote<-(data[[1]])[data[[2]]=="AD",]
  ad<-x[class=="AD",]
  Ad<-data.frame(rbind(data.frame(smote,kind="synthetic"),data.frame(ad,kind="original")))
  Ad[,3]<-as.factor(Ad[,3])
  batta<-c(1/exp(batt(smote,ad)),batta)
  C<-ggplot(data=Ad,aes(ABETA142,TAU)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black',n=200,bins=9) + 
    scale_fill_continuous(name="density",high="black",low="white") +
    guides(alpha="none") + xlim(0.1,.8)+ylim(0.1,.8)+theme_bw()+
    geom_point(aes(color=kind),alpha=.8,size=1.5)+scale_color_manual(name="kind",values =c('black',"#999999"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15))}


mylegend1<-g_legend(C)

cairo_ps(width=13.5,height = 3.3,file='increase_SMOTE_k_overlap.eps')
grid.arrange(arrangeGrob(A +theme(legend.position="none"),
                         B + ylab("")+theme(legend.position="none"),
                         C + ylab("")+theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1),
             widths = c(5,0.5))
dev.off()

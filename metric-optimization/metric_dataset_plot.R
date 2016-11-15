library(reshape2)
library(MASS)
library(mvtnorm)
library(ggplot2)
library(class)
library(stats4)
library(scales)
library(flexclust)

load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/metric optimization/metric_optim_dataset.RData")
{{melt_metric_opt_k<-metric_opt_k
  for (i in 1:nrow(metric_opt_k)){
   melt_metric_opt_k[i,-c(2,3,4,5)]<-metric_opt_k[i,-c(2,3,4,5)]-metric_opt_k[i,1]
  }
  melt_metric_opt_k<-melt(melt_metric_opt_k)[25:264,]
  melt_metric_opt_k$normalization<-as.factor(melt_metric_opt_k$normalization)
  melt_metric_opt_k$dataset<-as.factor(melt_metric_opt_k$dataset)
  
  melt_metric_opt_k$value<-as.numeric(as.character(melt_metric_opt_k$value))
  melt_metric_opt_k$variable<-as.numeric(as.character(melt_metric_opt_k$variable))
} #melt data

{melt_metric_opt_k_sd<-metric_opt_k_sd

  melt_metric_opt_k_sd<-melt(melt_metric_opt_k_sd)[25:264,]
  melt_metric_opt_k_sd$normalization<-as.factor(melt_metric_opt_k$normalization)
  melt_metric_opt_k_sd$dataset<-as.factor(melt_metric_opt_k_sd$dataset)
  
  melt_metric_opt_k_sd$value<-as.numeric(as.character(melt_metric_opt_k_sd$value))
  melt_metric_opt_k_sd$variable<-as.numeric(as.character(melt_metric_opt_k_sd$variable))
} #melt data sd

{metric_opt_k_max<-data.frame()
cont<-0
for (i in levels(melt_metric_opt_k$dataset)){
  cont<-cont+1
  aux<-(melt_metric_opt_k[melt_metric_opt_k$dataset==i,])[which.max(melt_metric_opt_k[melt_metric_opt_k$dataset==i,][,4]),]
  metric_opt_k_max<-rbind(aux,metric_opt_k_max)
}} #max improvemnt

library(gridExtra)
library(grid)
library(ggplot2)

ymax<-max(melt_metric_opt_k$value)+.3
ymin<-min(melt_metric_opt_k$value)-.3
A<-ggplot()+ geom_vline(xintercept = 2,alpha=.5,linetype = "longdash",colour="black")+theme_bw()+
 geom_line(data=melt_metric_opt_k,aes(x=variable,y=value,color=dataset))+ylim(ymin,ymax)+
  geom_point(data = metric_opt_k_max,aes(x=variable,y=value,color=dataset),size=2)+
  ylab("precision difference from 2-norm")+xlab("p-norm")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
ymax<-max(melt_metric_opt_k_sd$value)+.3
ymin<-min(melt_metric_opt_k_sd$value)-.3
B<-ggplot()+theme_bw()+
  geom_line(data=melt_metric_opt_k_sd,aes(x=variable,y=value,color=dataset))+ylim(ymin,ymax)+
  ylab("standard deviation")+xlab("p-norm")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())


mylegend1<-g_legend(A)

cairo_ps(width=12,height = 3.5,file='k_opt_metric_dataset.eps')
grid.arrange(arrangeGrob(A +theme(legend.position="none"),
                         B +theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1),
             widths = c(5,.8))
dev.off()
} #distance from opti and metric
library(flexclust)
{list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
contrast<-NULL
class_av_contrast<-NULL
for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-select_normalization(set,as.character(metric_opt_nonk$normalization[ss]))
  dista<-dist2(x=set,y=c(rep(0,ncol(set))),method="minkowski",p=2)
  con_aux<-0
  for (j in levels(class)){
    aux<-dista[class==j]
    con_aux<-con_aux+(min(aux)/max(aux))
  }
  class_av_contrast[ss]<-con_aux/length(levels(class))
  contrast[ss]<-min(dista)/max(dista)
}
contrast<-data.frame(contrast=contrast,dataset=metric_opt_k$dataset,`average contrast`=class_av_contrast)

for (i in levels(metric_opt_k$dataset)){
  
  contrast[contrast$dataset==i,4]<-metric_opt_k_max[metric_opt_k_max$dataset==i,]$variable
  contrast[contrast$dataset==i,5]<-metric_opt_k_max[metric_opt_k_max$dataset==i,]$value
}
colnames(contrast)<-c('contrast','dataset','class average contrast','p-norm','improvement')
}

A<-ggplot(contrast, aes(x=`class average contrast`,y=contrast)) +  geom_smooth(color="black") +
  geom_point(aes(color=dataset),size=2) + theme_bw()+ylab(expression(paste("constrast"^{-1})))+
  xlab(expression(paste("class average ","constrast"^{-1})))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

B<-ggplot(contrast, aes(x=`class average contrast`,y=improvement))+
  geom_smooth(color="black") +
  geom_point(aes(color=dataset),size=2) + theme_bw()+
  xlab(expression(paste("class average ","constrast"^{-1})))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

C<-ggplot(contrast, aes(x=`p-norm`,y=improvement))+
  geom_smooth(color="black") +
  geom_point(aes(color=dataset),size=2) + theme_bw()+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

mylegend1<-g_legend(A)

cairo_ps(width=12,height = 3,file='survey.eps')
grid.arrange(arrangeGrob(A +theme(legend.position="none"),
                         B +theme(legend.position="none"),
                         C +theme(legend.position="none"), 
                         nrow=1),
             arrangeGrob(mylegend1),
             widths = c(5,.8))
dev.off()
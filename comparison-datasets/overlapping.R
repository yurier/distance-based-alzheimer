library(corrplot)
library(mvtnorm)
library(corrplot)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","coral","cyan")

#load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/random undersampling/metric_and_norm_opt_by_daset.RData")
#metric_opt_k_max<-metric_opt_k_max[c(3,1,7,8,2,5,6,4),]
list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
data_overlapping<-data.frame()
set_names<-c("plasma","psyconvert","bloodabeta","adni","proteom","neuropsy","neuroimag","parkinson")
bal_names<-c("RU","N1","N2","N3","N4","MD","S","S1","S2","S3")

for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
 # 
for (j in bal_names[1:6]){
  sett<-select_imbalance(set,class,j);
  classt<-sett[[2]]; sett<-sett[[1]];aux<-1;aux_aux<-matrix(ncol=3,nrow=length(levels(class))*length(levels(class)))
for (i in levels(class)){
 for (ii in levels(class)){
  aux_aux[aux,1:3]<-c((1/exp(batta(sett[classt==i,],set[!class==ii,]))),i,ii)
  aux<-aux+1
}}
  aux<-data.frame(aux_aux,j,set_names[ss])
  data_overlapping<-rbind(data_overlapping,aux)
}
for (j in bal_names[7:10]){
    sett<-select_imbalance(set,class,j);
    classt<-as.factor(c(as.character(class),as.character(sett[[2]])));
    sett<-rbind(set,sett[[1]]);
    aux<-1;aux_aux<-matrix(ncol=3,nrow=length(levels(class))*length(levels(class)))
    for (i in levels(class)){
      for (ii in levels(class)){
        
        aux_aux[aux,1:3]<-c((1/exp(batta(sett[classt==i,],set[!class==ii,]))),i,ii)
        aux<-aux+1
      }}
    aux<-data.frame(aux_aux,j,set_names[ss])
    data_overlapping<-rbind(data_overlapping,aux)
  }
  
  }

data_overlapping[,1][is.infinite(data_overlapping[,1])] = NA
data_overlapping[,1]<-as.numeric(as.character(data_overlapping[,1]))
equal_ovllpp<-data_overlapping[data_overlapping[,2]==data_overlapping[,3],]
equal_ovllpp<-equal_ovllpp[complete.cases(equal_ovllpp),]
equal_ovllpp<-equal_ovllpp[!equal_ovllpp[,1]==1,]
not_ovllpp<-data_overlapping[!data_overlapping[,2]==data_overlapping[,3],]
not_ovllpp<-not_ovllpp[complete.cases(not_ovllpp),]

data_not<-data.frame(matrix(0,ncol=3,nrow=ss*length(bal_names)))
data_equal<-data.frame(matrix(0,ncol=3,nrow=ss*length(bal_names)))
aux<-1
for (j in set_names){
  for (jj in bal_names){
    aux_aux<-equal_ovllpp[equal_ovllpp[,5]==j,]
    data_equal[aux,1:3]<-c(mean(aux_aux[aux_aux[,4]==jj,][,1]),j,jj)
    aux<-aux+1
      }
}

aux<-1
for (j in set_names){
  for (jj in bal_names){
    aux_aux<-not_ovllpp[not_ovllpp[,5]==j,]
    data_not[aux,1:3]<-c(mean(aux_aux[aux_aux[,4]==jj,][,1]),j,jj)
    aux<-aux+1
  }
}

data_equal<-as.data.frame(data_equal);colnames(data_equal)<-c("averaged overlapping","dataset","methods")
data_not<-as.data.frame(data_not);colnames(data_not)<-c("averaged overlapping","dataset","methods")
data_not[,1]<-as.numeric(data_not[,1]);data_not[,2]<-as.factor(data_not[,2]);data_not[,3]<-as.factor(data_not[,3])
data_equal[,1]<-as.numeric(data_equal[,1]);data_equal[,2]<-as.factor(data_equal[,2]);data_equal[,3]<-as.factor(data_equal[,3])
data_equal[is.na(data_equal),][,1]<-1

#  ggplot(data=data_equal,aes(x=dataset,y=`averaged overlapping`,color=methods,group=methods))+
# theme_bw()+ geom_line()+scale_color_manual(values=cbPalette)+
#  theme(legend.text = element_text(size = 15),
#        axis.text=element_text(size=15),
#        legend.title = element_text(size = 15),
#        axis.title.y=element_text(size=15),
#       axis.title.x=element_text(size=15),
#        axis.line = element_line(colour = "black"),
#       panel.border = element_blank(),
#       panel.background = element_blank())
cairo_ps(width=7,height = 5,file='average_oversampling.eps')
ggplot(data=data_not,aes(x=dataset,y=`averaged overlapping`,color=methods,group=methods))+
    theme_bw()+ geom_point()+geom_line()+scale_color_manual(values=cbPalette)+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
dev.off()
  
conf<-matrix(0,ncol=length(bal_names),nrow=length(bal_names))
colnames(conf)<-bal_names;rownames(conf)<-bal_names
for (i in bal_names)
  for (j in bal_names){
    conf[i,j]<-sum(as.integer(data_not[data_not$methods==j,][,1]<data_not[data_not$methods==i,][,1]))/8
  }

conf<-conf*100

cairo_ps(width=8,height = 8 ,file='overlaped_conf.eps')
corrplot(conf,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.75,number.cex = 1.78,cl.pos="n",tl.srt=0)
dev.off()


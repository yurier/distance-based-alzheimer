

{ metric_range<-seq(.1,3,.05)
  list_metric_higher_parkinson_nonop<-matrix(0,ncol = 5,nrow = length(metric_range))
  
  metric_aux<-0
  for (metric in metric_range){
    set<-parkinsons
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    trains<-data.frame(NULL)
    tests<-NULL
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    k<-5
    
    
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          ans1 <- knn_random_break(train,test,cltra,k,metric)
          modtest[j]<-as.character(ans1[[1]][1])
        }
        datag<-data.frame(correct=as.character(class),predict=modtest)
        modtest<-NULL
        conf<-matrix(0,length(levels(class)),length(levels(class)))
        colnames(conf)<-levels(class);rownames(conf)<-levels(class)
        for (ii in levels(class)){
          for(jj in levels(class)){
            aux1<-datag[datag$correct==ii,]
            aux2<-aux1[aux1$predict==jj,]
            conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
          }}
        maux<-0
        for (m in 1:length(levels(class))){
          maux<-as.numeric(conf[m,m])+maux}
        
        tests[jjj]<-maux/length(levels(class))}
    
      high<-mean(tests[1:jjj])
      sd_hit<-sd(tests[1:jjj])

    metric_aux<-metric_aux+1
    list_metric_higher_parkinson_nonop[metric_aux,c(1,2,3,4)]<-c(high*100,sd_hit*100,k,metric)
    print(list_metric_higher_parkinson_nonop[1:metric_aux,])
  }} #nonopt
        
{
  metric_range<-seq(0.1,3,.05)
  list_metric_higher_parkinson<-matrix(0,ncol = 5,nrow = length(metric_range))
  
  metric_aux<-0
  for (metric in metric_range){
    set<-parkinsons
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    set<-Min_max_nrmlztn(set)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(c(k,"of",floor(sqrt(nrow(set)))))
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          
          ans1 <- knn_random_break(train,test,cltra,k,metric)
          modtest[j]<-as.character(ans1[[1]][1])
        }
        datag<-data.frame(correct=as.character(class),predict=modtest)
        modtest<-NULL
        conf<-matrix(0,length(levels(class)),length(levels(class)))
        colnames(conf)<-levels(class);rownames(conf)<-levels(class)
        for (ii in levels(class)){
          for(jj in levels(class)){
            aux1<-datag[datag$correct==ii,]
            aux2<-aux1[aux1$predict==jj,]
            conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
          }}
        maux<-0
        for (m in 1:length(levels(class))){
          maux<-as.numeric(conf[m,m])+maux}
        
        tests[k,jjj]<-maux/length(levels(class))}}
    test_mean<-NULL;test_sd<-NULL;
    for (i in 1:k){
      test_mean[i]<-mean(t(tests[i,1:jjj]))
      test_sd[i]<-sd(t(tests[i,1:jjj]))
    }
    metric_aux<-metric_aux+1
    list_metric_higher_parkinson[metric_aux,c(1,2,3,4)]<-c(test_mean[which.max(test_mean)]*100,
                                                             test_sd[which.max(test_mean)]*100,
                                                             which.max(test_mean),metric)
    print(list_metric_higher_parkinson[1:metric_aux,])
    
  }
  #optimized-k and randomized features
} #opt
    
    
{list_metric_higher_parkinson_nonop<-as.data.frame(list_metric_higher_parkinson_nonop)
list_metric_higher_parkinson_nonop[,4]<-as.numeric(as.character(list_metric_higher_parkinson_nonop[,4]))
list_metric_higher_parkinson_nonop[,1]<-as.numeric(as.character(list_metric_higher_parkinson_nonop[,1]))
list_metric_higher_parkinson_nonop[,2]<-as.numeric(as.character(list_metric_higher_parkinson_nonop[,2]))
list_metric_higher_parkinson_nonop[,3]<-as.integer(as.character(list_metric_higher_parkinson_nonop[,3]))
list_metric_higher_parkinson_nonop[,5]<-as.integer(as.character(list_metric_higher_parkinson_nonop[,5]))
colnames(list_metric_higher_parkinson_nonop)<-c("LOOCV precision","sd","k","p-norm","dimensions","optimization")

list_metric_higher_parkinson<-as.data.frame(list_metric_higher_parkinson)
list_metric_higher_parkinson[,4]<-as.numeric(as.character(list_metric_higher_parkinson[,4]))
list_metric_higher_parkinson[,1]<-as.numeric(as.character(list_metric_higher_parkinson[,1]))
list_metric_higher_parkinson[,2]<-as.numeric(as.character(list_metric_higher_parkinson[,2]))
list_metric_higher_parkinson[,3]<-as.integer(as.character(list_metric_higher_parkinson[,3]))
list_metric_higher_parkinson[,5]<-as.integer(as.character(list_metric_higher_parkinson[,5]))
colnames(list_metric_higher_parkinson)<-c("LOOCV precision","sd","k","p-norm","dimensions","optimization")
}

list_metric_higher_parkinson[,6]<-data.frame("optimization"='yes')
list_metric_higher_parkinson_nonop[,6]<-data.frame("optimization"='no')
metric_higher_parkinson<-rbind(list_metric_higher_parkinson,list_metric_higher_parkinson_nonop)
metric_higher_parkinson[,6]<-as.factor(metric_higher_parkinson[,6])
library(ggplot2)
library(gridExtra)
library(grid)
library(corrplot)
A<-ggplot(data=metric_higher_parkinson,aes(x=`p-norm`,y=`LOOCV precision`))+theme_bw()+
  geom_line(aes(color=optimization),size=1.3)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())


B<-ggplot(data=metric_higher_parkinson,aes(x=`p-norm`,y=k))+theme_bw()+
  geom_point(aes(color=optimization))+
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

cairo_ps(width=12,height = 3.5,file='higher_metric.eps')
grid.arrange(arrangeGrob(A +theme(legend.position="none"),
                         B +theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1),
             widths = c(5,0.6))
dev.off()

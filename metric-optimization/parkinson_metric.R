
library(gridExtra)
library(grid)
library(corrplot)
{list_datasets<-NULL
for (i in 2:22){
  list_datasets[i-1]<-list(parkinsons[,c(1:i,23)])
}

list_score_metric_parkinson_nonoptk<-matrix(0,ncol = 5,nrow = 6*length(list_datasets))

metric_aux<-0
for (metric in c(1,2,3,2/3,2/5,1/7)){
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    set<-Min_max_nrmlztn(set)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);
    rownames(conf)<-levels(class);    
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
        
      tests[1,jjj]<-maux/length(levels(class))}
      test_mean<-NULL;test_sd<-NULL;
      test_mean[1]<-mean(t(tests[1,1:jjj]))
      test_sd[1]<-sd(t(tests[1,1:jjj]))
      
    list_score_metric_parkinson_nonoptk[s+metric_aux*length(list_datasets),c(1,2,3,4,5)]<-c(test_mean[which.max(test_mean)]*100,
                                                                                            test_sd[which.max(test_mean)]*100,
                                                                                            k,metric,s)
    print(list_score_metric_parkinson_nonoptk[1:(s+metric_aux*length(list_datasets)),])
  }
  metric_aux<-metric_aux+1
}
} # 5NN

{list_datasets<-NULL
  
  for (i in 2:22){
    list_datasets[i-1]<-list(parkinsons[,c((sample(1:22))[1:i],23)])
  }
  
  list_score_metric_parkinson_nonoptk_random<-matrix(0,ncol = 5,nrow = 6*length(list_datasets))
  
  metric_aux<-0
  for (metric in c(1,2,3,2/3,2/5,1/7)){
    for (s in 1:length(list_datasets)){
      set<-data.frame(list_datasets[s])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      set<-Min_max_nrmlztn(set)
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);
      rownames(conf)<-levels(class);      
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
        
        tests[1,jjj]<-maux/length(levels(class))}
      test_mean<-NULL;test_sd<-NULL;
      test_mean[1]<-mean(t(tests[1,1:jjj]))
      test_sd[1]<-sd(t(tests[1,1:jjj]))
      
      list_score_metric_parkinson_nonoptk_random[s+metric_aux*length(list_datasets),c(1,2,3,4,5)]<-c(test_mean[which.max(test_mean)]*100,
                                                                                              test_sd[which.max(test_mean)]*100,
                                                                                              k,metric,s)
      print(list_score_metric_parkinson_nonoptk_random[1:(s+metric_aux*length(list_datasets)),])
    }
    metric_aux<-metric_aux+1
  }
  
} # 5NN and randomized features

{list_score_metric_parkinson_nonoptk<-as.data.frame(list_score_metric_parkinson_nonoptk)
list_score_metric_parkinson_nonoptk[as.character(list_score_metric_parkinson_nonoptk[,4])==2/3,][,4]<-"2/3"
list_score_metric_parkinson_nonoptk[as.character(list_score_metric_parkinson_nonoptk[,4])==2/5,][,4]<-"2/5"
list_score_metric_parkinson_nonoptk[as.character(list_score_metric_parkinson_nonoptk[,4])==1/7,][,4]<-"1/7"
list_score_metric_parkinson_nonoptk[,1]<-as.numeric(as.character(list_score_metric_parkinson_nonoptk[,1]))
list_score_metric_parkinson_nonoptk[,2]<-as.numeric(as.character(list_score_metric_parkinson_nonoptk[,2]))
list_score_metric_parkinson_nonoptk[,3]<-as.integer(as.character(list_score_metric_parkinson_nonoptk[,3]))
list_score_metric_parkinson_nonoptk[,5]<-as.integer(as.character(list_score_metric_parkinson_nonoptk[,5]))
colnames(list_score_metric_parkinson_nonoptk)<-c("LOOCV precision","sd","k","p-norm","dimensions")

list_score_metric_parkinson_nonoptk_random<-as.data.frame(list_score_metric_parkinson_nonoptk_random)
list_score_metric_parkinson_nonoptk_random[as.character(list_score_metric_parkinson_nonoptk_random[,4])==2/3,][,4]<-"2/3"
list_score_metric_parkinson_nonoptk_random[as.character(list_score_metric_parkinson_nonoptk_random[,4])==2/5,][,4]<-"2/5"
list_score_metric_parkinson_nonoptk_random[as.character(list_score_metric_parkinson_nonoptk_random[,4])==1/7,][,4]<-"1/7"
list_score_metric_parkinson_nonoptk_random[,1]<-as.numeric(as.character(list_score_metric_parkinson_nonoptk_random[,1]))
list_score_metric_parkinson_nonoptk_random[,2]<-as.numeric(as.character(list_score_metric_parkinson_nonoptk_random[,2]))
list_score_metric_parkinson_nonoptk_random[,3]<-as.integer(as.character(list_score_metric_parkinson_nonoptk_random[,3]))
list_score_metric_parkinson_nonoptk_random[,5]<-as.integer(as.character(list_score_metric_parkinson_nonoptk_random[,5]))
colnames(list_score_metric_parkinson_nonoptk_random)<-c("LOOCV precision","sd","k","p-norm","dimensions")

ymax<-max(max(list_score_metric_parkinson_nonoptk[,1]),max(list_score_metric_parkinson_nonoptk_random[,1]))
ymin<-min(min(list_score_metric_parkinson_nonoptk[,1]),min(list_score_metric_parkinson_nonoptk_random[,1]))
  A<-ggplot(data=list_score_metric_parkinson_nonoptk,aes(x=dimensions,y=`LOOCV precision`))+theme_bw()+
  geom_smooth(aes(x=dimensions,colour=`p-norm`),se=FALSE)+
  ylim(ymin,ymax)+xlim(1,21)+  geom_line(aes(x=dimensions,colour=`p-norm`),alpha=.4)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
  
    B<-ggplot(data=list_score_metric_parkinson_nonoptk_random,aes(x=dimensions,y=`LOOCV precision`))+theme_bw()+
    geom_smooth(aes(x=dimensions,colour=`p-norm`),se=FALSE)+
    ylim(ymin,ymax)+xlim(1,21)+ geom_line(aes(x=dimensions,colour=`p-norm`),alpha=.4)+
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
  
  cairo_ps(width=12,height = 3.5,file='5NN_metric_parkinson.eps')
  grid.arrange(arrangeGrob(A +theme(legend.position="none"),
                           B + ylab("")+theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend1),
               widths = c(5,0.4))
  dev.off()
  
  
} #plots

{matrix_aux<-NULL
  levels<-c("3","2","1","2/3","2/5","1/7")
  
  { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
    y<-1;x<-1
    
    for (i in levels){
      for (j in levels){
        cont[x,y]<-sum(as.integer((list_score_metric_parkinson_nonoptk)[list_score_metric_parkinson_nonoptk[,4]==j,][,1]<(list_score_metric_parkinson_nonoptk)[list_score_metric_parkinson_nonoptk[,4]==i,][,1]))/length(list_datasets)
        y<-y+1}
      y<-1
      x<-x+1      
    }
    
    rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
    colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
    
    
    matrix_aux[[1]]<-floor(cont*1000)/10
  } 
  
  { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
  y<-1;x<-1
  
  for (i in levels){
    for (j in levels){
      cont[x,y]<-sum(as.integer((list_score_metric_parkinson_nonoptk_random)[list_score_metric_parkinson_nonoptk_random[,4]==j,][,1]<(list_score_metric_parkinson_nonoptk_random)[list_score_metric_parkinson_nonoptk_random[,4]==i,][,1]))/length(list_datasets)
      y<-y+1}
    y<-1
    x<-x+1      
  }
  
  rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
  colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
  
  
  matrix_aux[[2]]<-floor(cont*1000)/10
} 
  
  A<-as.matrix(matrix_aux[[1]])
  B<-as.matrix(matrix_aux[[2]])
  cairo_ps(width=3,height = 3 ,file='5NN_metric_parkinson_confusion1.eps')
  corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1,number.cex = 1,cl.pos="n",tl.srt=0)
  dev.off()
  cairo_ps(width=3,height = 3 ,file='5NN_metric_parkinson_confusion2.eps')
    corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1,number.cex = 1,cl.pos="n",tl.srt=0)
  dev.off()} ###confusion matrices plot

{list_datasets<-NULL

for (i in 2:22){
  list_datasets[i-1]<-list(parkinsons[,c(1:i,23)])
}

list_score_metric_parkinson<-matrix(0,ncol = 5,nrow = 6*length(list_datasets))

metric_aux<-0
for (metric in c(1,2,3,2/3,2/5,1/7)){
for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  set<-Min_max_nrmlztn(set)
  trains<-data.frame(NULL)
  tests<-data.frame(NULL)
  conf<-matrix(0,length(levels(class)),length(levels(class)))
  colnames(conf)<-levels(class);
  rownames(conf)<-levels(class);  
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
  list_score_metric_parkinson[s+metric_aux*length(list_datasets),c(1,2,3,4,5)]<-c(test_mean[which.max(test_mean)]*100,
                                                                                  test_sd[which.max(test_mean)]*100,
                                                                                  which.max(test_mean),
                                                                                  metric,s)
  print(list_score_metric_parkinson[1:(s+metric_aux*length(list_datasets)),])
}
metric_aux<-metric_aux+1
}

} #optimized-k

{list_datasets_random<-NULL
  
  for (i in 2:22){
    list_datasets_random[i-1]<-list(parkinsons[,c((sample(1:22))[1:i],23)])
  }
  
  list_score_metric_parkinson_random<-matrix(0,ncol = 5,nrow = 6*length(list_datasets_random))
  
  metric_aux<-0
  for (metric in c(1,2,3,2/3,2/5,1/7)){
    for (s in 1:length(list_datasets)){
      set<-data.frame(list_datasets[s])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      set<-Min_max_nrmlztn(set)
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);
      rownames(conf)<-levels(class);      
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
      list_score_metric_parkinson_random[s+metric_aux*length(list_datasets),c(1,2,3,4,5)]<-c(test_mean[which.max(test_mean)]*100,
                                                                                      test_sd[which.max(test_mean)]*100,
                                                                                      which.max(test_mean),
                                                                                      metric,s)
      print(list_score_metric_parkinson_random[1:(s+metric_aux*length(list_datasets)),])
    }
    metric_aux<-metric_aux+1
  }
  
} #optimized-k and randomized features

{list_score_metric_parkinson<-as.data.frame(list_score_metric_parkinson)
  list_score_metric_parkinson[as.character(list_score_metric_parkinson[,4])==2/3,][,4]<-"2/3"
  list_score_metric_parkinson[as.character(list_score_metric_parkinson[,4])==2/5,][,4]<-"2/5"
  list_score_metric_parkinson[as.character(list_score_metric_parkinson[,4])==1/7,][,4]<-"1/7"
  list_score_metric_parkinson[,1]<-as.numeric(as.character(list_score_metric_parkinson[,1]))
  list_score_metric_parkinson[,2]<-as.numeric(as.character(list_score_metric_parkinson[,2]))
  list_score_metric_parkinson[,3]<-as.integer(as.character(list_score_metric_parkinson[,3]))
  list_score_metric_parkinson[,5]<-as.integer(as.character(list_score_metric_parkinson[,5]))
  colnames(list_score_metric_parkinson)<-c("LOOCV precision","sd","k","p-norm","dimensions")
  
  list_score_metric_parkinson_random<-as.data.frame(list_score_metric_parkinson_random)
  list_score_metric_parkinson_random[as.character(list_score_metric_parkinson_random[,4])==2/3,][,4]<-"2/3"
  list_score_metric_parkinson_random[as.character(list_score_metric_parkinson_random[,4])==2/5,][,4]<-"2/5"
  list_score_metric_parkinson_random[as.character(list_score_metric_parkinson_random[,4])==1/7,][,4]<-"1/7"
  list_score_metric_parkinson_random[,1]<-as.numeric(as.character(list_score_metric_parkinson_random[,1]))
  list_score_metric_parkinson_random[,2]<-as.numeric(as.character(list_score_metric_parkinson_random[,2]))
  list_score_metric_parkinson_random[,3]<-as.integer(as.character(list_score_metric_parkinson_random[,3]))
  list_score_metric_parkinson_random[,5]<-as.integer(as.character(list_score_metric_parkinson_random[,5]))
  colnames(list_score_metric_parkinson_random)<-c("LOOCV precision","sd","k","p-norm","dimensions")
  
  ymax<-max(max(list_score_metric_parkinson[,1]),max(list_score_metric_parkinson_random[,1]))
  ymin<-min(min(list_score_metric_parkinson[,1]),min(list_score_metric_parkinson_random[,1]))
  
  C<-ggplot(data=list_score_metric_parkinson,aes(x=dimensions,y=`LOOCV precision`))+theme_bw()+
    geom_smooth(aes(x=dimensions,colour=`p-norm`),se=FALSE)+ylim(ymin,ymax)+xlim(1,21)+
    geom_line(aes(x=dimensions,colour=`p-norm`),alpha=.4)+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  D<-ggplot(data=list_score_metric_parkinson_random,aes(x=dimensions,y=`LOOCV precision`))+theme_bw()+
    geom_smooth(aes(x=dimensions,colour=`p-norm`),se=FALSE)+ylim(ymin,ymax)+xlim(1,21)+
    geom_line(aes(x=dimensions,colour=`p-norm`),alpha=.4)+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  mylegend1<-g_legend(C)
  
  cairo_ps(width=12,height = 3.5,file='kopt_metric_parkinson.eps')
  grid.arrange(arrangeGrob(C +theme(legend.position="none"),
                           D + ylab("")+theme(legend.position="none"),
                           nrow=1),
               arrangeGrob(mylegend1),
               widths = c(5,0.4))
  dev.off()
  
  
} #plots

{matrix_aux<-NULL
  levels<-c("3","2","1","2/3","2/5","1/7")
  
  { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
    y<-1;x<-1
    
    for (i in levels){
      for (j in levels){
        cont[x,y]<-sum(as.integer((list_score_metric_parkinson)[list_score_metric_parkinson[,4]==j,][,1]<(list_score_metric_parkinson)[list_score_metric_parkinson[,4]==i,][,1]))/length(list_datasets)
        y<-y+1}
      y<-1
      x<-x+1      
    }
    
    rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
    colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
    
    
    matrix_aux[[1]]<-floor(cont*1000)/10
  } 
  
  { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
    y<-1;x<-1
    
    for (i in levels){
      for (j in levels){
        cont[x,y]<-sum(as.integer((list_score_metric_parkinson_random)[list_score_metric_parkinson_random[,4]==j,][,1]<(list_score_metric_parkinson_random)[list_score_metric_parkinson_random[,4]==i,][,1]))/length(list_datasets)
        y<-y+1}
      y<-1
      x<-x+1      
    }
    
    rownames(cont)<-c("3","2","1","2/3","2/5","1/7")
    colnames(cont)<-c("3","2","1","2/3","2/5","1/7")
    
    
    matrix_aux[[2]]<-floor(cont*1000)/10
  } 
  
  A<-as.matrix(matrix_aux[[1]])
  B<-as.matrix(matrix_aux[[2]])
  cairo_ps(width=3,height = 3 ,file='kopt_metric_parkinson_confusion1.eps')
  corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1,number.cex = 1,cl.pos="n",tl.srt=0)
  dev.off()
  cairo_ps(width=3,height = 3 ,file='kopt_metric_parkinson_confusion2.eps')
  corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1,number.cex = 1,cl.pos="n",tl.srt=0)
  dev.off()} ###confusion matrices plot
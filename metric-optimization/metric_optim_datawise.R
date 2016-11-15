list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)


{
  
  load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/normalization/norm_k_optim_imb_loocv.RData")
  
  normes<-matrix(0,nrow=8*4,ncol=4)
  
  for (i in 0:7){
    #normes[(1+4*i):(4+4*i),1]<-as.numeric(normes[(1+4*i):(4+4*i),1])-as.numeric(min(normes[(1+4*i):(4+4*i),1]))
    normes[(1+4*i):(4+4*i),]<-as.character(norm_k_optim[[i+1]][1:4,])
  }
  
  normes<-as.data.frame(normes)
  normes[,1]<-as.numeric(as.character(normes[,1]))
  normes[,2]<-as.numeric(as.character(normes[,2]))
  normes[,3]<-as.numeric(as.character(normes[,3]))
  for (i in 0:7){
    normes[(1+4*i):(4+4*i),5]<-(c('plasma','psyconvert','bloodabeta','adni','proteom','neuropsy','neuroimag','parkinson'))[i+1]
    #normes[(1+4*i):(4+4*i),]<-as.character(norm_k_optim[[i+1]][1:4,])
  }
  normes[,5]<-as.factor(normes[,5])
  
  colnames(normes)<-c("average precision","sd","k","normalization","dataset")
  
} #normes

{
#load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/metric optimization/metric_optim_dataset.RData")

metric_optim_dataset<-data.frame(normes)
metric_range<-seq(.1,3,.1)
print(metric_optim_dataset)
metric_optim_dataset[,6:(5+length(metric_range))]<-0
colnames(metric_optim_dataset)<-c(colnames(normes),paste0(as.character(metric_range)))
metric_opt_nonk<-data.frame()
  for (i in 0:7){
  metric_opt_nonk<-rbind(metric_opt_nonk,metric_optim_dataset[(1+4*i):(4+4*i),][(which.max(metric_optim_dataset[(1+4*i):(4+4*i),][,1])),])
  }
colnames(metric_opt_nonk)<-c(colnames(normes),paste0(as.character(metric_range)))
metric_opt_nonk_sd<-metric_opt_nonk
for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  
  #normalized set
  set<-select_normalization(set,as.character(metric_opt_nonk$normalization[ss]))
  
  #defined k
  k<-as.numeric(metric_opt_nonk$k[ss])
  metric_aux<-5
  for (metric in metric_range){
  {   trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);
      rownames(conf)<-levels(class);
}      
 
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
        
    metric_aux<-metric_aux+1
    metric_opt_nonk[ss,metric_aux]<-test_mean*100
    metric_opt_nonk_sd[ss,metric_aux]<-test_sd*100
    View(metric_opt_nonk)
    View(metric_opt_nonk_sd)
    } 
} #LOOCV
} #using old norm and k from norm

{ metric_opt_k<-data.frame()
  for (i in 0:7){
    metric_opt_k<-rbind(metric_opt_k,metric_optim_dataset[(1+4*i):(4+4*i),][(which.max(metric_optim_dataset[(1+4*i):(4+4*i),][,1])),])
  }
  colnames(metric_opt_k)<-c(colnames(normes),paste0(as.character(metric_range)))
  metric_opt_k_sd<-metric_opt_k
  metric_opt_k_k<-metric_opt_k
  
for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    
    set<-select_normalization(set,as.character(metric_opt_k$normalization[ss]))
    
    metric_aux<-5
    for (metric in metric_range){
     
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      for (k in 1:floor(sqrt(nrow(set)))){
        print(k)
        for (jjj in 1:2){
          for (j in 1:nrow(set)){
            train<-set[-j,]
            cltra<-class[-j]
            test<-set[j,]
            cltes<-class[j]
            #data<-undersample(train,cltra,rnorm(1),1)
            #train<-data[[1]]
            #cltra<-data[[2]]
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
      
      for (i in 1:k){
        test_mean[i]<-mean(t(tests[i,1:jjj]))
        test_sd[i]<-sd(t(tests[i,1:jjj]))
      }
      
      metric_aux<-metric_aux+1
      metric_opt_k[ss,metric_aux]<-test_mean[which.max(test_mean)]*100
      metric_opt_k_sd[ss,metric_aux]<-test_sd[which.max(test_mean)]*100
      metric_opt_k_k[ss,metric_aux]<-which.max(test_mean)
      View(metric_opt_k)
    } 
  } #LOOCV
} 
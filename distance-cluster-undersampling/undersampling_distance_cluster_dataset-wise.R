library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)
load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/random undersampling/metric_and_norm_opt_by_daset.RData")
list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
list_score_structured<-matrix(0,ncol = 6*5,nrow = length(list_datasets))
metric_opt_k_max<-metric_opt_k_max[c(3,1,7,8,2,5,6,4),]


{
    for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-2
  pnorm<-2
  
  perc<-.5; #perc<-seq(0,1.0,0.1)
  conf<-matrix(0,length(levels(class)),length(levels(class)))
  
  trains<-data.frame(NULL)
  tests<-data.frame(NULL)
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1;s<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  
  for (k in 1:floor(sqrt(nrow(set)))){
    print(c(k,"of",floor(sqrt(nrow(set)))))
    
      for (j in c(1:CV)){
        
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      data<-nearmiss1(train,cltra,perc,2)
      train<-data[[1]]
      cltra<-data[[2]]
      #test<-rbind(test,data[[3]])
      #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,k,pnorm)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in levels(class)){
        for(jj in levels(class)){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      
      maux<-0
      for (m in 1:length(levels(class))){
        maux<-conf[m,m]+maux}
      tests[k,j]<-maux/length(levels(class))}
    }
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:k){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  
  list_score_structured[ss,1]<-test_mean[which.max(test_mean)]*100
  list_score_structured[ss,2]<-test_sd[which.max(test_mean)]*100
  list_score_structured[ss,3]<-which.max(test_mean)
  print(list_score_structured)} #2-fold CV

  for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    perc<-.5; #perc<-seq(0,1.0,0.1)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1;s<-1
    classes_length<-1:length(class)
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(k)
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          print(j)
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          data<-nearmiss1(train,cltra,perc,2)
          train<-data[[1]];cltra<-data[[2]];
          
          #test<-rbind(test,data[[3]]);
          #cltes<-as.factor(c(as.character(cltes),as.character(data[[4]])))
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
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
    
    list_score_structured[ss,4]<-test_mean[which.max(test_mean)]*100
    list_score_structured[ss,5]<-test_sd[which.max(test_mean)]*100
    list_score_structured[ss,6]<-which.max(test_mean)
    print(list_score_structured)
  } #LOOCV
  } #NEARMISS1
{
  for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-2
  pnorm<-2
  
  perc<-.5; #perc<-seq(0,1.0,0.1)
  conf<-matrix(0,length(levels(class)),length(levels(class)))
  
  trains<-data.frame(NULL)
  tests<-data.frame(NULL)
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1;s<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  
  for (k in 1:floor(sqrt(nrow(set)))){
    print(c(k,"of",floor(sqrt(nrow(set)))))
    
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      data<-nearmiss2(train,cltra,perc,2)
      train<-data[[1]]
      cltra<-data[[2]]
      #test<-rbind(test,data[[3]])
      cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,k,pnorm)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in levels(class)){
        for(jj in levels(class)){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      
      maux<-0
      for (m in 1:length(levels(class))){
        maux<-conf[m,m]+maux}
      tests[k,j]<-maux/length(levels(class))}
    }
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:k){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  
  list_score_structured[ss,7]<-test_mean[which.max(test_mean)]*100
  list_score_structured[ss,8]<-test_sd[which.max(test_mean)]*100
  list_score_structured[ss,9]<-which.max(test_mean)
  print(list_score_structured)} #2-fold CV
  
  for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    perc<-.5; #perc<-seq(0,1.0,0.1)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1;s<-1
    classes_length<-1:length(class)
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(k)
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          data<-nearmiss2(train,cltra,perc,2)
          train<-data[[1]];cltra<-data[[2]];
          
          #test<-rbind(test,data[[3]]);
          #cltes<-as.factor(c(as.character(cltes),as.character(data[[4]])))
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
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
    
    list_score_structured[ss,10]<-test_mean[which.max(test_mean)]*100
    list_score_structured[ss,11]<-test_sd[which.max(test_mean)]*100
    list_score_structured[ss,12]<-which.max(test_mean)
    print(list_score_structured)
  } #LOOCV
 
} #NEARMISS2
{
  for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-2
  pnorm<-2
  
  perc<-.5; #perc<-seq(0,1.0,0.1)
  conf<-matrix(0,length(levels(class)),length(levels(class)))
  
  trains<-data.frame(NULL)
  tests<-data.frame(NULL)
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1;s<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  
  for (k in 1:floor(sqrt(nrow(set)))){
    print(c(k,"of",floor(sqrt(nrow(set)))))
    
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      data<-nearmiss3(train,cltra,perc,2)
      train<-data[[1]]
      cltra<-data[[2]]
      #test<-rbind(test,data[[3]])
      cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,k,pnorm)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in levels(class)){
        for(jj in levels(class)){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      
      maux<-0
      for (m in 1:length(levels(class))){
        maux<-conf[m,m]+maux}
      tests[k,j]<-maux/length(levels(class))}
    }
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:k){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  
  list_score_structured[ss,13]<-test_mean[which.max(test_mean)]*100
  list_score_structured[ss,14]<-test_sd[which.max(test_mean)]*100
  list_score_structured[ss,15]<-which.max(test_mean)
  print(list_score_structured)} #2-fold CV
  for (ss in 2:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    perc<-.5; #perc<-seq(0,1.0,0.1)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1;s<-1
    classes_length<-1:length(class)
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(k)
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          data<-nearmiss3(train,cltra,perc,2)
          train<-data[[1]];cltra<-data[[2]];
          
          #test<-rbind(test,data[[3]]);
          #cltes<-as.factor(c(as.character(cltes),as.character(data[[4]])))
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
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
    
    list_score_structured[ss,16]<-test_mean[which.max(test_mean)]*100
    list_score_structured[ss,17]<-test_sd[which.max(test_mean)]*100
    list_score_structured[ss,18]<-which.max(test_mean)
    print(list_score_structured)
  } #LOOCV
     
} #NEARMISS3
{
  for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-2
  pnorm<-2
  
  perc<-.5; #perc<-seq(0,1.0,0.1)
  conf<-matrix(0,length(levels(class)),length(levels(class)))
  
  trains<-data.frame(NULL)
  tests<-data.frame(NULL)
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1;s<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  
  for (k in 1:floor(sqrt(nrow(set)))){
    print(c(k,"of",floor(sqrt(nrow(set)))))
    
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      data<-nearmiss4(train,cltra,perc,2)
      train<-data[[1]]
      cltra<-data[[2]]
      #test<-rbind(test,data[[3]])
      cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,k,pnorm)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in levels(class)){
        for(jj in levels(class)){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      
      maux<-0
      for (m in 1:length(levels(class))){
        maux<-conf[m,m]+maux}
      tests[k,j]<-maux/length(levels(class))}
    }
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:k){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  
  list_score_structured[ss,19]<-test_mean[which.max(test_mean)]*100
  list_score_structured[ss,20]<-test_sd[which.max(test_mean)]*100
  list_score_structured[ss,21]<-which.max(test_mean)
  print(list_score_structured)} #2-fold CV
  for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    perc<-.5; #perc<-seq(0,1.0,0.1)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1;s<-1
    classes_length<-1:length(class)
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(k)
      for (jjj in 1:2){
        for (j in 1:nrow(set)){
          train<-set[-j,]
          cltra<-class[-j]
          test<-set[j,]
          cltes<-class[j]
          data<-nearmiss4(train,cltra,perc,2)
          train<-data[[1]];cltra<-data[[2]];
          
          #test<-rbind(test,data[[3]]);
          #cltes<-as.factor(c(as.character(cltes),as.character(data[[4]])))
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
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
    
    list_score_structured[ss,22]<-test_mean[which.max(test_mean)]*100
    list_score_structured[ss,23]<-test_sd[which.max(test_mean)]*100
    list_score_structured[ss,24]<-which.max(test_mean)
    print(list_score_structured)
  } #LOOCV

} #NEARMISS4
{
      for (ss in 1:length(list_datasets)){
      set<-data.frame(list_datasets[ss])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      set<-Min_max_nrmlztn(set)
      
      cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
    CV<-2
      pnorm<-2
      
      perc<-.5; #perc<-seq(0,1.0,0.1)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      matrix_list<-NULL
      lista<-NULL
      ii<-1;k<-1;s<-1
      classes_length<-1:length(class)
      
      for(i in levels(class)){
        cl_length<-(1:length(class))[class==i]
        lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
        ii<-ii+1}
      
      
      for (k in 1:floor(sqrt(nrow(set)))){
        print(c(k,"of",floor(sqrt(nrow(set)))))
        
        for (j in c(1:CV)){
          
          train<-NULL;test<-NULL
          for (ii in 1:length(levels(class))){
            train<-c(lista[[ii]][,-j],train)
            test<-c(lista[[ii]][,j],test)}
          
          cltra<-class[train]; train<-set[train,]
          cltest<-class[test]; test<-set[test,]
          
          data<-mostdistant(train,cltra,perc,2)
          train<-data[[1]]
          cltra<-data[[2]]
          #test<-rbind(test,data[[3]])
          cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
          modtest<-as.character(ans1[[1]])
          cltest<-as.character(cltest)
          
          datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
          colnames(datag)<-c('predict','correct')
          conf <- data.frame()
          for (ii in levels(class)){
            for(jj in levels(class)){
              aux1<-datag[datag$correct==ii,]
              aux2<-aux1[aux1$predict==jj,]
              conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
          
          maux<-0
          for (m in 1:length(levels(class))){
            maux<-conf[m,m]+maux}
          tests[k,j]<-maux/length(levels(class))}
        }
      
      test_mean<-NULL
      test_sd<-NULL
      
      for (i in 1:k){
        test_mean[i]<-mean(t(tests[i,1:CV]))
        test_sd[i]<-sd(t(tests[i,1:CV]))
      }
      
      list_score_structured[ss,25]<-test_mean[which.max(test_mean)]*100
      list_score_structured[ss,26]<-test_sd[which.max(test_mean)]*100
      list_score_structured[ss,27]<-which.max(test_mean)
      print(list_score_structured)} #2-fold CV
      for (ss in 1:length(list_datasets)){
        set<-data.frame(list_datasets[ss])
        row.names(set)<-NULL
        class<-set[,ncol(set)]
        class<-as.factor(as.character(class))
        set<-set[,-ncol(set)]
        row.names(set)<-NULL
        set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
        pnorm<-metric_opt_k_max$variable[ss]
        perc<-.5; #perc<-seq(0,1.0,0.1)
        conf<-matrix(0,length(levels(class)),length(levels(class)))
        colnames(conf)<-levels(class);rownames(conf)<-levels(class)
        trains<-data.frame(NULL)
        tests<-data.frame(NULL)
        matrix_list<-NULL
        lista<-NULL
        ii<-1;k<-1;s<-1
        classes_length<-1:length(class)
        
        for (k in 1:floor(sqrt(nrow(set)))){
          print(k)
          for (jjj in 1:2){
            for (j in 1:nrow(set)){
              train<-set[-j,]
              cltra<-class[-j]
              test<-set[j,]
              cltes<-class[j]
              data<-mostdistant(train,cltra,perc,2)
              train<-data[[1]];cltra<-data[[2]];
              
              #test<-rbind(test,data[[3]]);
              #cltes<-as.factor(c(as.character(cltes),as.character(data[[4]])))
              
              ans1 <- knn_random_break(train,test,cltra,k,pnorm)
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
        
        list_score_structured[ss,28]<-test_mean[which.max(test_mean)]*100
        list_score_structured[ss,29]<-test_sd[which.max(test_mean)]*100
        list_score_structured[ss,30]<-which.max(test_mean)
        print(list_score_structured)
      } #LOOCV
      
      } #MOSDISTANT
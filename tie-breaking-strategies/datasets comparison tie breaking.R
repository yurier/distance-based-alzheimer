list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
list_score_tie_breaking<-matrix(0,ncol = 3*3,nrow = 2*length(list_datasets))
{
for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
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
    ans1 <- knn_minus1_break(train,test,cltra,k,2)
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
  
  list_score_tie_breaking[s,1]<-test_mean[which.max(test_mean)]*100
  list_score_tie_breaking[s,2]<-test_sd[which.max(test_mean)]*100
  list_score_tie_breaking[s,3]<-which.max(test_mean)
  print(list_score_tie_breaking)
} #LOOCV

for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-ifelse(cut_CV>=2,2,2)
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
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      #test<-rbind(test,data[[3]])
      #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_minus1_break(train,test,cltra,k,pnorm)
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
  
  list_score_tie_breaking[ss+length(list_datasets),1]<-test_mean[which.max(test_mean)]*100
  list_score_tie_breaking[ss+length(list_datasets),2]<-test_sd[which.max(test_mean)]*100
  list_score_tie_breaking[ss+length(list_datasets),3]<-which.max(test_mean)
  print(list_score_tie_breaking)} #2-fold CV
} #MINUS

{
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
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
          ans1 <- knn_plus1_break(train,test,cltra,k,2)
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
    
    list_score_tie_breaking[s,4]<-test_mean[which.max(test_mean)]*100
    list_score_tie_breaking[s,5]<-test_sd[which.max(test_mean)]*100
    list_score_tie_breaking[s,6]<-which.max(test_mean)
    print(list_score_tie_breaking)
  } #LOOCV
  
for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-ifelse(cut_CV>=2,2,2)
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
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      
      #test<-rbind(test,data[[3]])
      #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_plus1_break(train,test,cltra,k,pnorm)
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
  
  list_score_tie_breaking[ss+length(list_datasets),4]<-test_mean[which.max(test_mean)]*100
  list_score_tie_breaking[ss+length(list_datasets),5]<-test_sd[which.max(test_mean)]*100
  list_score_tie_breaking[ss+length(list_datasets),6]<-which.max(test_mean)
  print(list_score_tie_breaking)} #2-fold CV
} #PLUS

{
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
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
          ans1 <- knn_random_break(train,test,cltra,k,2)
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
    
    list_score_tie_breaking[s,7]<-test_mean[which.max(test_mean)]*100
    list_score_tie_breaking[s,8]<-test_sd[which.max(test_mean)]*100
    list_score_tie_breaking[s,9]<-which.max(test_mean)
    print(list_score_tie_breaking)
  } #LOOCV
  
for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    
    cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
    CV<-ifelse(cut_CV>=2,2,2)
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
      for (j in c(1:CV)){
        
        train<-NULL;test<-NULL
        for (ii in 1:length(levels(class))){
          train<-c(lista[[ii]][,-j],train)
          test<-c(lista[[ii]][,j],test)}
        
        cltra<-class[train]; train<-set[train,]
        cltest<-class[test]; test<-set[test,]
        
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
    
    list_score_tie_breaking[ss+length(list_datasets),7]<-test_mean[which.max(test_mean)]*100
    list_score_tie_breaking[ss+length(list_datasets),8]<-test_sd[which.max(test_mean)]*100
    list_score_tie_breaking[ss+length(list_datasets),9]<-which.max(test_mean)
    print(list_score_tie_breaking)} #2-fold CV 
  } #RANDOM
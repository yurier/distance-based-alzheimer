BION<-data.frame(BIO[,-ncol(BIO)],rnorm(nrow(BIO)),BIO[,ncol(BIO)])
list_datasets<-list(BION[,c(4,6,ncol(BION))],BION[,c(2,4,8,ncol(BION))],BION[,c(1,2,4,8,ncol(BION))],BION[,c(2,4,6,8,9,ncol(BION))],BION[,c(1,4,6,7,8,9,ncol(BION))],BION[,c(1,2,4,6,7,8,9,ncol(BION))])
list_score<-matrix(0,ncol = 3*3,nrow = 2*length(list_datasets))

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
    
    list_score[s,1]<-test_mean[which.max(test_mean)]*100
    list_score[s,2]<-test_sd[which.max(test_mean)]*100
    list_score[s,3]<-which.max(test_mean)
    print(list_score)
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
    
    list_score[ss+length(list_datasets),1]<-test_mean[which.max(test_mean)]*100
    list_score[ss+length(list_datasets),2]<-test_sd[which.max(test_mean)]*100
    list_score[ss+length(list_datasets),3]<-which.max(test_mean)
    print(list_score)} #2-fold CV 
} #imbalanced

list_datasets<-list(BION[,c(1,4,ncol(BION))],BION[,c(4,6,8,ncol(BION))],BION[,c(1,4,6,8,ncol(BION))],BION[,c(1,4,6,8,9,ncol(BION))],BION[,c(1,2,4,6,7,8,ncol(BION))],BION[,c(1,4,2,6,7,8,9,ncol(BION))])

{for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  CV<-10
  pnorm<-2
  perc<-1; #perc<-seq(0,1.0,0.1)
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
      
      data<-undersample(train,cltra,rnorm(1),perc)
      train<-data[[1]]
      cltra<-data[[2]]
      test<-rbind(test,data[[3]])
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
  
  list_score[ss,4]<-test_mean[which.max(test_mean)]*100
  list_score[ss,5]<-test_sd[which.max(test_mean)]*100
  list_score[ss,6]<-which.max(test_mean)
  print(list_score)} #10fold CV

for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  CV<-10
  pnorm<-2
  perc<-1; #perc<-seq(0,1.0,0.1)
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
        data<-undersample(train,cltra,rnorm(1),perc)
        train<-data[[1]]
        cltra<-data[[2]]
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
  
  list_score[ss+length(list_datasets),4]<-test_mean[which.max(test_mean)]*100
  list_score[ss+length(list_datasets),5]<-test_sd[which.max(test_mean)]*100
  list_score[ss+length(list_datasets),6]<-which.max(test_mean)
} #LOOCV
} #undersamplin

list_datasets<-list(BION[,c(4,6,ncol(BION))],BION[,c(4,7,9,ncol(BION))],BION[,c(1,4,6,8,ncol(BION))],BION[,c(1,2,4,7,9,ncol(BION))],BION[,c(1,2,4,6,8,9,ncol(BION))],BION[,c(1,4,2,6,7,8,9,ncol(BION))])

{
  for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    CV<-10
    pnorm<-2
    perc<-1; #perc<-seq(0,1.0,0.1)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    
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
        if (CV!=1)
        {for (ii in 1:length(levels(class))){
          train<-c(lista[[ii]][,-j],train)
          test<-c(lista[[ii]][,j],test)}} else
          {for (ii in 1:length(levels(class))){
            train<-c(lista[[ii]][-j],train)
            test<-c(lista[[ii]][j],test)}}
        
        cltra<-class[train]; train<-set[train,]
        cltest<-class[test]; test<-set[test,]
        
        data<-SMOTE(data = train,class = cltra, degree = 1,pnorm = 2,k = 5)
        
        train<-rbind(train,data[[1]])
        cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))
        
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
    
    list_score[ss,7]<-test_mean[which.max(test_mean)]*100
    list_score[ss,8]<-test_sd[which.max(test_mean)]*100
    list_score[ss,9]<-which.max(test_mean)
    print( list_score)} #10fold CV
  
  for (ss in 1:length(list_datasets)){
    set<-data.frame(list_datasets[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    pnorm<-2
    data<-SMOTE(data = set,class = class,degree = 1,pnorm = 2,k = 5)
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    trains<-data.frame(NULL)
    tests<-data.frame(NULL)
    matrix_list<-NULL
    
    for (k in 1:floor(sqrt(nrow(set)))){
      print(c(k,"of",floor(sqrt(nrow(set)))))
      
      for (jjj in 1:2){
        modtest<-NULL
        for (j in 1:nrow(set)){
          
          train<-set[-j,]
          cltra<-class[-j]
          
          if(0==length(grep(paste0("_",j,"_"),row.names(data[[1]])))){
            train<-rbind(train,data[[1]])
            cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))}  
          else {
            train<-rbind(train,(data[[1]])[-grep(paste0("_",j,"_"),row.names(data[[1]])),])
            cltra<-as.factor(c(as.character(cltra),as.character((data[[2]])[-grep(paste0("_",j,"_"),row.names(data[[1]]))])))}
          
          test<-set[j,];
          cltes<-class[j];
          
          ans1 <- knn_random_break(train,test,cltra,k,pnorm)
          
          modtest[j]<-as.character(ans1[[1]][1])}
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
    test_mean<-NULL;test_sd<-NULL
    for (i in 1:k){
      test_mean[i]<-mean(t(tests[i,1:jjj]))
      test_sd[i]<-sd(t(tests[i,1:jjj]))
    }
    
    list_score[ss+length(list_datasets),7]<-test_mean[which.max(test_mean)]*100
    list_score[ss+length(list_datasets),8]<-test_sd[which.max(test_mean)]*100
    list_score[ss+length(list_datasets),9]<-which.max(test_mean)
    print( list_score)
  } #LOOCV
  
} #smote
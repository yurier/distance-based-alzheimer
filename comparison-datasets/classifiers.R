list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
library(e1071)
library(rpart)
classifier<-data.frame(matrix(0,ncol=4*5,nrow=length(list_datasets)))

{for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    
    cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
    CV<-ifelse(cut_CV>=2,2,2)

    tests<-NULL
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1
    classes_length<-1:length(class)
    
    for(i in levels(class)){
      cl_length<-(1:length(class))[class==i]
      lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
      ii<-ii+1}
    
      for (j in c(1:CV)){
        
        train<-NULL;test<-NULL
        for (ii in 1:length(levels(class))){
          train<-c(lista[[ii]][,-j],train)
          test<-c(lista[[ii]][,j],test)}
        
        cltra<-class[train]; train<-set[train,]
        cltest<-class[test]; test<-set[test,]

        svm.model <- svm(cltra ~ ., data =  train, cost = 100, gamma = 1)
        ans1  <- as.character(predict(svm.model, test))
        modtest<-as.character(ans1)
        cltest<-as.character(cltest)
        
        datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
        colnames(datag)<-c('predict','correct')
        conf<-matrix(0,length(levels(class)),length(levels(class)))
        colnames(conf)<-levels(class);rownames(conf)<-levels(class)
        for (ii in levels(class)){
          for(jj in levels(class)){
            aux1<-datag[datag$correct==ii,]
            aux2<-aux1[aux1$predict==jj,]
            conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
        
        maux<-0
        for (m in 1:length(levels(class))){
          maux<-conf[m,m]+maux}
        tests[j]<-maux/length(levels(class))}
    
    test_mean<-NULL
    test_sd<-NULL
    
    for (i in 1:CV){
      test_mean<-mean(tests[1:CV])
      test_sd<-sd(tests[1:CV])
    }
    
    classifier[s,1]<-test_mean*100
    classifier[s,2]<-test_sd*100
    print(classifier)} #2-fold CV

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
  modtest<-NULL
  for (j in 1:nrow(set)){
    print(j)
    train<-set[-j,]
    cltra<-class[-j]
    test<-set[j,]
    cltes<-class[j]
    svm.model <- svm(cltra ~ ., data =  train, cost = 100, gamma = 1)
    ans1  <- as.character(predict(svm.model, test))
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
  
  test<-maux/length(levels(class))
  
  classifier[s,3]<-test*100
  classifier[s,4]<-0
  print(classifier)
} #LOOCV
  }
{library(party)

for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-ifelse(cut_CV>=2,2,2)
  
  tests<-NULL
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  for (j in c(1:CV)){
    
    train<-NULL;test<-NULL
    for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)}
    
    cltra<-class[train]; train<-set[train,]
    cltest<-class[test]; test<-set[test,]
    x.ct <- ctree(cltra ~ ., data=train)
    ans1 <- as.character(predict(x.ct, newdata=test))
    modtest<-as.character(ans1)
    cltest<-as.character(cltest)
    
    datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
    colnames(datag)<-c('predict','correct')
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    for (ii in levels(class)){
      for(jj in levels(class)){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    
    maux<-0
    for (m in 1:length(levels(class))){
      maux<-conf[m,m]+maux}
    tests[j]<-maux/length(levels(class))}
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:CV){
    test_mean<-mean(tests[1:CV])
    test_sd<-sd(tests[1:CV])
  }
  
  classifier[s,5]<-test_mean*100
  classifier[s,6]<-test_sd*100
  print(classifier)} #2-fold CV

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
  modtest<-NULL
  for (j in 1:nrow(set)){
    print(j)
    train<-set[-j,]
    cltra<-class[-j]
    test<-set[j,]
    cltes<-class[j]
    x.ct <- ctree(cltra ~ ., data=train)
    ans1 <- as.character(predict(x.ct, newdata=test))
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
  
  test<-maux/length(levels(class))
  
  classifier[s,7]<-test*100
  classifier[s,8]<-0
  print(classifier)
} #LOOCV
  }
{
library(ipred)
for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-ifelse(cut_CV>=2,2,2)
  
  tests<-NULL
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  for (j in c(1:CV)){
    
    train<-NULL;test<-NULL
    for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)}
    
    cltra<-class[train]; train<-set[train,]
    cltest<-class[test]; test<-set[test,]
    x.ip <- bagging(cltra ~ ., data=train)
    ans1 <- as.character(predict(x.ip, newdata=test))
    modtest<-as.character(ans1)
    cltest<-as.character(cltest)
    
    datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
    colnames(datag)<-c('predict','correct')
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    for (ii in levels(class)){
      for(jj in levels(class)){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    
    maux<-0
    for (m in 1:length(levels(class))){
      maux<-conf[m,m]+maux}
    tests[j]<-maux/length(levels(class))}
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:CV){
    test_mean<-mean(tests[1:CV])
    test_sd<-sd(tests[1:CV])
  }
  
  classifier[s,9]<-test_mean*100
  classifier[s,10]<-test_sd*100
  print(classifier)} #2-fold CV

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
  modtest<-NULL
  for (j in 1:nrow(set)){
    print(j)
    train<-set[-j,]
    cltra<-class[-j]
    test<-set[j,]
    cltes<-class[j]
    x.ip <- bagging(cltra ~ ., data=train)
    ans1 <- as.character(predict(x.ip, newdata=test))
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
  
  test<-maux/length(levels(class))
  
  classifier[s,11]<-test*100
  classifier[s,12]<-0
  print(classifier)
} #LOOCV
}
{
  library(class)
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-Min_max_nrmlztn(set)
    
    cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
    CV<-ifelse(cut_CV>=2,2,2)
    
    tests<-NULL
    matrix_list<-NULL
    lista<-NULL
    ii<-1;k<-1
    classes_length<-1:length(class)
    
    for(i in levels(class)){
      cl_length<-(1:length(class))[class==i]
      lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
      ii<-ii+1}
    
    for (j in c(1:CV)){
      
      train<-NULL;test<-NULL
      for (ii in 1:length(levels(class))){
        train<-c(lista[[ii]][,-j],train)
        test<-c(lista[[ii]][,j],test)}
      
      cltra<-class[train]; train<-set[train,]
      cltest<-class[test]; test<-set[test,]
      cd <- lvqinit(train, cltra, 10)
      ans1 <- as.character(lvqtest(cd, test = test))
      modtest<-as.character(ans1)
      cltest<-as.character(cltest)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
      colnames(datag)<-c('predict','correct')
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      for (ii in levels(class)){
        for(jj in levels(class)){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      
      maux<-0
      for (m in 1:length(levels(class))){
        maux<-conf[m,m]+maux}
      tests[j]<-maux/length(levels(class))}
    
    test_mean<-NULL
    test_sd<-NULL
    
    for (i in 1:CV){
      test_mean<-mean(tests[1:CV])
      test_sd<-sd(tests[1:CV])
    }
    
    classifier[s,13]<-test_mean*100
    classifier[s,14]<-test_sd*100
    print(classifier)} #2-fold CV
  
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
    modtest<-NULL
    for (j in 1:nrow(set)){
      print(j)
      train<-set[-j,]
      cltra<-class[-j]
      test<-set[j,]
      cltes<-class[j]
      cd <- lvqinit(train, cltra, 10)
      ans1 <- as.character(lvqtest(cd, test = test))
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
    
    test<-maux/length(levels(class))
    
    classifier[s,15]<-test*100
    classifier[s,16]<-0
    print(classifier)
  } #LOOCV
}

{library(kknn)
for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
  CV<-ifelse(cut_CV>=2,2,2)
  
  tests<-NULL
  matrix_list<-NULL
  lista<-NULL
  ii<-1;k<-1
  classes_length<-1:length(class)
  
  for(i in levels(class)){
    cl_length<-(1:length(class))[class==i]
    lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
    ii<-ii+1}
  
  for (j in c(1:CV)){
    
    train<-NULL;test<-NULL
    for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)}
    
    cltra<-class[train]; train<-set[train,]
    cltest<-class[test]; test<-set[test,]
    data.kknn <- kknn(cltra~., train, test, distance = 1,kernel = "triangular")
    ans1 <- as.character(fitted(data.kknn))
    modtest<-as.character(ans1)
    cltest<-as.character(cltest)
    
    datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
    colnames(datag)<-c('predict','correct')
    conf<-matrix(0,length(levels(class)),length(levels(class)))
    colnames(conf)<-levels(class);rownames(conf)<-levels(class)
    for (ii in levels(class)){
      for(jj in levels(class)){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    
    maux<-0
    for (m in 1:length(levels(class))){
      maux<-conf[m,m]+maux}
    tests[j]<-maux/length(levels(class))}
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in 1:CV){
    test_mean<-mean(tests[1:CV])
    test_sd<-sd(tests[1:CV])
  }
  
  classifier[s,17]<-test_mean*100
  classifier[s,18]<-test_sd*100
  print(classifier)} #2-fold CV

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
  modtest<-NULL
  for (j in 1:nrow(set)){
    print(j)
    train<-set[-j,]
    cltra<-class[-j]
    test<-set[j,]
    cltes<-class[j]
    data.kknn <- kknn(cltra~., train, test, distance = 1,kernel = "triangular")
    ans1 <- as.character(fitted(data.kknn))
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
  
  test<-maux/length(levels(class))
  
  classifier[s,19]<-test*100
  classifier[s,20]<-0
  print(classifier)
} #LOOCV
}
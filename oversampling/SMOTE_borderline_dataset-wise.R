    library(corrplot)
    library(mvtnorm)
    library(ggplot2)
    library(class)
    library(MASS)
    library(gridExtra)
    library(grid)
    #load("~/Dropbox/Projeto Alzheimer/Thesis/Examples/random undersampling/metric_and_norm_opt_by_daset.RData")
    list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
    list_score_SMOTE_bordeline<-matrix(0,ncol = 6*4,nrow = length(list_datasets))
    #metric_opt_k_max<-metric_opt_k_max[c(3,1,7,8,2,5,6,4),]
    

{
      for (ss in 1:length(list_datasets)){
        set<-data.frame(list_datasets[ss])
        row.names(set)<-NULL
        class<-set[,ncol(set)]
        class<-as.factor(as.character(class))
        set<-set[,-ncol(set)]
        row.names(set)<-NULL
        set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
        pnorm<-metric_opt_k_max$variable[ss]
        #CV<-ifelse(cut_CV>5,5,2)
        CV<-2

        perc<-.5; #perc<-seq(0,1.0,0.1)
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
        
        list_score_SMOTE_bordeline[ss,1]<-test_mean[which.max(test_mean)]*100
        list_score_SMOTE_bordeline[ss,2]<-test_sd[which.max(test_mean)]*100
        list_score_SMOTE_bordeline[ss,3]<-which.max(test_mean)
        print( list_score_SMOTE_bordeline)} #2fold CV
      
      for (s in 1:length(list_datasets)){
        set<-data.frame(list_datasets[s])
        row.names(set)<-NULL
        class<-set[,ncol(set)]
        class<-as.factor(as.character(class))
        set<-set[,-ncol(set)]
        row.names(set)<-NULL
        set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
        pnorm<-metric_opt_k_max$variable[ss]
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
        
        list_score_SMOTE_bordeline[s,c(4,5,6)]<-c(test_mean[which.max(test_mean)]*100,
                                                  test_sd[which.max(test_mean)]*100,
                                                  which.max(test_mean))
        print( list_score_SMOTE_bordeline)
      } #LOOCV
      
    } #smote
{    
    for (ss in 1:length(list_datasets)){
      set<-data.frame(list_datasets[ss])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
      pnorm<-metric_opt_k_max$variable[ss]      
      #CV<-ifelse(cut_CV>5,5,2)
      CV<-2
      perc<-.5; #perc<-seq(0,1.0,0.1)
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

            for (ii in 1:length(levels(class))){
            train<-c(lista[[ii]][,-j],train)
            test<-c(lista[[ii]][,j],test)}
          
          cltra<-class[train]; train<-set[train,]
          cltest<-class[test]; test<-set[test,]
          
          data<-SMOTE_border1(train,cltra,1,2,5)
          
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
      
      list_score_SMOTE_bordeline[ss,7]<-test_mean[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,8]<-test_sd[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,9]<-which.max(test_mean)
      print(list_score_SMOTE_bordeline)} #2fold CV
    
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    data<-SMOTE_border1(set,class,1,2,5)
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
    
    list_score_SMOTE_bordeline[s,c(10,11,12)]<-c(test_mean[which.max(test_mean)]*100,
                                              test_sd[which.max(test_mean)]*100,
                                              which.max(test_mean))
    print( list_score_SMOTE_bordeline)
  } #LOOCV      
} #borderline1
{
  for (ss in 1:length(list_datasets)){
      set<-data.frame(list_datasets[ss])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
      pnorm<-metric_opt_k_max$variable[ss]      
      #CV<-ifelse(cut_CV>5,5,2)
      CV<-2
      perc<-.5; #perc<-seq(0,1.0,0.1)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      
      perc<-.5; #perc<-seq(0,1.0,0.1)
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
          
          data<-SMOTE_border2(train,cltra,1,2,5)
          
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
      
      list_score_SMOTE_bordeline[ss,13]<-test_mean[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,14]<-test_sd[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,15]<-which.max(test_mean)
      print(list_score_SMOTE_bordeline)} #2fold CV
    
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    data<-SMOTE_border2(set,class,1,2,5)
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
    
    list_score_SMOTE_bordeline[s,c(16,17,18)]<-c(test_mean[which.max(test_mean)]*100,
                                              test_sd[which.max(test_mean)]*100,
                                              which.max(test_mean))
    print( list_score_SMOTE_bordeline)
  } #LOOCV
    } #borderline2
{   
  for (ss in 1:length(list_datasets)){
      set<-data.frame(list_datasets[ss])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
      pnorm<-metric_opt_k_max$variable[ss]      
      #CV<-ifelse(cut_CV>5,5,2)
      CV<-2
      perc<-.5; #perc<-seq(0,1.0,0.1)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      
      perc<-.5; #perc<-seq(0,1.0,0.1)

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
          
          data<-SMOTE_border3(train,cltra,1,2,5)
          
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
      
      list_score_SMOTE_bordeline[ss,19]<-test_mean[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,20]<-test_sd[which.max(test_mean)]*100
      list_score_SMOTE_bordeline[ss,21]<-which.max(test_mean)
      print(list_score_SMOTE_bordeline)} #2fold CV
    
  for (s in 1:length(list_datasets)){
    set<-data.frame(list_datasets[s])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL
    set<-select_normalization(set,as.character(metric_opt_k_max$normalization[ss]))
    pnorm<-metric_opt_k_max$variable[ss]
    data<-SMOTE_border2(set,class,1,2,5)
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
    
    list_score_SMOTE_bordeline[s,c(22,23,24)]<-c(test_mean[which.max(test_mean)]*100,
                                                 test_sd[which.max(test_mean)]*100,
                                                 which.max(test_mean))
    print( list_score_SMOTE_bordeline)
    } #LOOCV
    } #borderline3
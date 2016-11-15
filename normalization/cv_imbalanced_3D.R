library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)
library(Hmisc)
library(reshape)
ex3<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  CN<-ex3[which(ex3$DX=='NL'),]
  MCI<-ex3[which(ex3$DX=="MCI"),]
  AD<-ex3[which(ex3$DX=="Dementia"),]
  x<-rbind(CN,MCI,AD)[,2:3]
  class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  class<-as.factor(class)
  rownames(x)<-NULL
  list_data_norm<-list(data.frame(Min_max_nrmlztn(x),class=class),
                       data.frame(z_nrmlztn(x),class=class),
                       data.frame(max_nrmlztn(x),class=class),
                       data.frame(decimal_nrmlztn(x),class=class))
  norm_results<-as.data.frame(matrix(0,nrow=length(list_data_norm),ncol=floor(sqrt(nrow(set)))))
  norm_results_sd<-norm_results
  cv_cont<-0
  list_norm_results<-list(NULL)
  list_norm_results_sd<-list(NULL)
  cv_path<-c(2:30)
  for(cv_var in cv_path){
    for (ss in 1:length(list_data_norm)){
    {set<-data.frame(list_data_norm[ss])
    row.names(set)<-NULL
    class<-set[,ncol(set)]
    class<-as.factor(as.character(class))
    set<-set[,-ncol(set)]
    row.names(set)<-NULL} #dataissues
    
    {
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      matrix_list<-NULL
    } #initialization
    
    {#cut_CV<-(as.vector(summary(class)))[(which.min(as.vector(summary(class))))]
      #CV<-ifelse(cut_CV>=5,5,2)
      CV<-cv_var
      pnorm<-2
      
      
      lista<-NULL
      ii<-1;k<-1;s<-1
      classes_length<-1:length(class)
      
      for(i in levels(class)){
        cl_length<-(1:length(class))[class==i]
        lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
        ii<-ii+1}
    } #folded CV partitions  
    
    for (k in 1:ncol(norm_results)){
      for (j in c(1:CV)){
        
        train<-NULL;test<-NULL
        for (ii in 1:length(levels(class))){
          train<-c(lista[[ii]][,-j],train)
          test<-c(lista[[ii]][,j],test)}
        
        cltra<-class[train]; train<-set[train,]
        cltest<-class[test]; test<-set[test,]
        
        #data<-undersample(train,cltra,rnorm(1),1)
        #train<-data[[1]]
        #cltra<-data[[2]]
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
    for (i in 1:k){
      norm_results[ss,i]<-mean(t(tests[i,]))
      norm_results_sd[ss,i]<-sd(t(tests[i,]))
    }
  }
  cv_cont<-cv_cont+1
  list_norm_results[cv_cont]<-list(norm_results)
  list_norm_results_sd[cv_cont]<-list(norm_results_sd)
  View(list_norm_results[cv_cont]);View(list_norm_results_sd[cv_cont]);
  }
  
    big_melt<-data.frame()
    big_melt_sd<-data.frame()
    for (i in 1:cv_cont){
      norm_results<-list_norm_results[[i]]*100; norm_results_sd<-list_norm_results_sd[[i]]*100
      colnames(norm_results)<-1:ncol(norm_results);  colnames(norm_results_sd)<-1:ncol(norm_results_sd);
      rownames(norm_results)<-c("min-max","z-score","max","decimal"); rownames(norm_results_sd)<-c("min-max","z-score","max","decimal") 
      melt_imb<-melt(as.matrix(norm_results))
      melt_imb_sd<-melt(as.matrix(norm_results_sd))
      melt_imb[,1]<-as.factor(melt_imb[,1]); colnames(melt_imb)<-c("normalization","k","average precision")
      melt_imb$CV<-cv_path[i]
      
      melt_imb_sd[,1]<-as.factor(melt_imb_sd[,1]); colnames(melt_imb_sd)<-c("normalization","k","sd")
      melt_imb_sd$CV<-cv_path[i]
      
      big_melt_sd<-rbind(melt_imb_sd,big_melt_sd)
      big_melt<-rbind(melt_imb,big_melt)
    }

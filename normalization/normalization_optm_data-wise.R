list_datasets<-list(plasma,plasma_neuro_psy_converter,plasma_converter,BIO,PROTEOM,NEUROPS,FDGAV45,parkinsons)
#list_score_tie_breaking<-matrix(0,ncol = 3*3,nrow = 2*length(list_datasets))
elm <- matrix(NA,4,4); elm[,4]<-c("min-max","z-score","max","decimal")
norm_k_optim_ru<-rep(list(elm),8)

#create fuction to decide which method use based on list of highest score
#load k values

for (ss in 1:length(list_datasets)){
  set<-data.frame(list_datasets[ss])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  
  normalized_sets<-list(data.frame(Min_max_nrmlztn(set)),
                       data.frame(z_nrmlztn(set)),
                       data.frame(max_nrmlztn(set)),
                       data.frame(decimal_nrmlztn(set)))
  
  #krange
  
  for (t in 1:length(normalized_sets)){
    set<-normalized_sets[[t]]
    row.names(set)<-NULL
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
    
    norm_k_optim_ru[[ss]][t,1]<-test_mean[which.max(test_mean)]*100
    norm_k_optim_ru[[ss]][t,2]<-test_sd[which.max(test_mean)]*100
    norm_k_optim_ru[[ss]][t,3]<-which.max(test_mean)
    print(norm_k_optim_ru[[ss]])
  } #LOOCV
 }
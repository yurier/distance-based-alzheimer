knn_imputation<-function(data,class,k,relax){
  
  library(grid)
  library(lattice)
  library(modeltools)
  library(stats4)
  library(flexclust)
  
  classes<-levels(class)
  k<-5; datas<-data
  data_save_1<-data.frame()
  data_save_2<-data.frame()
  for (cl in classes){
  
  data<-datas[classes==cl,]
  #print(".....complete cases......");print(summary(complete.cases(data)));
  
  data_complete<-data[complete.cases(data),]
  data_notcomplete<-data[!complete.cases(data),]
  data_imputed<-data_notcomplete
  
  if(nrow(data_notcomplete)!=0){
    for (i in 1:nrow(data_notcomplete)){
    D<-(1:ncol(data_notcomplete))[(!is.na(data_notcomplete[i,]))]
    nD<-(1:ncol(data_notcomplete))[(is.na(data_notcomplete[i,]))]
    D_set<-data_complete[,D]
    dista<-dist2(D_set,data_notcomplete[i,D])
    nearest<-data_complete[order(dista),][1:min(k,nrow(data_complete)),]
    
    for (j in 1:length(nD)){
      data_imputed[i,nD[j]]<-mean(nearest[,nD[j]])
    }
  }}
  data_save_1<-rbind(data_save_1,data_complete)
  data_save_2<-rbind(data_save_2,data_imputed)
  }
  answer<-rbind(data_save_1,data_save_2)
  answer<-answer[as.character(1:nrow(datas)),]
  return(data.frame(answer))
}
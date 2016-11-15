KNN_weighted <- function(train,test,class,k){
  library(grid)
  library(lattice)
  library(modeltools)
  library(stats4)
  library(flexclust)
  
  
  class<-as.factor(class)
  prob<-NULL
  row.names(train) <- NULL
  row.names(test) <- NULL
  assign<-NULL
  dist_matrix_test<-dist2(x=train,y=test,method='euclidean')
  rownames(dist_matrix_test)<-1:nrow(dist_matrix_test)
  #dist_matrix<-as.matrix(dist(train, method = "euclidean", diag = TRUE, upper = TRUE))
  for(i in 1:ncol(dist_matrix_test)){
    #aux_closers<-as.numeric(rownames(as.data.frame(sort(dist_matrix[,i]))))[2:(k+1)]
    aux_closers<-as.numeric(rownames(as.data.frame(sort(dist_matrix_test[,i]))))[1:k]
    voting<-NULL
    voting<-data.frame(sort(summary(class[aux_closers]),decreasing = TRUE))
    dists<-dist_matrix_test[aux_closers,i]
    weighted<-(dists[k]-dists)/(dists[k]-dists[1])
    if(dists[1]==dists[k]){
      weighted[[1]]<-1
    }
    weighted<-data.frame(as.numeric(weighted),as.character(class[aux_closers]))
    for (j in levels(class[aux_closers])){
      voting[j,]<-sum(weighted[weighted[,2]==j,][,1])
    }
      voting$names<-(rownames(voting))
      assign[i]<-voting[which.max(voting[,1]),]$names
      prob[i]<-voting[which.max(voting[,1]),1]/sum(weighted[,1])
  }
 
  return(list(as.factor(assign),prob))
}



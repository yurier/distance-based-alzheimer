knn_plus1_break <- function(train,test,class,k,pnrm){
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
  dist_matrix_test<-dist2(x=train,y=test,method="minkowski",p=pnrm)
  rownames(dist_matrix_test)<-1:nrow(dist_matrix_test)
  #dist_matrix<-as.matrix(dist(train, method = "euclidean", diag = TRUE, upper = TRUE))
  if (!identical(train,test)){
    for(i in 1:ncol(dist_matrix_test)){
    #aux_closers<-as.numeric(rownames(as.data.frame(sort(dist_matrix[,i]))))[2:(k+1)]
    aux<-as.numeric(rownames(as.data.frame(sort(dist_matrix_test[,i]))))
    aux_closers<-aux[1:k]
    voting<-data.frame(sort(summary(class[aux_closers]),decreasing = TRUE))
    ties<-as.numeric(as.list(summary(voting[,1]==voting[1,1]))$'TRUE')
    if (is.null(ties)){
      ties<-0
    }
    if (ties<=1){
      assign[i]<-rownames(voting)[1]
      prob[i]<-voting[1,1]/k}
    a<-1
    while (ties>1){
      aux_closers<-aux[1:(k+a)]
      voting<-data.frame(sort(summary(class[aux_closers]),decreasing = TRUE))
      ties<-as.numeric(as.list(summary(voting[,1]==voting[1,1]))$'TRUE')
      if (is.null(ties)){
        ties<-0
      }
      assign[i]<-sample(rownames(voting)[1:ties],1)
      prob[i]<-voting[1,1]/(k+a)
      a<-a+1
    }}}
  if (identical(train,test)){
    for(i in 1:ncol(dist_matrix_test)){
      #aux_closers<-as.numeric(rownames(as.data.frame(sort(dist_matrix[,i]))))[2:(k+1)]
      aux<-as.numeric(rownames(as.data.frame(sort(dist_matrix_test[,i]))))
      aux_closers<-aux[2:(k+1)]
      voting<-data.frame(sort(summary(class[aux_closers]),decreasing = TRUE))
      ties<-as.numeric(as.list(summary(voting[,1]==voting[1,1]))$'TRUE')
      if (is.null(ties)){
        ties<-0
      }
      if (ties<=1){
        assign[i]<-rownames(voting)[1]
        prob[i]<-voting[1,1]/k}
      a<-1
      while (ties>1){
        aux_closers<-aux[1:(k+a)]
        voting<-data.frame(sort(summary(class[aux_closers]),decreasing = TRUE))
        ties<-as.numeric(as.list(summary(voting[,1]==voting[1,1]))$'TRUE')
        if (is.null(ties)){
          ties<-0
        }
        assign[i]<-sample(rownames(voting)[1:ties],1)
        prob[i]<-voting[1,1]/(k+a)
        a<-a+1
      }}}
  
  
  return(list(as.factor(assign),prob))
}

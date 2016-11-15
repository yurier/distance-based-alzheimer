nearmiss2 <- function(data,class,degree,pnorm){
  library(grid)
  library(lattice)
  library(modeltools)
  library(stats4)
  library(flexclust)
  data<-data.frame(data)
  rownames(data)<-NULL
  list_class_order<-NULL
  
  class<-as.factor(as.character(class))
  minor_class_number<-length(class[class==levels(class)[1]])
  for (i in levels(class)){
    if (length(class[class==i])<=minor_class_number){
      minor_class_number<-length(class[class==i])
      minor_class_name<-i
    } }
  
  #list_class_order[which(levels(class)==i)]<-
  
  dataset<-data.frame(NULL)
  dataset_class<-NULL
  test_add<-NULL
  test_class<-NULL
  
  
  
  for (j in levels(class)[!c(levels(class) %in% minor_class_name)]){
    dist_matrix<-dist2(x=data[class==minor_class_name,],y=data[class==j,],method="minkowski",p=pnrm)
    dist_matrix<-as.data.frame(dist_matrix)
    colnames(dist_matrix)<-(1:nrow(data))[class==j]
    rownames(dist_matrix)<-(1:nrow(data))[class==minor_class_name]
    aux_selecta<-NULL
    selecta<-NULL
    degree_number<-ceiling((1-degree)*length(class[class==j])+degree*minor_class_number)
    
    
    if (all(nrow(dist_matrix)<2,ncol(dist_matrix)>2,degree_number!=0)){
      dist_matrix<-dist_matrix[,!(colnames(dist_matrix) %in% selecta)]
      selecta<-dist_matrix[order(dist_matrix,decreasing = FALSE)]
      selecta<-unique(names(selecta))
      selecta<-selecta[1:(degree_number-length(aux_selecta))]
      selecta<-selecta[(!is.na(selecta))]
      aux_selecta<-c(selecta,aux_selecta)
      selecta<-aux_selecta
      dist_matrix<-as.data.frame(dist_matrix)} else {selecta<-(1:nrow(data))[class==j]}
    
    if (ncol(dist_matrix)>=2){
      selecta<-NULL
      if (all(degree!=0,degree_number!=0)){
        while (length(aux_selecta)<degree_number){
          dist_matrix<-dist_matrix[,!(colnames(dist_matrix) %in% selecta)]
          aux_matrix<-NULL  #fazer ciclo para completer os dados
          for (s in colnames(dist_matrix)){
            aux<-dist_matrix[order(dist_matrix[,s],decreasing = TRUE)[1:ifelse(nrow(dist_matrix)<3,nrow(dist_matrix),3)],s]
            aux_matrix<-cbind(aux_matrix,c(as.numeric(s),mean(aux)))}
          colnames(aux_matrix)<-as.character(aux_matrix[1,])
          selecta<-as.vector(aux_matrix[1,as.vector(order(aux_matrix[2,],decreasing = FALSE))])
          selecta<-unique(selecta)
          selecta<-selecta[1:(degree_number-length(aux_selecta))]
          selecta<-selecta[(!is.na(selecta))]
          aux_selecta<-c(selecta,aux_selecta)}
        selecta<-aux_selecta}
      else {selecta<-(1:nrow(data))[class==j]}}
    
    unval<-as.numeric(rownames(data[class==j,]))
    unval<-unval[!(unval %in% selecta)]
    test_add<-rbind(data[row.names(data) %in% unval,],test_add)
    test_class<-c(rep(j,nrow(data[row.names(data) %in% unval,])),test_class)
    dataset<-rbind(data[row.names(data) %in% selecta,],dataset)
    dataset_class<-c(rep(j,degree_number),dataset_class)
  }  
  dataset<-rbind(data[row.names(data) %in% (1:nrow(data))[class==minor_class_name],],dataset)
  dataset_class<-c(rep(minor_class_name,minor_class_number),dataset_class)
  
  test_class<-as.factor(as.character(test_class))
  dataset_class<-as.factor(as.character(dataset_class))
  
  return(list(dataset,dataset_class,test_add,test_class))
  
}
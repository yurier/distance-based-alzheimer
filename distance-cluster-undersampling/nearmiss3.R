nearmiss3 <- function(data,class,degree,pnorm){
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
    dist_matrix<-dist2(x=data[class==j,],y=data[class==minor_class_name,],method="minkowski",p=pnrm)
    rownames(dist_matrix)<-(1:nrow(data))[class==j]
    colnames(dist_matrix)<-(1:nrow(data))[class==minor_class_name]
    aux_selecta<-NULL
    selecta<-NULL
    degree_number<-floor((1-degree)*length(class[class==j])+degree*minor_class_number)
    degree_number<-length(class[class==j])-degree_number
    
    
    if (all(ncol(dist_matrix)<2,degree_number!=0)){
      dist_matrix<-dist_matrix[!(rownames(dist_matrix) %in% selecta),]
      selecta<-dist_matrix[order(dist_matrix,decreasing = FALSE)]
      selecta<-unique(names(selecta))
      selecta<-selecta[1:(degree_number-length(aux_selecta))]
      selecta<-selecta[(!is.na(selecta))]
      aux_selecta<-c(selecta,aux_selecta)
      selecta<-aux_selecta} else {selecta<-NULL}
    
    if (!is.vector(dist_matrix)){
    if (all(degree!=0,degree_number!=0)){
      while (length(aux_selecta)<degree_number){
        dist_matrix<-dist_matrix[!(row.names(dist_matrix) %in% selecta),]
        aux_matrix<-NULL
        for (s in colnames(dist_matrix)){
          aux<-dist_matrix[order(dist_matrix[,s],decreasing = FALSE)[1:2],s]
          aux_matrix<-cbind(aux_matrix,c(as.numeric(names(aux[1])),mean(aux[1])))}
        selecta<-as.vector(aux_matrix[1,as.vector(order(aux_matrix[nrow(aux_matrix),],decreasing = FALSE))])
        selecta<-unique(selecta)
        selecta<-selecta[1:(degree_number-length(aux_selecta))]
        selecta<-selecta[(!is.na(selecta))]
        aux_selecta<-c(selecta,aux_selecta)}
      selecta<-aux_selecta}}
    
    unval<-as.numeric(rownames(data[class==j,]))
    unval<-unval[!(unval %in% selecta)]
    test_add<-rbind(data[row.names(data) %in% selecta,],test_add)
    test_class<-c(rep(j,nrow(data[row.names(data) %in% selecta,])),test_class)
    dataset<-rbind(data[row.names(data) %in% unval,],dataset)
    dataset_class<-c(rep(j,length(unval)),dataset_class)
    }
  
  dataset<-rbind(data[row.names(data) %in% (1:nrow(data))[class==minor_class_name],],dataset)
  dataset_class<-c(rep(minor_class_name,minor_class_number),dataset_class)
  
  test_class<-as.factor(as.character(test_class))
  dataset_class<-as.factor(as.character(dataset_class))
  
  return(list(dataset,dataset_class,test_add,test_class))
  
}
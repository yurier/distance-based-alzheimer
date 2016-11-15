nearmiss <- function(t_minor,class_minor,t_major,class_major,N,k){
  
  min_cl<-as.character(class_minor[1])
  maj_cl<-as.character(class_major[1])
  rownames(t_minor)<-1:nrow(t_minor)
  data<-rbind(t_minor,t_major)
  rownames(data)<-1:nrow(data)
  classes<-as.factor(c(as.character(class_minor),as.character(class_major)))
  library(stats4)
  library(grid)
  library(lattice)
  library(modeltools)
  library(flexclust)
  
  synth<-data.frame()
  dista<-dist2(x=t_minor,y=t_major,method="minkowski",p=2)
  max<-matrix(0,ncol(dista),1)
  
  for (i in 1:ncol(dista)){
  max[i]<-mean(dista[,i][order(dista[,i])[(nrow(dista)-k+1):nrow(dista)]])}
  
  t_major<-t_major[order(max)[1:N],]
  
  return(t_major) 
}
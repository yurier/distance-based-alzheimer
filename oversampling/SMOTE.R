SMOTE <- function(data,class,degree,pnorm,k){


library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)

row.names(data)<-(1:nrow(data))
class<-as.factor(as.character(class))
major_class_number<-length(class[class==levels(class)[1]])
  for (i in levels(class)){
    if (length(class[class==i])>=major_class_number){
      major_class_number<-length(class[class==i])
      major_class_name<-i
}}


synth_aux<-NULL
synth_class<-NULL

for (j in levels(class)[!c(levels(class) %in% major_class_name)]){

degree_number<-ceiling((1-degree)*length(class[class==j])+degree*major_class_number)  
dista<-dist2(x=data[class==j,],y=data[class==j,],method="minkowski",p=pnrm)
dista<-as.data.frame(dista)
colnames(dista)<-(1:nrow(data))[class==j]
rownames(dista)<-(1:nrow(data))[class==j]

index<-sample(colnames(dista), size =degree_number-nrow(data[class==j,]),replace=TRUE) 

if(length(index)>=1){
  for (i in index){
  knear<-data[(rownames(dista))[(order(dista[,as.character(i)]))[1:(min(k,nrow(dista)))]],]
  rand<-runif(1,0,1)
  i<-as.numeric(i)
  knear<-knear[sample(seq_len(min(k,nrow(dista))),1),]
  aux<-(1-rand)*data[i,]+rand*knear
  row.names(aux)<-paste0('_',as.character(i),'_',row.names(knear),'_')
  if (all(is.na(aux))){aux<-data[i,]}
  synth_aux<-rbind(aux,synth_aux)}
  
synth_class<-c(rep(j,length(index)),synth_class)
}
}

synth_class<-as.character(synth_class)
  return(list(synth_aux,synth_class))
}
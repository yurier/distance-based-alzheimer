SMOTE_border3 <- function(data,class,degree,pnorm,k){
  
  library(stats4)
  library(grid)
  library(lattice)
  library(modeltools)
  library(flexclust)
  row.names(data)<-1:nrow(data)
  class<-as.factor(as.character(class))
  major_class_number<-length(class[class==levels(class)[1]])
  for (i in levels(class)){
    if (length(class[class==i])>=major_class_number){
      major_class_number<-length(class[class==i])
      major_class_name<-i
    }}
  
  synth<-data.frame();synth_class<-NULL;
  for (j in levels(class)[!c(levels(class) %in% major_class_name)]){
    if(nrow(data[class==j,])!=nrow(data[class==major_class_name,])){
    {
    synth_aux<-data.frame(); 
    degree_number<-ceiling((1-degree)*length(class[class==j])+degree*major_class_number)  
    dista<-dist2(x=data,y=data[class==j,],method="minkowski",p=pnrm)
    dista<-as.data.frame(dista)
    colnames(dista)<-(1:nrow(data))[class==j]
    rownames(dista)<-(1:nrow(data))
    
    index<-(1:nrow(data))[class==j]
    
    danger_count<-1;danger<-NULL
    safe_count<-1;safe<-NULL
    noise_count<-1;noise<-NULL}
    for (i in index){
      knear<-data[order(dista[,as.character(i)]),][2:(k+1),]
      class[as.numeric(rownames(knear))]
      region<-data.frame(t(summary(class[as.numeric(rownames(knear))])))
      if(any(names(region)==major_class_name)){
        region<-subset(region,select=major_class_name)} else {region<-0}
      if ((0<=region)&(region<k/2)){ #SAFE dataset
        safe[safe_count]<-as.numeric(i)
        safe_count<-safe_count+1
      }
      if ((k/2<=region)&(region<k)){ #DANGER dataset
        danger[danger_count]<-as.numeric(i)
        danger_count<-danger_count+1
      }
      if ((region==k)){ #DANGER dataset
        noise[noise_count]<-as.numeric(i)
        noise_count<-noise_count+1
      }
    }
    #plot(data,col="white")
      if (all(!is.null(danger),nrow(data[class==j,])!=1)){{
      danger_dista<-dist2(x=data,y=data[as.character(danger),],method="minkowski",p=pnrm)
      danger_dista<-as.data.frame(danger_dista)
      colnames(danger_dista)<-as.character(danger)
      rownames(danger_dista)<-rownames(data)
      index<-as.character(danger)
      aux_s<-dist2((1:k)*length(index),degree_number-nrow(data[class==j,]))
      s<-which.min(aux_s)
      balance<-degree_number-nrow(data[class==j,])}
    
    while (nrow(synth_aux)<=balance){
      for (i in index){
        knear<-data[(rownames(danger_dista))[(order(danger_dista[,as.character(i)]))[2:(1+(min(s,nrow(danger_dista))))]],]
        rknear<-knear[complete.cases(knear),]
        #rand<-runif(s,0,1)
        rand<-runif(min(s,nrow(danger_dista)-1),0,1)
        i<-as.numeric(i)
        #aux<-(1-rand)*data[i,][rep(1,s),]+rand*knear
        aux<-(1-rand)*data[i,][rep(1,min(s,nrow(danger_dista)-1)),]+rand*knear
        row.names(aux)<-paste0('_',as.character(i),'_',row.names(knear),'_')
        if (all(is.na(aux))){aux<-data[i,]}
        synth_aux<-rbind(aux,synth_aux)}}
    
    synth_aux<-synth_aux[sample(1:nrow(synth_aux),size=balance),]
    synth<-rbind(synth_aux,synth)
    #index_aux<-sample(1:nrow(synth_aux), size =balance,replace=TRUE)
    #synth<-rbind(synth,synth_aux[index_aux,])
    
    synth_class<-c(rep(j,nrow(synth_aux)),synth_class)
    #points(data[safe,], cex = .5, col = "blue")
    #points(data[noise,], cex = .5, col = "black")
    #points(data[danger,],cex = .5, col='red')
    #points(synth_aux,cex=.5,col="green")
    }
  }}
  
  synth_class<-as.character(synth_class)
  
  return(list(synth,synth_class)) 
}
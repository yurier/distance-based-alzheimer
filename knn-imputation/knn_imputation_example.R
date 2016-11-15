
library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)
mean_error<-NULL;s<-0
for(ij in seq(.1,.50,.01)){
data<-BreastTissue[,-10]
class<-BreastTissue[,10]


data_copy<-Min_max_nrmlztn(data)

for (i in levels(class)){
  dat<-data[class==i,]
  nasnum<-floor(nrow(dat)*ij)
  lines<-sample(1:nrow(dat),nasnum)
  for(j in lines){
    randlines<-sample(1:ncol(dat),2)
    dat[j,randlines]<-dat[j,randlines]*NA    
  }
  data[class==i,]<-dat
}


for (i in levels(class)){
    dat<-data[class==i,]
    nrow(dat)
    print(summary(complete.cases(dat)))}

data<-Min_max_nrmlztn(data)

#print(summary(complete.cases(data)))

datas<-knn_imputation(data = data,class = class,k = 5,relax = 1)

imputed_data<-datas[!complete.cases(data),]
true_data<-data_copy[!complete.cases(data),]

error<-dist2(x=true_data,y=imputed_data)
error_aux<-NULL

for (i in 1:nrow(imputed_data)){
  error_aux[i]<-error[i,i]
}
s<-s+1
mean_error[s]<-mean(error_aux)

}
decimal_nrmlztn <- function(data){
  
  expp<-1
  for (i in 1:ncol(data)){
    aux<-as.matrix(data[,i])
    while (abs(aux[which.max(aux)])>=1){
      aux<-as.matrix(data[,i]/(10^expp))
      expp<-expp+1
      }
    data[,i]<-data[,i]/(10^(expp-1))
    expp<-1}
  return(data)
}
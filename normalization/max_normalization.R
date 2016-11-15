max_nrmlztn <- function(data){
  for (i in 1:ncol(data)){
    maxi<-max(data[,i])
    data[,i]=(data[,i])/(maxi)
  }
  return(data)
}
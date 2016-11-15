z_nrmlztn <- function(data){
  for (i in 1:ncol(data)){
    ave<-mean(data[,i])
    std<-sd(data[,i])
    data[,i]=(data[,i]-ave)/(std)
  }
  return(data)
}
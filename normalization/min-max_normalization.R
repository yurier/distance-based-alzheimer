Min_max_nrmlztn <- function(data){
  for (i in 1:ncol(data)){
    maxi<-max(data[,i],na.rm = TRUE)
    mini<-min(data[,i],na.rm = TRUE)
    data[,i]=(data[,i]-mini)/(maxi-mini)
  }

return(data)
  }
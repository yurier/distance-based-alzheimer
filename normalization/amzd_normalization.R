amzd_nrmlztn <- function(data){
  for (j in 1:ncol(data)){
  for (i in 1:nrow(data)){
  data[i,j]<-(abs(data[i,j])-10^(-1+floor(log10(data[i,j]))+1)*as.numeric(strsplit(as.character(data[i,j]),'')[[1]][1]))/10^(-1+floor(log10(data[i,j]))+1)
}}  
  return(data)}
matrix_distances <- function(data){
matrix_dist<-matrix(,nrow=nrow(data),ncol=nrow(data))
for (i in seq(1,nrow(data))){
  for (j in seq(i,nrow(data))){
    matrix_dist[i,j]<-as.numeric(dist(rbind(data[i,],data[j,])))
  }
}
return(matrix_dist)
}
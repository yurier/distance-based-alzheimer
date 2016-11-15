select_normalization<-function(set,name_method){
  if (name_method=="min-max"){set<-data.frame(Min_max_nrmlztn(set))}
  if (name_method=="max"){set<-data.frame(max_nrmlztn(set))}  
  if (name_method=="z-score"){set<-data.frame(z_nrmlztn(set))}
  if (name_method=="decimal"){set<-data.frame(decimal_nrmlztn(set))}
  return(set)
}
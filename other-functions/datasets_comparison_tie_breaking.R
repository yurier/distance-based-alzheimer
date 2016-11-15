list_datasets<-list(BreastTissue,bezdekIris,Data_Cortex_Nuclear,parkinsons,ecoli,yeast,Vietnan,PLASMA,BIO)
list_score<-matrix(0,ncol = 3,nrow = length(list_datasets))

for (s in 1:length(list_datasets)){
  set<-data.frame(list_datasets[s])
  row.names(set)<-NULL
  class<-set[,ncol(set)]
  class<-as.factor(as.character(class))
  set<-set[,-ncol(set)]
  row.names(set)<-NULL
  set<-Min_max_nrmlztn(set)
  
  pnorm<-2
  
  hit<-NULL
  mean_hit<-0
  hits<-NULL
  for (K in 1:floor(sqrt(nrow(set)))){
    for (jj in 1:1){
    for (j in 1:nrow(set)){

    train<-set[-j,]
    cltra<-class[-j]
    
    test<-set[j,]
    cltes<-class[j]
    
    ans1 <- knn_minus1_break(train,test,cltra,K,2)
    
    modtest<-as.character(ans1[[1]][1])
    cltes<-as.character(cltes[[1]])
    
    hit<-c(hit,as.data.frame(t(summary(as.integer(modtest==cltes))))[4,3])}
    hits<-c(mean(hit),hits)
    hit<-NULL}
    
    if ( (mean(hits)) > mean_hit){
        mean_hit<-mean(hits)
        sd_hit<-sd(hits)
        k_opt<-K
        high<-mean_hit}
    hits<-NULL
    print(K)}
  mean_hit<-0
  
  list_score[s,]<-c(high,sd_hit,k_opt)
  print(list_score)
}



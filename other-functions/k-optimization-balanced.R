library(corrplot)
library(ggplot2)
plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
dement<-PLASMA[PLASMA$DX=='Dementia',]
mci<-PLASMA[PLASMA$DX=='MCI',]
normal<-PLASMA[PLASMA$DX=='NL',]
dement$DX<-"AD"
mci$DX<-"MCI"
normal$DX<-"CN"
PLASMA<-rbind(dement,mci,normal)
row.names(PLASMA)<-1:nrow(PLASMA)
PLASMA$DX<-as.factor(as.character(PLASMA$DX))
dement<-PLASMA[PLASMA$DX=='AD',]
mci<-PLASMA[PLASMA$DX=='MCI',]
normal<-PLASMA[PLASMA$DX=='CN',]
PLASMA<-subset(PLASMA, select=c("DX","ABETA","PTAU"))

CV<-10
pnorm<-2

perc<-seq(1,60,1)
conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
trains<-as.data.frame(c(1:length(perc)))
tests<-as.data.frame(c(1:length(perc)))

k<-1

indexN<-matrix(0,ncol=CV,nrow=floor(nrow(normal)/CV)); normal_list<-as.numeric(rownames(normal))
indexD<-matrix(0,ncol=CV,nrow=floor(nrow(dement)/CV)); dement_list<-as.numeric(rownames(dement)) 
indexM<-matrix(0,ncol=CV,nrow=floor(nrow(mci)/CV)); mci_list<-as.numeric(rownames(mci))

indexN[,1]<-sample(normal_list, size = floor(nrow(normal)/CV))
indexD[,1]<-sample(dement_list, size = floor(nrow(dement)/CV))
indexM[,1]<-sample(mci_list, size = floor(nrow(mci)/CV))

for (i in 1:(CV-1)){
  indexN[,i+1]<-sample(normal_list[!normal_list %in% indexN[,1:i]], size = floor(nrow(normal)/CV))
  indexD[,i+1]<-sample(dement_list[!dement_list%in%indexD[,1:i]], size = floor(nrow(dement)/CV))
  indexM[,i+1]<-sample(mci_list[!mci_list%in%indexM[,1:i]], size = floor(nrow(mci)/CV))
}

for (i in perc){
  for (j in c(1:CV)){
    plasma<-PLASMA
    
    Dtest <-plasma[indexD[,j], ]
    classD <- plasma[indexD[,-j], ]
    Mtest <-plasma[indexM[,j], ]
    classM <- plasma[indexM[,-j], ]
    Ntest <-plasma[indexN[,j], ]
    classN <- plasma[indexN[,-j], ]
    
    test<-rbind(Dtest,Mtest,Ntest)
    cltes<-test[,1]
    test<-test[,2:(ncol(test))]
    
    if (i != 0){
      classD<-rbind(classD,data.frame(DX='AD',SMOTE(classD[,-1],floor((nrow(classM)-nrow(classD))),5)))
      classN<-rbind(classN,data.frame(DX='CN',SMOTE(classN[,-1],floor((nrow(classM)-nrow(classN))),5)))
    }

    train<-rbind(classD,classM,classN)
    cltra<-train[,1]
    train<-train[,2:(ncol(train))]
    
    test<-rbind(Dtest,Mtest,Ntest)
    cltes<-test[,1]
    test<-test[,2:(ncol(test))]
    
    ans1 <- KNN_random_break(train,test,cltra,i,pnorm)
    modtest<-ans1[[1]]
    
    ans <- KNN_random_break(train,train,cltra,i,pnorm)
    modtrain<-ans[[1]]
    
    modtrain<-as.character(modtrain)
    cltra<-as.character(cltra)
    modtest<-as.character(modtest)
    cltes<-as.character(cltes)
    
    datag<-data.frame(cbind(as.vector(modtest),as.vector(cltes)))
    colnames(datag)<-c('predict','correct')
    conf <- data.frame()
    for (ii in c("AD","MCI","CN")){
      for(jj in c("AD","MCI","CN")){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    matrix_aux<-matrix_aux+conf
    tests[k,j]<-(conf[1,1]+conf[2,2]+conf[3,3])/3
    
    datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra)))
    colnames(datag)<-c('predict','correct')
    conf1 <- data.frame()
    for (ii in c("AD","MCI","CN")){
      for(jj in c("AD","MCI","CN")){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    
    trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3
    
  }
  print(nrow(classN))
  print(nrow(classM))
  print(nrow(classD))
  print(nrow(test))
  print(i)
  k<-1+k
  matrix_aux<-matrix_aux/CV
  cairo_ps(width=5,height = 5 ,file=sprintf('conf_%gkbal.eps',i))
  corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
  dev.off()
  conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
}

test_mean<-NULL
train_mean<-NULL
test_sd<-NULL
train_sd<-NULL

for (i in c(1:length(perc))){
  test_mean[i]<-mean(t(tests[i,1:CV]))
  test_sd[i]<-sd(t(tests[i,1:CV]))
  train_mean[i]<-mean(t(trains[i,1:CV]))
  train_sd[i]<-sd(t(trains[i,1:CV]))
}

dataf<-data.frame(reduction=perc,test_mean,test_sd,train_mean,train_sd,train=factor('train'),test=factor('test'))
cairo_ps(width=6,height = 3.5 ,file='k-optimum-balanced.eps')
ggplot()+theme_bw()+
  geom_line(data=dataf,aes(x=reduction, y=test_mean,colour=test),size=1)+
  geom_line(data=dataf,aes(x=reduction, y=train_mean,colour=train),size=1)+
  geom_errorbar(data=dataf,aes(colour=test,x=reduction,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
  geom_errorbar(data=dataf,aes(colour=train,x=reduction,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
  scale_color_manual(values=c("#999999","#E69F00"),name="set")+
  labs(x='k',y='Correct classified average')
dev.off()



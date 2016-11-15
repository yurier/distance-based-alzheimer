library(corrplot)
library(ggplot2)
plasma<-df_MERGE_PLASMA_complete<-df_MERGE_PLASMA_complete[complete.cases(df_MERGE_PLASMA_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
dement<-PLASMA[PLASMA$DX=='Dementia',]
mci<-PLASMA[PLASMA$DX=='MCI',]
normal<-PLASMA[PLASMA$DX=='NL',]
dement$DX<-"AD"
mci$DX<-"MCI"
normal$DX<-"CN"
PLASMA<-rbind(dement,mci,normal)
PLASMA$DX<-as.factor(as.character(PLASMA$DX))
PLASMA<-subset(PLASMA, select=c("DX","CDRSB","AB42"))

CV<-10
pnorm<-2

perc<-seq(0,1,0.1)

trains<-as.data.frame(c(1:length(perc)))
tests<-as.data.frame(c(1:length(perc)))
set.seed(12)
k<-1
for (i in perc){
  for (j in c(1:CV)){
    plasma<-PLASMA
    dement<-plasma[plasma$DX=='AD',]
    mci<-plasma[plasma$DX=='MCI',]
    normal<-plasma[plasma$DX=='CN',]
    indexD <- sample(seq_len(nrow(dement)), size = floor(nrow(dement)*0.80))
    indexM <- sample(seq_len(nrow(mci)), size = floor(nrow(mci)*0.80))
    indexN <- sample(seq_len(nrow(normal)), size = floor(nrow(normal)*0.80))
    Dtest <-dement[-indexD, ]
    dement <- dement[indexD, ]
    Mtest <-mci[-indexM, ]
    mci <- mci[indexM, ]
    Ntest <-normal[-indexN, ]
    normal <- normal[indexN, ]
    
    test<-rbind(Dtest,Mtest,Ntest)
    cltes<-test[,1]
    test<-test[,2:(ncol(test))]
    
    #indexM<-sample(seq_len(nrow(mci)), size = floor(nrow(mci)*i))
    indexM<-sample(seq_len(nrow(mci)), size = floor((1-i)*(nrow(mci))+(i)*(nrow(dement))))
    indexN<-sample(seq_len(nrow(normal)), size = floor((1-i)*(nrow(normal))+(i)*(nrow(dement))))
    #dement<- dement[indexD,]
    mci<- mci[indexM,]
    normal<- normal[indexN,]
    
    train<-rbind(dement,mci,normal)
    cltra<-train[,1]
    train<-train[,2:(ncol(train))]
    
    ans1 <- KNN_random_break(train,test,cltra,5,pnorm)
    modtest<-ans1[[1]]
    
    ans <- KNN_random_break(train,train,cltra,5,pnorm)
    modtrain<-ans[[1]]
    
    modtrain<-as.character(modtrain)
    cltra<-as.character(cltra)
    modtest<-as.character(modtest)
    cltes<-as.character(cltes)
    
    train_rate<-as.data.frame(t(sort(summary(modtrain==cltra))))
    trains[k,j]<-as.numeric(as.vector.factor((train_rate[train_rate[,2]==TRUE,][,3])
    ))/nrow(train)
    
    test_rate<-as.data.frame(t(sort(summary(modtest==cltes))))
    tests[k,j]<-as.numeric(as.vector.factor((test_rate[test_rate[,2]==TRUE,][,3])
    ))/nrow(test)
    print(i)
  }
  k<-1+k
  datag<-data.frame(cbind(as.vector(modtest),as.vector(cltes)))
  colnames(datag)<-c('predict','correct')
  conf <- data.frame()
  for (ii in c("AD","MCI","CN")){
    for(jj in c("AD","MCI","CN")){
      aux1<-datag[datag$correct==ii,]
      aux2<-aux1[aux1$predict==jj,]
      conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
  
  cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
  corrplot(as.matrix(conf),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
  dev.off()
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
cairo_ps(width=6,height = 3.5 ,file='undersamplingexample.eps')
ggplot()+theme_bw()+
  geom_line(data=dataf,aes(x=reduction, y=test_mean,colour=test),size=1)+
  geom_line(data=dataf,aes(x=reduction, y=train_mean,colour=train),size=1)+
  geom_errorbar(data=dataf,aes(colour=test,x=reduction,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
  geom_errorbar(data=dataf,aes(colour=train,x=reduction,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
  scale_color_manual(values=c("#999999","#E69F00"),name="set")+scale_x_continuous(labels=scales::percent)+
  labs(x='Undersampling percentage',y='Correct classified average')
dev.off()



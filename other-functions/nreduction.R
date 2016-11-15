require(ggplot2)
plasma<-df_MERGE_PLASMA_complete[complete.cases(df_MERGE_PLASMA_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
dement<-PLASMA[PLASMA$DX=='Dementia',]
mci<-PLASMA[PLASMA$DX=='MCI',]
normal<-PLASMA[PLASMA$DX=='NL',]

PLASMA<-rbind(dement,mci,normal)
PLASMA$DX<-as.factor(as.character(PLASMA$DX))
PLASMA<-subset(PLASMA, select=c("DX","AB40","AB42"))

CV<-30
pnorm<-2

perc<-seq(0.1,1,0.1)

trains<-as.data.frame(c(1:length(perc)))
tests<-as.data.frame(c(1:length(perc)))

set.seed(1234)
k<-1
for (i in perc){
  for (j in c(1:CV)){
    
    plasma<-PLASMA
    
    dement<-plasma[plasma$DX=='Dementia',]
    mci<-plasma[plasma$DX=='MCI',]
    normal<-plasma[plasma$DX=='NL',]
    
    indexD<-sample(seq_len(nrow(dement)), size = floor(nrow(dement)*i))
    indexM<-sample(seq_len(nrow(mci)), size = floor(nrow(mci)*i))
    indexN<-sample(seq_len(nrow(normal)), size = floor(nrow(normal)*i))

    dement<- dement[indexD,]
    mci<- mci[indexM,]
    normal<- normal[indexN,]
    
    indexD <- sample(seq_len(nrow(dement)), size = floor(nrow(dement)*0.5))
    indexM <- sample(seq_len(nrow(mci)), size = floor(nrow(mci)*0.5))
    indexN <- sample(seq_len(nrow(normal)), size = floor(nrow(normal)*0.5))
    
    Dtest <-dement[-indexD, ]
    classD <- dement[indexD, ]
    
    Mtest <-mci[-indexM, ]
    classM <- mci[indexM, ]
    
    Ntest <-normal[-indexN, ]
    classN <- normal[indexN, ]
    
    train<-rbind(classD,classM,classN)
    cltra<-train[,1]
    train<-train[,2:(ncol(train))]
    
    test<-rbind(Dtest,Mtest,Ntest)
    cltes<-test[,1]
    test<-test[,2:(ncol(test))]
    
    ans1 <- KNN_random_break(train,test,cltra,5,pnorm)
    modtest<-ans1[[1]]
    
    ans <- KNN_random_break(train,train,cltra,5,pnorm)
    modtrain<-ans[[1]]
    
    modtrain<-as.character(modtrain)
    cltra<-as.character(cltra)
    modtest<-as.character(modtest)
    cltes<-as.character(cltes)
    
    train_rate<-as.data.frame(t(sort(summary(modtrain==cltra))))
    trains[k,j]<-as.numeric(as.vector.factor((train_rate[train_rate[,2]==FALSE,][,3])
    ))/nrow(train)
    
    test_rate<-as.data.frame(t(sort(summary(modtest==cltes))))
    tests[k,j]<-as.numeric(as.vector.factor((test_rate[test_rate[,2]==FALSE,][,3])
    ))/nrow(test)
    print(i)
  }
  k<-1+k
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
cairo_ps(width=6,height = 3.5 ,file='ndependence.eps')
ggplot()+theme_bw()+
  geom_line(data=dataf,aes(x=reduction, y=test_mean,colour=test),size=1)+
  geom_line(data=dataf,aes(x=reduction, y=train_mean,colour=train),size=1)+
  geom_errorbar(data=dataf,aes(colour=test,x=reduction,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
  geom_errorbar(data=dataf,aes(colour=train,x=reduction,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
  scale_color_manual(values=c("#999999","#E69F00"),name="set")+scale_x_continuous(labels=scales::percent)+
  labs(x='Data percentage',y='Classification error')
dev.off()



library(MASS)
library(mvtnorm)
library(ggplot2)
library(class)
library(stats4)
library(scales)

diseas<-parkinsons[parkinsons$status==1,]
healthy<-parkinsons[parkinsons$status==0,]
CV<-30
diseas<-diseas[ , -which(names(diseas) %in% c("name","status"))]
healthy<-healthy[ , -which(names(healthy) %in% c("name","status"))]
dime<-ncol(healthy)
test<-NULL
train<-NULL
disea<-data.frame(diseas[,1:dime],class=factor('parkinson'))
health<-data.frame(healthy[,1:dime],class=factor('healthy'))
k<-1
normes<-seq(0.1,3,0.2)
trains<-as.data.frame(c(1:CV))
tests<-as.data.frame(c(1:CV))

for (pnrm in normes){
  for (j in (1:CV)){
    
    indexD <- sample(seq_len(nrow(diseas)), size = floor(nrow(diseas)*0.8))
    indexH <- sample(seq_len(nrow(healthy)), size = floor(nrow(healthy)*0.8))
    
    Dtest <-disea[-indexD, ]
    classD <- disea[indexD, ]
    
    Htest <-health[-indexH, ]
    classH <- health[indexH, ]
    
    train<-rbind(classH,classD)
    cl<-train[,ncol(train)]
    train<-train[,1:(ncol(train)-1)]
    
    test<-rbind(Htest,Dtest)
    val<-test[,ncol(test)]
    test<-test[,1:(ncol(test)-1)]
    
    ans1 <- KNN_random_break(train,test,cl,7,pnrm)
    modtest<-ans1[[1]]
    
    ans <- KNN_random_break(train,train,cl,7,pnrm)
    modtrain<-ans[[1]]
    
    train_rate<-as.data.frame(t(sort(summary(modtrain==cl))))
    trains[j,k]<-as.numeric(as.vector.factor((train_rate[train_rate[,2]==FALSE,][,3])
    ))/nrow(train)
    
    test_rate<-as.data.frame(t(sort(summary(modtest==cl))))
    tests[j,k]<-as.numeric(as.vector.factor((test_rate[test_rate[,2]==FALSE,][,3])
    ))/nrow(test)
  }
  k<-k+1
  }

test_mean<-NULL
train_mean<-NULL
test_sd<-NULL
train_sd<-NULL

for (i in c(1:length(normes))){
  test_mean[i]<-mean(t(tests[1:CV,i]))
  test_sd[i]<-sd(t(tests[1:CV,i]))
  train_mean[i]<-mean(t(trains[1:CV,i]))
  train_sd[i]<-sd(t(trains[1:CV,i]))
}
#trains<-trains[c(2:dime),]
#tests<-tests[c(2:dime),]
#dataf<-data.frame(dimension=c(2:dime,2:dime),rbind(tests,trains),set=as.factor(c(rep('tests',(dime-1)),rep('trains',(dime-1)))))
dataf<-data.frame(pnorm=normes,test_mean,test_sd,train_mean,train_sd,train=factor('train'),test=factor('test'))

cairo_ps(width=6,height = 3.5 ,file='metricloss17.eps')
ggplot()+theme_bw()+
geom_line(data=dataf,aes(x=pnorm, y=test_mean,colour=test),size=1)+
geom_line(data=dataf,aes(x=pnorm, y=train_mean,colour=train),size=1)+
geom_errorbar(data=dataf,aes(colour=test,x=pnorm,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
geom_errorbar(data=dataf,aes(colour=train,x=pnorm,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
labs(x='Dimension',y='Error')+
ylim(0,1)
dev.off()


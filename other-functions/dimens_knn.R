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
trains<-as.data.frame(c(2:dime))
tests<-as.data.frame(c(2:dime))
pnorm<-2

for (i in (2:dime)){
 
disea<-data.frame(diseas[,1:i],class=factor('parkinson'))
health<-data.frame(healthy[,1:i],class=factor('healthy'))

for (j in (1:CV)){

indexD <- sample(seq_len(nrow(diseas)), size = floor(nrow(diseas)*0.4))
indexH <- sample(seq_len(nrow(healthy)), size = floor(nrow(healthy)*0.4))

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

ans1 <- KNN_random_break(train,test,cl,7,pnorm)
modtest<-ans1[[1]]

ans <- KNN_random_break(train,train,cl,7,pnorm)
modtrain<-ans[[1]]

train_rate<-as.data.frame(t(sort(summary(modtrain==cl))))
trains[i,j]<-as.numeric(as.vector.factor((train_rate[train_rate[,2]==FALSE,][,3])
))/nrow(train)

test_rate<-as.data.frame(t(sort(summary(modtest==cl))))
tests[i,j]<-as.numeric(as.vector.factor((test_rate[test_rate[,2]==FALSE,][,3])
))/nrow(test)
}}

test_mean<-NULL
train_mean<-NULL
test_sd<-NULL
train_sd<-NULL


for (i in c(2:dime)){
  test_mean[i-1]<-mean(t(tests[i, 1:CV]))
  test_sd[i-1]<-sd(t(tests[i, 1:CV]))
  train_mean[i-1]<-mean(t(trains[i, 1:CV]))
  train_sd[i-1]<-sd(t(trains[i, 1:CV]))
  }
#trains<-trains[c(2:dime),]
#tests<-tests[c(2:dime),]
#dataf<-data.frame(dimension=c(2:dime,2:dime),rbind(tests,trains),set=as.factor(c(rep('tests',(dime-1)),rep('trains',(dime-1)))))
dataf<-data.frame(cbind(dimension=c(2:dime),test_mean,test_sd,train_mean,train_sd),train=factor('train'),test=factor('test'))

cairo_ps(width=6,height = 3.5 ,file='metricloss17.eps')
ggplot()+theme_bw()+
  geom_line(data=dataf,aes(x=dimension, y=test_mean,colour=test),size=1)+
  geom_line(data=dataf,aes(x=dimension, y=train_mean,colour=train),size=1)+
  geom_errorbar(data=dataf,aes(colour=test,x=dimension,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
  geom_errorbar(data=dataf,aes(colour=train,x=dimension,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
  scale_color_manual(values=c("#999999","#E69F00"),name="set")+
  labs(x='Dimension',y='Error')+ylim(0,0.28)
dev.off()


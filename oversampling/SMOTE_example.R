library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

{CN<-ex2[which(ex2$DX==1),]
MCI<-ex2[which(ex2$DX==2),]
AD<-ex2[which(ex2$DX==3),]
x<-rbind(CN,MCI,AD)[,2:3]
x<-Min_max_nrmlztn(x)
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
class<-as.factor(class)
rownames(x)<-NULL

CV<-10
pnorm<-2

perc<-seq(0,1.5,0.1)
conf<-matrix(0,3,3)

trains<-data.frame(NULL)
tests<-data.frame(NULL)
matrix_list<-NULL
lista<-NULL
ii<-1;k<-1;s<-1
classes_length<-1:length(class)

for(i in levels(class)){
  cl_length<-(1:length(class))[class==i]
  #set.seed(100)
  lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
  ii<-ii+1}


for (i in perc){
  print(i)
  matrix_aux<-matrix(0,3,3)
  for (j in c(1:CV)){
    
    train<-NULL;test<-NULL
    for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)
    }
    
    cltra<-class[train]; train<-x[train,]; cltra1<-cltra; train1<-train;
    cltest<-class[test]; test<-x[test,]
    
if(i != 0){
    data<-SMOTE(data = train,class = cltra, degree = i,pnorm = 2,k = 5)
    train<-rbind(train,data[[1]])
    cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))}
    #test<-rbind(test,data[[3]])
    #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
    
    ans1 <- knn_random_break(train,test,cltra,5,2)
    modtest<-as.character(ans1[[1]])
    cltest<-as.character(cltest)
    
    ans <- knn_random_break(train,train1,cltra,5,2)
    modtrain<-as.character(ans[[1]])
    cltra1<-as.character(cltra1)
    datag<-data.frame(cbind(as.vector(modtest),as.vector(cltest)))
    colnames(datag)<-c('predict','correct')
    conf <- data.frame()
    for (ii in c("CN","MCI","AD")){
      for(jj in c("CN","MCI","AD")){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    matrix_aux<-matrix_aux+conf
    tests[k,j]<-(conf[1,1]+conf[2,2]+conf[3,3])/3
    
    datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra1)))
    colnames(datag)<-c('predict','correct')
    conf1 <- data.frame()
    for (ii in c("CN","MCI","AD")){
      for(jj in c("CN","MCI","AD")){
        aux1<-datag[datag$correct==ii,]
        aux2<-aux1[aux1$predict==jj,]
        conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3}
  k<-1+k
  matrix_list[s]<-list(matrix_aux/(CV))
  s<-s+1
  #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
  #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
  #dev.off()
  #conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  #matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
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
#cairo_ps(width=7,height = 4 ,file='underneasrmiss1example.eps')
S<-ggplot()+theme_bw()+
  geom_line(data=dataf,aes(x=reduction, y=test_mean,colour=test),size=1)+
  geom_line(data=dataf,aes(x=reduction, y=train_mean,colour=train),size=1)+
  geom_errorbar(data=dataf,aes(colour=test,x=reduction,y=test_mean,ymax=test_mean+test_sd,ymin=test_mean-test_sd),size=1)+
  geom_errorbar(data=dataf,aes(colour=train,x=reduction,y=train_mean,ymax=train_mean+train_sd,ymin=train_mean-train_sd),size=1)+
  scale_color_manual(values=c("#999999","#E69F00"),name="set")+scale_x_continuous(labels=scales::percent)+
  labs(x='Undersampling percentage',y='Correct classified average')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        legend.position = "none",
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 
#dev.off()


A<-as.matrix(matrix_list[[1]]*100)
B<-as.matrix(matrix_list[[6]]*100)
C<-as.matrix(matrix_list[[11]]*100)
D<-as.matrix(matrix_list[[16]]*100)
cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_SMOTE.eps')
par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
dev.off()
cairo_ps(width=7,height = 4 ,file='SMOTE_example.eps')
S
dev.off()

} #SMOTE

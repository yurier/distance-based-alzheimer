library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)
library(Hmisc)
library(reshape2)

min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ex3<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)
CN<-ex3[which(ex3$DX=='NL'),]
MCI<-ex3[which(ex3$DX=="MCI"),]
AD<-ex3[which(ex3$DX=="Dementia"),]
x<-rbind(CN,MCI,AD)[,2:3]
x<-Min_max_nrmlztn(x)
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
class<-as.factor(class)
rownames(x)<-NULL

CV<-10
pnorm<-2

perc<-c(1,2,3)
conf<-matrix(0,3,3)

trains<-data.frame(NULL)
tests<-data.frame(NULL)
matrix_list<-NULL
lista<-NULL
ii<-1;k<-1;s<-1
classes_length<-1:length(class)

for(i in levels(class)){
  cl_length<-(1:length(class))[class==i]
  lista[ii]<-list(matrix(sample(cl_length,(length(cl_length)-(length(cl_length) %% CV))),as.integer(length(cl_length)/CV),CV))
  ii<-ii+1}


for (i in perc){
  matrix_aux<-matrix(0,3,3)
  for (j in c(1:CV)){
    
    train<-NULL;test<-NULL
    for (ii in 1:length(levels(class))){
      train<-c(lista[[ii]][,-j],train)
      test<-c(lista[[ii]][,j],test)
    }
    
    cltra<-class[train]; train<-x[train,]
    cltest<-class[test]; test<-x[test,]
    
    if (i==1){ans1 <- knn_random_break(train,train,cltra,5,pnorm)}
    if (i==2){ans1 <- knn_minus1_break(train,train,cltra,5,pnorm)}
    if (i==3){ans1 <- knn_plus1_break(train,train,cltra,5,pnorm)}
    modtest<-as.character(ans1[[1]])
    cltest<-as.character(cltest)
    
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
    
    }
  k<-1+k
  matrix_list[s]<-list(matrix_aux/(CV))
  s<-s+1
  print(i)
  #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
  #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
  #dev.off()
  #conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  #matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
}
rownames(tests)<-c("random","minus","plus")
melto<-data.frame(melt(t(tests)))

cairo_ps(width=6,height = 3 ,file='10foldties.eps')
ggplot(melto,aes(factor(X2),melto[,3]))+theme_bw()+
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",aes(width=0.5))+
  stat_summary(fun.y = 'median', geom = "point",colour="red")+
  geom_jitter(width = 0.2)+
  ylab("precision")+
  xlab("tie breaking strategy")+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
dev.off()

test_mean<-NULL
test_sd<-NULL

for (i in c(1:length(perc))){
  test_mean[i]<-mean(t(tests[i,1:CV]))
  test_sd[i]<-sd(t(tests[i,1:CV]))
}

A<-as.matrix(matrix_list[[1]]*100)
B<-as.matrix(matrix_list[[2]]*100)
C<-as.matrix(matrix_list[[3]]*100)
cairo_ps(width=11,height = 3.5 ,file='confusion_tie_breaking.eps')
par(mfrow = c(1,3),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
dev.off()


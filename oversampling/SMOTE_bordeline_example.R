library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(Hmisc)
library(grid)
library(reshape2)
plots<-NULL
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ex2<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
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
      
      ans1 <- knn_random_break(train,test,cltra,ifelse(i<.75,5,20),2)
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
      
      #datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra1)))
      #colnames(datag)<-c('predict','correct')
      #conf1 <- data.frame()
      #for (ii in c("CN","MCI","AD")){
      #  for(jj in c("CN","MCI","AD")){
      #    aux1<-datag[datag$correct==ii,]
      #    aux2<-aux1[aux1$predict==jj,]
      #    conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      #trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3
      }
    k<-1+k
    matrix_list[s]<-list(matrix_aux/(CV))
    s<-s+1
    #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
    #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 1),tl.col="black")
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
    #train_mean[i]<-mean(t(trains[i,1:CV]))
    #train_sd[i]<-sd(t(trains[i,1:CV]))
  }
  
  rownames(tests)<-c(perc)
  melto<-data.frame(melt(t(tests)))
  
  plots[[1]]<-ggplot(melto,aes_string(factor(melto[,2]),as.numeric(melto[,3])))+theme_bw()+
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",aes_string(width=0.5))+
    geom_jitter(width = 0.3)+
    stat_summary(fun.y = 'median', geom = "point",colour="red",alpha=.5)+
    ylab("precision")+
    scale_x_discrete(name='oversampling degree',labels=c("","10%","","30%","","50%","","70%","","90%","","110%","","130%","","150%"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  A<-as.matrix(matrix_list[[1]]*100)
  B<-as.matrix(matrix_list[[6]]*100)
  C<-as.matrix(matrix_list[[11]]*100)
  D<-as.matrix(matrix_list[[16]]*100)
  cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_SMOTE.eps')
  par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
  corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  dev.off()
  #cairo_ps(width=7,height = 4 ,file='SMOTE_example.eps')
  #S
  #dev.off()x'
  
} #SMOTE

{CN<-ex2[which(ex2$DX=="NL"),]
MCI<-ex2[which(ex2$DX=="MCI"),]
AD<-ex2[which(ex2$DX=="Dementia"),]
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
    data<-SMOTE_border1(train,cltra,i,2,5)
    train<-rbind(train,data[[1]])
    cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))}
    #test<-rbind(test,data[[3]])
    #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
    
    ans1 <- knn_random_break(train,test,cltra,ifelse(i<.75,5,20),2)
    modtest<-as.character(ans1[[1]])
    cltest<-as.character(cltest)
    
    #ans <- knn_random_break(train,train1,cltra,5,2)
    #modtrain<-as.character(ans[[1]])
    #cltra1<-as.character(cltra1)
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
    
    #datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra1)))
    #colnames(datag)<-c('predict','correct')
    #conf1 <- data.frame()
    #for (ii in c("CN","MCI","AD")){
    #  for(jj in c("CN","MCI","AD")){
    #    aux1<-datag[datag$correct==ii,]
    #    aux2<-aux1[aux1$predict==jj,]
    #    conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
    #trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3
    }
  k<-1+k
  matrix_list[s]<-list(matrix_aux/(CV))
  s<-s+1
  #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
  #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 1),tl.col="black")
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
  #train_mean[i]<-mean(t(trains[i,1:CV]))
  #train_sd[i]<-sd(t(trains[i,1:CV]))
}

rownames(tests)<-c(perc)
melto<-data.frame(melt(t(tests)))

plots[[2]]<-ggplot(melto,aes_string(factor(melto[,2]),as.numeric(melto[,3])))+theme_bw()+
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",aes_string(width=0.5))+
  geom_jitter(width = 0.3)+
  stat_summary(fun.y = 'median', geom = "point",colour="red",alpha=.5)+
  ylab("precision")+
  scale_x_discrete(name='oversampling degree',labels=c("","10%","","30%","","50%","","70%","","90%","","110%","","130%","","150%"))+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 

A<-as.matrix(matrix_list[[1]]*100)
B<-as.matrix(matrix_list[[6]]*100)
C<-as.matrix(matrix_list[[11]]*100)
D<-as.matrix(matrix_list[[16]]*100)
cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_SMOTE_borderline_1.eps')
par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
dev.off() #SMOTE
} #smote bordeline 1

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
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
        data<-SMOTE_border2(train,cltra,i,2,5)
        train<-rbind(train,data[[1]])
        cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))}
      #test<-rbind(test,data[[3]])
      #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,ifelse(i<.75,5,20),2)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      #ans <- knn_random_break(train,train1,cltra,5,2)
      #modtrain<-as.character(ans[[1]])
      #cltra1<-as.character(cltra1)
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
      
      #datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra1)))
      #colnames(datag)<-c('predict','correct')
      #conf1 <- data.frame()
      #for (ii in c("CN","MCI","AD")){
      #  for(jj in c("CN","MCI","AD")){
      #    aux1<-datag[datag$correct==ii,]
      #    aux2<-aux1[aux1$predict==jj,]
      #    conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      #trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3
      }
    k<-1+k
    matrix_list[s]<-list(matrix_aux/(CV))
    s<-s+1
    #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
    #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 1),tl.col="black")
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
    #train_mean[i]<-mean(t(trains[i,1:CV]))
    #train_sd[i]<-sd(t(trains[i,1:CV]))
  }
  
  rownames(tests)<-c(perc)
  melto<-data.frame(melt(t(tests)))
  
  plots[[3]]<-ggplot(melto,aes_string(factor(melto[,2]),as.numeric(melto[,3])))+theme_bw()+
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",aes_string(width=0.5))+
    geom_jitter(width = 0.3)+
    stat_summary(fun.y = 'median', geom = "point",colour="red",alpha=.5)+
    ylab("precision")+
    scale_x_discrete(name='oversampling degree',labels=c("","10%","","30%","","50%","","70%","","90%","","110%","","130%","","150%"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  
  A<-as.matrix(matrix_list[[1]]*100)
  B<-as.matrix(matrix_list[[6]]*100)
  C<-as.matrix(matrix_list[[11]]*100)
  D<-as.matrix(matrix_list[[16]]*100)
  cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_SMOTE_borderline_2.eps')
  par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
  corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  dev.off() #SMOTE
  
} #smote bordeline 2

{CN<-ex2[which(ex2$DX=="NL"),]
  MCI<-ex2[which(ex2$DX=="MCI"),]
  AD<-ex2[which(ex2$DX=="Dementia"),]
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
        data<-SMOTE_border3(train,cltra,i,2,5)
        train<-rbind(train,data[[1]])
        cltra<-as.factor(c(as.character(cltra),as.character(data[[2]])))}
      #test<-rbind(test,data[[3]])
      #cltest<-as.factor(c(as.character(cltest),as.character(data[[4]])))
      
      ans1 <- knn_random_break(train,test,cltra,ifelse(i<.75,5,20),2)
      modtest<-as.character(ans1[[1]])
      cltest<-as.character(cltest)
      
      #ans <- knn_random_break(train,train1,cltra,5,2)
      #modtrain<-as.character(ans[[1]])
      #cltra1<-as.character(cltra1)
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
      
      #datag<-data.frame(cbind(as.vector(modtrain),as.vector(cltra1)))
      #colnames(datag)<-c('predict','correct')
      #conf1 <- data.frame()
      #for (ii in c("CN","MCI","AD")){
      #  for(jj in c("CN","MCI","AD")){
      #    aux1<-datag[datag$correct==ii,]
      #    aux2<-aux1[aux1$predict==jj,]
      #    conf1[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      #trains[k,j]<-(conf1[1,1]+conf1[2,2]+conf1[3,3])/3
      }
    k<-1+k
    matrix_list[s]<-list(matrix_aux/(CV))
    s<-s+1
    #cairo_ps(width=5,height = 5 ,file=sprintf('conf_%g.eps',10*i))
    #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 1),tl.col="black")
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
    #train_mean[i]<-mean(t(trains[i,1:CV]))
    #train_sd[i]<-sd(t(trains[i,1:CV]))
  }
  
  rownames(tests)<-c(perc)
  melto<-data.frame(melt(t(tests)))
  
  plots[[4]]<-ggplot(melto,aes_string(factor(melto[,2]),as.numeric(melto[,3])))+theme_bw()+
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",aes_string(width=0.5))+
    geom_jitter(width = 0.3)+
    stat_summary(fun.y = 'median', geom = "point",colour="red",alpha=.5)+
    ylab("precision")+
    scale_x_discrete(name='oversampling degree',labels=c("","10%","","30%","","50%","","70%","","90%","","110%","","130%","","150%"))+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  A<-as.matrix(matrix_list[[1]]*100)
  B<-as.matrix(matrix_list[[6]]*100)
  C<-as.matrix(matrix_list[[11]]*100)
  D<-as.matrix(matrix_list[[16]]*100)
  
  cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_SMOTE_borderline_3.eps')
  par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
  corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
  dev.off()
  
} #smote bordeline 3

cairo_ps(width=12,height = 8,file='smote_borderline_example_plot.eps')
grid.arrange(arrangeGrob(plots[[1]] + theme(legend.position="none"),
                         plots[[2]] +ylab("") +theme(legend.position="none"),
                         plots[[3]] + theme(legend.position="none"),
                         plots[[4]] +ylab("") + theme(legend.position="none"),
                         nrow=2,ncol = 2))
dev.off()
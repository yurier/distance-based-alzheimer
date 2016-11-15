  library(mvtnorm)
  library(ggplot2)
  library(class)
  library(corrplot)
  library(MASS)
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  ex3<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)
 
  set.seed(1)
  ex3<-data.frame(DXCHANGE=ex3[,1],Min_max_nrmlztn(ex3[,2:3]))
  row.names(ex3_norm)<-NULL
  CN<-ex3[which(ex3$DX=='NL'),]
  MCI<-ex3[which(ex3$DX=="MCI"),]
  AD<-ex3[which(ex3$DX=="Dementia"),]
  
  indexCN <- sample(seq_len(nrow(CN)), size = floor(0.5 *nrow(CN)))
  indexMCI <- sample(seq_len(nrow(MCI)), size = floor(0.5 *nrow(MCI)))
  indexAD <- sample(seq_len(nrow(AD)), size = floor(0.5*nrow(AD)))
  
  ADtest <-AD[-indexAD, ]
  AD <- AD[indexAD, ]
  MCItest <- MCI[-indexMCI, ]
  MCI <- MCI[indexMCI, ]
  CNtest <- CN[-indexCN, ]
  CN <- CN[indexCN, ]
  
  x<-rbind(CN,MCI,AD)[,2:3]
  classx<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
  row.names(x) <- NULL
  
  ytest<-rbind(CNtest,MCItest,ADtest)[,2:3]
  classy<-c(rep("CN",nrow(CNtest)),rep("MCI",nrow(MCItest)),rep("AD",nrow(ADtest)))
  #######################################################
  cairo_ps(width=11,height = 3.5 ,file='confusion_tie_breaking.eps')
  par(mfrow = c(1,3),oma = c(0,0,0,0),mar = c(0,0,0,0))
  ans <- knn_minus1_break(x,ytest,as.factor(classx),5,2)
  mod<-ans[[1]]
  datag<-cbind(ytest,as.vector(mod),as.vector(classy))
  colnames(datag)<-c('x','y','predict','correct')
  conf <- data.frame()
  for (i in c("AD","MCI","CN")){
    for(j in c("AD","MCI","CN")){
      aux1<-datag[datag$correct==i,]
      aux2<-aux1[aux1$predict==j,]
      conf[i,j]<-nrow(aux2)/nrow(aux1)
    }
  }
  (conf[1,1]+conf[2,2]+conf[3,3])/3
  corrplot(as.matrix(conf*100),is.corr = FALSE,order ="AOE",method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
  #######################################################
  ans <- knn_plus1_break(x,ytest,as.factor(classx),5,2)
  mod<-ans[[1]]
  datag<-cbind(ytest,as.vector(mod),as.vector(classy))
  colnames(datag)<-c('x','y','predict','correct')
  conf <- data.frame()
  for (i in c("AD","MCI","CN")){
    for(j in c("AD","MCI","CN")){
      aux1<-datag[datag$correct==i,]
      aux2<-aux1[aux1$predict==j,]
      conf[i,j]<-nrow(aux2)/nrow(aux1)
    }
  }
  (conf[1,1]+conf[2,2]+conf[3,3])/3
  corrplot(as.matrix(conf*100),is.corr = FALSE,order ="AOE",method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
  
  ans <- knn_random_break(x,ytest,as.factor(classx),5,2)
  mod<-ans[[1]]
  datag<-cbind(ytest,as.vector(mod),as.vector(classy))
  colnames(datag)<-c('x','y','predict','correct')
  conf <- data.frame()
  for (i in c("AD","MCI","CN")){
    for(j in c("AD","MCI","CN")){
      aux1<-datag[datag$correct==i,]
      aux2<-aux1[aux1$predict==j,]
      conf[i,j]<-nrow(aux2)/nrow(aux1)
    }
  }
  (conf[1,1]+conf[2,2]+conf[3,3])/3
  corrplot(as.matrix(conf*100),is.corr = FALSE,order ="AOE",method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2.5,number.cex = 2.5,cl.pos="n",tl.srt=0)
  
  #######################################################
  dev.off()

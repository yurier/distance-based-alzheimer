library(corrplot)

{datamelt<-melt(as.matrix(full2fold_precision))
colnames(datamelt)<-c('dataset',"method","average precision")
ggplot(data=datamelt,
            aes(x=dataset,y=`average precision`,group = method, colour = method, label = method))+geom_point()

levels<-c("MN","PL","RD","RU","N1","N2","N3","N4","MD","S","S1","S2","S3")

cont<-matrix(NA,nrow = length(levels(datamelt[,2])),ncol = length(levels(datamelt[,2])))
y<-1;x<-1




for (i in levels){
  for (j in levels){
    cont[x,y]<-sum(as.integer((datamelt)[datamelt[,2]==j,][,3]<(datamelt)[datamelt[,2]==i,][,3]))/length(levels(aux[,1]))
    print(sum(as.integer((datamelt)[datamelt[,2]==j,][,3]<(datamelt)[datamelt[,2]==i,][,3])))
    y<-y+1}
  y<-1
  x<-x+1      
  }

rownames(cont)<-levels
colnames(cont)<-levels

cairo_ps(width=11,height = 11 ,file="cont_2cv.eps")
corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
dev.off()
} #2-fold cv

{datamelt<-melt(as.matrix(loocvdata_precision))
colnames(datamelt)<-c('dataset',"method","average precision")
ggplot(data=datamelt,
       aes(x=dataset,y=`average precision`,group = method, colour = method, label = method))+geom_point()

levels<-c("MN","PL","RD","RU","N1","N2","N3","N4","MD","S","S1","S2","S3")

cont<-matrix(NA,nrow = length(levels(datamelt[,2])),ncol = length(levels(datamelt[,2])))
y<-1;x<-1




for (i in levels){
  for (j in levels){
    cont[x,y]<-sum(as.integer((datamelt)[datamelt[,2]==j,][,3]<(datamelt)[datamelt[,2]==i,][,3]))/length(levels(aux[,1]))
    print(sum(as.integer((datamelt)[datamelt[,2]==j,][,3]<(datamelt)[datamelt[,2]==i,][,3])))
    y<-y+1}
  y<-1
  x<-x+1      
}

rownames(cont)<-levels
colnames(cont)<-levels

cairo_ps(width=11,height = 11 ,file="cont_loocv.eps")
corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
dev.off()} #loocv
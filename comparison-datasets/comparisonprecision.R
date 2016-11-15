name_methods<-c("S","S1","S2","S3","N1","N2","N3","N4",'MD',"RU","Imb")
conf<-matrix(0,nrow=length(name_methods),ncol=length(name_methods))
colnames(conf)<-name_methods; rownames(conf)<-name_methods
LOOCV_conf<-conf;  fold2CV_conf<-conf

for (i in name_methods){
  for (j in name_methods){
    fold2CV_conf[i,j]<-sum(as.integer(folsCV_imb[,j]<folsCV_imb[,i]))/8
  }}

for (i in name_methods){
  for (j in name_methods){
    LOOCV_conf[i,j]<-sum(as.integer(LOOCV_imb[,j]<LOOCV_imb[,i]))/8
  }}

cairo_ps(width=8,height = 8 ,file='LOOCV_conf.eps')
corrplot(LOOCV_conf*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.75,number.cex = 1.78,cl.pos="n",tl.srt=0)
dev.off()

cairo_ps(width=8,height = 8 ,file='fold2CV_conf.eps')
corrplot(fold2CV_conf*100,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.75,number.cex = 1.78,cl.pos="n",tl.srt=0)
dev.off()

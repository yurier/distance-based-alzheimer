library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ex2<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)
CN<-ex2[which(ex2$DX=="NL"),]
AD<-ex2[which(ex2$DX=="Dementia"),]
x<-rbind(CN,AD)[,2:3]

scale1=(max(x[,1])-min(x[,1]))/110
scale2=(max(x[,2])-min(x[,2]))/110

class<-c(rep("CN",nrow(CN)),rep("AD",nrow(AD)))
g<-c(rep(0,nrow(CN)),rep(1,nrow(AD)))

xnew<-expand.grid(x=seq(min(x[,1]-1), max(x[,1]+1),
                        by=scale1),
                  y=seq(min(x[,2]-1), max(x[,2]+1), 
                        by=scale2))


#mod2 <- KNN_random_break(x, xnew, g, k=5, 2)
#prob <- mod2[[2]]
#prob1 <- ifelse(mod2[[1]]=="1", prob, 1-prob)
#prob <- ifelse(mod2[[1]]=="1", 0, 1)
mod <- knn(x, xnew, g, k=5, prob=TRUE)
prob <- attr(mod, "prob")
prob1 <- ifelse(mod=="1", prob, 1-prob)
prob <- ifelse(mod=="1", 0, 1)

px1<-seq(min(x[,1]-1), max(x[,1]+1),by=scale1)
px2<-seq(min(x[,2]-1), max(x[,2]+1),by=scale2)
prob15 <- matrix(prob, length(px1), length(px2))
prob25<-matrix(prob1, length(px1), length(px2))
par(mar=rep(2,4))


dataf<-cbind(xnew,as.vector(prob15),as.vector(prob25))
colnames(dataf)<-c('x','y','prob','probability')

cairo_ps(width=9,height = 5,file='binaryknn.eps')

ggplot(dataf)+
  theme_bw()+
  geom_contour(aes(x=x, y=y, z=prob), bins=1,data=dataf, colour = "black", alpha = 1,size=1)+
  geom_point(aes(x=x, y=y, size=probability), colour = "black", alpha = 1/3)+
  scale_size(range=c(-1, 3))+
  geom_point(aes(x=x, y=y, color=class), size=3,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+
  scale_colour_manual(values=c( "#E69F00","#56B4E9"))+
  labs(x='ABETA',y='PTAU')+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
dev.off()


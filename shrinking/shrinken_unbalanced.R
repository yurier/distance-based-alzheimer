library(ggplot2)
library(mvtnorm)
library(class)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(1234) #fixed seed

low<-data.frame(t(c(0,0)))
for (i in 1:7){
  x<-runif(1,-1,1)
  y<-runif(1,-sqrt(1-x^2),sqrt(1-x^2))
  low<-rbind(low,c(x,y))}

x<-data.frame(rnorm(110,2,4))
y<-data.frame(rnorm(110,2,4))
big<-data.frame(x,y)
names(big)<-c("X1","X2")

circle<-circleFun(c(0,0),2*1,npoints = 1000) #geom_path will do open circles, geom_polygon will do filled circles                                                                                                                                                                                                                                                                                                                                 axis.text.x=element_blank(),axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank())

x<-rbind(low,big)[,1:2]
class<-c(rep("A",nrow(low)),rep("B",nrow(big)))

row.names(x) <- NULL

xnew<-expand.grid(x=seq(min(x[,1]-1), max(x[,1]+1),
                        by=0.03),
                  y=seq(min(x[,2]-1), max(x[,2]+1), 
                        by=0.03))

mod <- knn(x, xnew, class, k=3, prob=TRUE)
prob <- attr(mod, "prob")
prob1 <- ifelse(mod=="A", prob, 1-prob)
prob <- ifelse(mod=="A", 0, 1)

px1<-seq(min(x[,1]-1), max(x[,1]+1),by=0.03)
px2<-seq(min(x[,2]-1), max(x[,2]+1),by=0.03)
prob15 <- matrix(prob, length(px1), length(px2))
prob25<-matrix(prob1, length(px1), length(px2))
par(mar=rep(2,4))


dataf<-cbind(xnew,as.vector(prob15),as.vector(prob25))
colnames(dataf)<-c('x','y','prob','probability')

cairo_ps(width=7,height = 4,file='shrink.eps')
ggplot(dataf)+
  geom_contour(aes(x=x, y=y, z=prob), bins=1 ,data=dataf, colour = "black", alpha = 1,size=1)+
  theme_bw()+
  geom_point(aes(x=x, y=y, size=probability), colour = "black", alpha = 1/3)+
  scale_size(range=c(-1, 3.5))+
  geom_point(aes(x=x, y=y, color=class), size=3,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+
  scale_colour_manual(values=c( "#E69F00","#56B4E9"))+
  labs(x='Variable 1',y='Variable 2')+
  geom_point(data=circle,aes(x=x,y=y),size=0.5)+
  xlim(-2,2)+
  ylim(-1,2)+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()
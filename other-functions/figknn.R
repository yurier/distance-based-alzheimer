library(gridExtra)
library(grid)
library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(1254) #fixed seed
ex<-data.frame('x'=rnorm(10),'y'=rnorm(10),'class'=c(rep('A',5),rep('B',5)))  #create a random dataset
point=c(-0.5,-0.5) #inquiring point
dist<-NULL #declaring the vector of distances
for (i in 1:nrow(ex)){
  dist[i]<-norm(as.matrix(ex[i,1:2]-point))
}
newdata<-cbind(ex,dist)
newdata <- newdata[order(dist),]
dat <- circleFun(point,2*newdata$dist[5]+0.1,npoints = 1000) #geom_path will do open circles, geom_polygon will do filled circles

cairo_ps(width=6,height = 3.1 ,file='fig1.eps')
ggplot(ex,aes(x=x,y=y))+geom_point(aes(color=class),size=4)+
  scale_colour_manual(values=c("#999999",  "#E69F00"))+
  geom_point(aes(x=point[1],y=point[2]),shape=17,size=3)+
  geom_point(data=dat,aes(x=x,y=y),size=0.5)+
  geom_path(data=dat,size=0.5)+
  coord_cartesian(xlim = c(-0.4, 0.7))+
  coord_cartesian(ylim = c(-1.6, 0.6))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dev.off()

set.seed(1) #fixed seed
ex<-data.frame('x'=rnorm(21),'y'=rnorm(21),'class'=c(rep('A',7),rep('B',7),rep('C',7)))  #create a random dataset
point=c(0.5,0) #inquiring point
dist<-NULL #declaring the vector of distances
for (i in 1:nrow(ex)){
  dist[i]<-norm(as.matrix(ex[i,1:2]-point))
}
newdata<-cbind(ex,dist)
newdata <- newdata[order(dist),]
dat <- circleFun(point,2*newdata$dist[5]+0.07,npoints = 1000) #geom_path will do open circles, geom_polygon will do filled circles

cairo_ps(width=6,height = 3.1 ,file='fig2.eps')
ggplot(ex,aes(x=x,y=y))+geom_point(aes(color=class),size=4)+
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_point(aes(x=point[1],y=point[2]),shape=17,size=3)+
  geom_point(data=dat,aes(x=x,y=y),size=0.5)+
  geom_path(data=dat,size=0.5)+
  xlim(c(-0.6, 1.2))+
  ylim(c(-0.55, .55))+
theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dev.off()


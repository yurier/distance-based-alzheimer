library(mvtnorm)
library(ggplot2)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ex1<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA)
CN<-ex1[which(ex1$DX=="NL"),]
AD<-ex1[which(ex1$DX=="Dementia"),]
set.seed(12)
CN<-data.frame(rnorm(100000,mean(CN[,2]),sd(CN[,2])),class="CN")
AD<-data.frame(rnorm(100000,mean(AD[,2]),sd(AD[,2])),class='AD')
names(CN)<-c("ABETA","class")
names(AD)<-c("ABETA","class")
aux<-rbind(CN,AD)

cairo_ps(width=6,height = 3.1 ,file='ex1.eps')
ggplot(aux, aes(x=ABETA))+
  geom_density(aes(group=class, colour=class, fill=class), alpha=0.3)+
  theme_bw()+ylab("Probability density")+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  geom_vline(xintercept = 168)+
  xlim(0,350)+
  theme(legend.text = element_text(size = 15),
        axis.text=element_text(size=15),
        legend.title = element_text(size = 15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
dev.off()
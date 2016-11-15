library(corrplot)
library(mvtnorm)
library(ggplot2)
library(class)
library(MASS)
library(gridExtra)
library(grid)
library(Hmisc)
library(reshape)
ex3<-data.frame(DX=AD1$DX,ABETA=AD1$ABETA,PTAU=AD1$PTAU)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
##undersampling
{###undersampled 50
      { CN<-ex3[which(ex3$DX=='NL'),]
        MCI<-ex3[which(ex3$DX=="MCI"),]
        AD<-ex3[which(ex3$DX=="Dementia"),]
        x<-rbind(CN,MCI,AD)[,2:3]
        class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
        class<-as.factor(class)
        rownames(x)<-NULL
        list_data_norm<-list(data.frame(Min_max_nrmlztn(x),class=class),
                             data.frame(z_nrmlztn(x),class=class),
                             data.frame(max_nrmlztn(x),class=class),
                             data.frame(decimal_nrmlztn(x),class=class))
        norm_results<-as.data.frame(matrix(0,nrow=length(list_data_norm),ncol=floor(sqrt(nrow(set)))))
        norm_results_sd<-norm_results
        for (s in 1:length(list_data_norm)){
          set<-data.frame(list_data_norm[s])
          row.names(set)<-NULL
          class<-set[,ncol(set)]
          class<-as.factor(as.character(class))
          set<-set[,-ncol(set)]
          row.names(set)<-NULL
          trains<-data.frame(NULL)
          tests<-data.frame(NULL)
          colnames(conf)<-levels(class);rownames(conf)<-levels(class)
          for (k in 1:floor(sqrt(nrow(set)))){
            print(k)
            for (jjj in 1:2){
              for (j in 1:nrow(set)){
                train<-set[-j,]
                cltra<-class[-j]
                test<-set[j,]
                cltes<-class[j]
                
                data<-undersample(train,cltra,rnorm(1),.5)
                train<-data[[1]]
                cltra<-data[[2]]
                
                ans1 <- knn_random_break(train,test,cltra,k,2)
                modtest[j]<-as.character(ans1[[1]][1])
              }
              datag<-data.frame(correct=as.character(class),predict=modtest)
              modtest<-NULL
              conf<-matrix(0,length(levels(class)),length(levels(class)))
              colnames(conf)<-levels(class);rownames(conf)<-levels(class)
              for (ii in levels(class)){
                for(jj in levels(class)){
                  aux1<-datag[datag$correct==ii,]
                  aux2<-aux1[aux1$predict==jj,]
                  conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
                }}
              maux<-0
              for (m in 1:length(levels(class))){
                maux<-as.numeric(conf[m,m])+maux}
              
              tests[k,jjj]<-maux/length(levels(class))}}
          
          for (i in 1:k){
            norm_results[s,i]<-mean(t(tests[i,1:jjj]))
            norm_results_sd[s,i]<-sd(t(tests[i,1:jjj]))
          }
          
          print(norm_results)
          
        } #LOOCV
        
        colnames(norm_results)<-1:ncol(norm_results);  colnames(norm_results_sd)<-1:ncol(norm_results_sd);
        rownames(norm_results)<-c("min-max","z-score","max","decimal"); rownames(norm_results_sd)<-c("min-max","z-score","max","decimal")
        melt_ur50<-melt(as.matrix(norm_results*100))
        melt_ur50_sd<-melt(as.matrix(norm_results_sd*100))
        norm_results_ur50<-norm_results*100
        melt_ur50[,1]<-as.factor(melt_ur50[,1]); colnames(melt_ur50)<-c("normalization","k","average precision")
      }
  
  
  ###undersampled 75
  {
    CN<-ex3[which(ex3$DX=='NL'),]
    MCI<-ex3[which(ex3$DX=="MCI"),]
    AD<-ex3[which(ex3$DX=="Dementia"),]
    x<-rbind(CN,MCI,AD)[,2:3]
    class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
    class<-as.factor(class)
    rownames(x)<-NULL
    list_data_norm<-list(data.frame(Min_max_nrmlztn(x),class=class),
                         data.frame(z_nrmlztn(x),class=class),
                         data.frame(max_nrmlztn(x),class=class),
                         data.frame(decimal_nrmlztn(x),class=class))
    norm_results<-as.data.frame(matrix(0,nrow=length(list_data_norm),ncol=floor(sqrt(nrow(set)))))
    norm_results_sd<-norm_results
    for (s in 1:length(list_data_norm)){
      set<-data.frame(list_data_norm[s])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      for (k in 1:floor(sqrt(nrow(set)))){
        print(k)
        for (jjj in 1:2){
          for (j in 1:nrow(set)){
            train<-set[-j,]
            cltra<-class[-j]
            test<-set[j,]
            cltes<-class[j]
            
            data<-undersample(train,cltra,rnorm(1),.75)
            train<-data[[1]]
            cltra<-data[[2]]
            
            ans1 <- knn_random_break(train,test,cltra,k,2)
            modtest[j]<-as.character(ans1[[1]][1])
          }
          datag<-data.frame(correct=as.character(class),predict=modtest)
          modtest<-NULL
          conf<-matrix(0,length(levels(class)),length(levels(class)))
          colnames(conf)<-levels(class);rownames(conf)<-levels(class)
          for (ii in levels(class)){
            for(jj in levels(class)){
              aux1<-datag[datag$correct==ii,]
              aux2<-aux1[aux1$predict==jj,]
              conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
            }}
          maux<-0
          for (m in 1:length(levels(class))){
            maux<-as.numeric(conf[m,m])+maux}
          
          tests[k,jjj]<-maux/length(levels(class))}}
      
      for (i in 1:k){
        norm_results[s,i]<-mean(t(tests[i,1:jjj]))
        norm_results_sd[s,i]<-sd(t(tests[i,1:jjj]))
      }
      
      print(norm_results)
      
    } #LOOCV
    colnames(norm_results)<-1:ncol(norm_results);  colnames(norm_results_sd)<-1:ncol(norm_results_sd);
    rownames(norm_results)<-c("min-max","z-score","max","decimal"); rownames(norm_results_sd)<-c("min-max","z-score","max","decimal")
    melt_ur75<-melt(as.matrix(norm_results*100))
    melt_ur75_sd<-melt(as.matrix(norm_results_sd*100))
    norm_results_ur75<-norm_results*100
    melt_ur75[,1]<-as.factor(melt_ur75[,1]); colnames(melt_ur75)<-c("normalization","k","average precision")
  } 
  ###undersampled 100
  {
    CN<-ex3[which(ex3$DX=='NL'),]
    MCI<-ex3[which(ex3$DX=="MCI"),]
    AD<-ex3[which(ex3$DX=="Dementia"),]
    x<-rbind(CN,MCI,AD)[,2:3]
    class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
    class<-as.factor(class)
    rownames(x)<-NULL
    list_data_norm<-list(data.frame(Min_max_nrmlztn(x),class=class),
                         data.frame(z_nrmlztn(x),class=class),
                         data.frame(max_nrmlztn(x),class=class),
                         data.frame(decimal_nrmlztn(x),class=class))
    norm_results<-as.data.frame(matrix(0,nrow=length(list_data_norm),ncol=floor(sqrt(nrow(set)))))
    norm_results_sd<-norm_results
    
    for (s in 1:length(list_data_norm)){
      set<-data.frame(list_data_norm[s])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      for (k in 1:floor(sqrt(nrow(set)))){
        print(k)
        for (jjj in 1:2){
          for (j in 1:nrow(set)){
            train<-set[-j,]
            cltra<-class[-j]
            test<-set[j,]
            cltes<-class[j]
            
            data<-undersample(train,cltra,rnorm(1),1)
            train<-data[[1]]
            cltra<-data[[2]]
            
            ans1 <- knn_random_break(train,test,cltra,k,2)
            modtest[j]<-as.character(ans1[[1]][1])
          }
          datag<-data.frame(correct=as.character(class),predict=modtest)
          modtest<-NULL
          conf<-matrix(0,length(levels(class)),length(levels(class)))
          colnames(conf)<-levels(class);rownames(conf)<-levels(class)
          for (ii in levels(class)){
            for(jj in levels(class)){
              aux1<-datag[datag$correct==ii,]
              aux2<-aux1[aux1$predict==jj,]
              conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
            }}
          maux<-0
          for (m in 1:length(levels(class))){
            maux<-as.numeric(conf[m,m])+maux}
          
          tests[k,jjj]<-maux/length(levels(class))}}
      
      for (i in 1:k){
        norm_results[s,i]<-mean(t(tests[i,1:jjj]))
        norm_results_sd[s,i]<-sd(t(tests[i,1:jjj]))
      }
      
      print(norm_results)
      
    } #LOOCV
    colnames(norm_results)<-1:ncol(norm_results);  colnames(norm_results_sd)<-1:ncol(norm_results_sd);
    rownames(norm_results)<-c("min-max","z-score","max","decimal"); rownames(norm_results_sd)<-c("min-max","z-score","max","decimal")
    melt_ur100<-melt(as.matrix(norm_results*100))
    melt_ur100_sd<-melt(as.matrix(norm_results_sd*100))
    norm_results_ur100<-norm_results*100
    melt_ur100[,1]<-as.factor(melt_ur100[,1]);
    colnames(melt_ur100)<-c("normalization","k","average precision")
  } 
  ###imbalanced
  {
    CN<-ex3[which(ex3$DX=='NL'),]
    MCI<-ex3[which(ex3$DX=="MCI"),]
    AD<-ex3[which(ex3$DX=="Dementia"),]
    x<-rbind(CN,MCI,AD)[,2:3]
    class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
    class<-as.factor(class)
    rownames(x)<-NULL
    list_data_norm<-list(data.frame(Min_max_nrmlztn(x),class=class),
                         data.frame(z_nrmlztn(x),class=class),
                         data.frame(max_nrmlztn(x),class=class),
                         data.frame(decimal_nrmlztn(x),class=class))
    norm_results<-as.data.frame(matrix(0,nrow=length(list_data_norm),ncol=floor(sqrt(nrow(set)))))
    norm_results_sd<-norm_results
    
    for (s in 1:length(list_data_norm)){
      set<-data.frame(list_data_norm[s])
      row.names(set)<-NULL
      class<-set[,ncol(set)]
      class<-as.factor(as.character(class))
      set<-set[,-ncol(set)]
      row.names(set)<-NULL
      trains<-data.frame(NULL)
      tests<-data.frame(NULL)
      conf<-matrix(0,length(levels(class)),length(levels(class)))
      colnames(conf)<-levels(class);rownames(conf)<-levels(class)
      for (k in 1:floor(sqrt(nrow(set)))){
        print(k)
        for (jjj in 1:2){
          for (j in 1:nrow(set)){
            train<-set[-j,]
            cltra<-class[-j]
            test<-set[j,]
            cltes<-class[j]
            
            ans1 <- knn_random_break(train,test,cltra,k,2)
            modtest[j]<-as.character(ans1[[1]][1])
          }
          datag<-data.frame(correct=as.character(class),predict=modtest)
          modtest<-NULL
          conf<-matrix(0,length(levels(class)),length(levels(class)))
          colnames(conf)<-levels(class);rownames(conf)<-levels(class)
          for (ii in levels(class)){
            for(jj in levels(class)){
              aux1<-datag[datag$correct==ii,]
              aux2<-aux1[aux1$predict==jj,]
              conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))
            }}
          maux<-0
          for (m in 1:length(levels(class))){
            maux<-as.numeric(conf[m,m])+maux}
          
          tests[k,jjj]<-maux/length(levels(class))}}
      
      for (i in 1:k){
        norm_results[s,i]<-mean(t(tests[i,1:jjj]))
        norm_results_sd[s,i]<-sd(t(tests[i,1:jjj]))
      }
      
      print(norm_results)
      
    } #LOOCV
    colnames(norm_results)<-1:ncol(norm_results);  colnames(norm_results_sd)<-1:ncol(norm_results_sd);
    rownames(norm_results)<-c("min-max","z-score","max","decimal"); rownames(norm_results_sd)<-c("min-max","z-score","max","decimal") 
    melt_imb<-melt(as.matrix(norm_results*100))
    melt_imb_sd<-melt(as.matrix(norm_results_sd*100))
    norm_results_imb<-norm_results*100
    melt_imb[,1]<-as.factor(melt_imb[,1]); colnames(melt_imb)<-c("normalization","k","average precision")
  } 
  ###maxes
  {
    max_imb<-rbind((melt_imb[melt_imb[,1]==levels[1],])[which.max((melt_imb[melt_imb[,1]==levels[1],])[,3]),],
                   (melt_imb[melt_imb[,1]==levels[2],])[which.max((melt_imb[melt_imb[,1]==levels[2],])[,3]),],
                   (melt_imb[melt_imb[,1]==levels[3],])[which.max((melt_imb[melt_imb[,1]==levels[3],])[,3]),],
                   (melt_imb[melt_imb[,1]==levels[4],])[which.max((melt_imb[melt_imb[,1]==levels[4],])[,3]),])
    
    max_ur50<-rbind((melt_ur50[melt_ur50[,1]==levels[1],])[which.max((melt_ur50[melt_ur50[,1]==levels[1],])[,3]),],
                    (melt_ur50[melt_ur50[,1]==levels[2],])[which.max((melt_ur50[melt_ur50[,1]==levels[2],])[,3]),],
                    (melt_ur50[melt_ur50[,1]==levels[3],])[which.max((melt_ur50[melt_ur50[,1]==levels[3],])[,3]),],
                    (melt_ur50[melt_ur50[,1]==levels[4],])[which.max((melt_ur50[melt_ur50[,1]==levels[4],])[,3]),])
    
    max_ur75<-rbind((melt_ur75[melt_ur75[,1]==levels[1],])[which.max((melt_ur75[melt_ur75[,1]==levels[1],])[,3]),],
                    (melt_ur75[melt_ur75[,1]==levels[2],])[which.max((melt_ur75[melt_ur75[,1]==levels[2],])[,3]),],
                    (melt_ur75[melt_ur75[,1]==levels[3],])[which.max((melt_ur75[melt_ur75[,1]==levels[3],])[,3]),],
                    (melt_ur75[melt_ur75[,1]==levels[4],])[which.max((melt_ur75[melt_ur75[,1]==levels[4],])[,3]),])
    
    max_ur100<-rbind((melt_ur100[melt_ur100[,1]==levels[1],])[which.max((melt_ur100[melt_ur100[,1]==levels[1],])[,3]),],
                     (melt_ur100[melt_ur100[,1]==levels[2],])[which.max((melt_ur100[melt_ur100[,1]==levels[2],])[,3]),],
                     (melt_ur100[melt_ur100[,1]==levels[3],])[which.max((melt_ur100[melt_ur100[,1]==levels[3],])[,3]),],
                     (melt_ur100[melt_ur100[,1]==levels[4],])[which.max((melt_ur100[melt_ur100[,1]==levels[4],])[,3]),])
    
    
  }  
  ###plots of normalizations
  { A<-ggplot(data=melt_imb,aes(x=k,y=`average precision`,color=normalization))+theme_bw()+
    scale_color_manual(values = cbPalette)+scale_x_continuous(breaks = seq(0,25,5))+
    geom_line()+scale_y_continuous(limits=c(31,53),breaks=seq(31, 53, 9))+
    geom_point(data=max_imb,aes(x=k,y=`average precision`),size=1.5)+
    theme(legend.text = element_text(size = 15),
          axis.text=element_text(size=15),
          legend.title = element_text(size = 15),
          axis.title.y=element_text(size=15),
          axis.title.x=element_text(size=15),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
    
    B<-ggplot(data=melt_ur50,aes(x=k,y=`average precision`,color=normalization))+theme_bw()+
      geom_line()+scale_y_continuous(limits=c(31,53),breaks=seq(31, 53, 9))+
      scale_color_manual(values = cbPalette)+scale_x_continuous(breaks = seq(0,25,5))+
      geom_point(data=max_ur50,aes(x=k,y=`average precision`),size=1.5)+
      theme(legend.text = element_text(size = 15),
            axis.text=element_text(size=15),
            legend.title = element_text(size = 15),
            axis.title.y=element_text(size=15),
            axis.title.x=element_text(size=15),
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    C<-ggplot(data=melt_ur75,aes(x=k,y=`average precision`,color=normalization))+theme_bw()+
      geom_line()+scale_y_continuous(limits=c(31,53),breaks=seq(31, 53, 9))+
      scale_color_manual(values = cbPalette)+scale_x_continuous(breaks = seq(0,25,5))+
      geom_point(data=max_ur75,aes(x=k,y=`average precision`),size=1.5)+
      theme(legend.text = element_text(size = 15),
            axis.text=element_text(size=15),
            legend.title = element_text(size = 15),
            axis.title.y=element_text(size=15),
            axis.title.x=element_text(size=15),
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank()) 
    
    D<-ggplot(data=melt_ur100,aes(x=k,y=`average precision`,color=normalization))+theme_bw()+
      geom_line()+scale_y_continuous(limits=c(31,53),breaks=seq(31, 53, 9))+
      scale_color_manual(values = cbPalette)+scale_x_continuous(breaks = seq(0,25,5))+
      geom_point(data=max_ur100,aes(x=k,y=`average precision`),size=1.5)+
      theme(legend.text = element_text(size = 15),
            axis.text=element_text(size=15),
            legend.title = element_text(size = 15),
            axis.title.y=element_text(size=15),
            axis.title.x=element_text(size=15),
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank())
    mylegend<-g_legend(A)
    
    #cairo_ps(width=12,height = 3,file='undersampling_nearmiss1_knn_plot.eps')
    cairo_ps(width=14,height = 3,file='normalization_undersampling_plot_LOOCV.eps')
    
    
    grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                             B + ylab("")+theme(legend.position="none"),
                             C + ylab("")+theme(legend.position="none"),
                             D + ylab("")+theme(legend.position="none"),
                             nrow=1),
                 arrangeGrob(mylegend),
                 widths = c(5,0.6))
    dev.off()}
  ###confusion matrices plot
  {matrix_aux<-NULL
    levels<-c("min-max","z-score","max","decimal")
    
    { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
      y<-1;x<-1
      
      melt_imb[,1]<-as.character(melt_imb[,1])
      
      for (i in levels){
        for (j in levels){
          cont[x,y]<-sum(as.integer((melt_imb)[melt_imb[,1]==j,][,3]<(melt_imb)[melt_imb[,1]==i,][,3]))/ncol(norm_results)
          y<-y+1}
        y<-1
        x<-x+1      
      }
      
      rownames(cont)<-c("min","zsc","mx","dec")
      colnames(cont)<-c("min","zsc","mx","dec")
      
      
      matrix_aux[[1]]<-floor(cont*1000)/10
      #cairo_ps(width=11,height = 11 ,file="imb.eps")
      #corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
      #dev.off()
    } 
    
    { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
      y<-1;x<-1
      
      melt_ur50[,1]<-as.character(melt_ur50[,1])
      
      for (i in levels){
        for (j in levels){
          cont[x,y]<-sum(as.integer((melt_ur50)[melt_ur50[,1]==j,][,3]<(melt_ur50)[melt_ur50[,1]==i,][,3]))/ncol(norm_results)
          y<-y+1}
        y<-1
        x<-x+1      
      }
      
      rownames(cont)<-c("min","zsc","mx","dec")
      colnames(cont)<-c("min","zsc","mx","dec")
      
      matrix_aux[[2]]<-floor(cont*1000)/10
      #cairo_ps(width=11,height = 11 ,file="ur50.eps")
      #corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
      #dev.off()
    } 
    
    { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
      y<-1;x<-1
      
      melt_ur75[,1]<-as.character(melt_ur75[,1])
      
      for (i in levels){
        for (j in levels){
          cont[x,y]<-sum(as.integer((melt_ur75)[melt_ur75[,1]==j,][,3]<(melt_ur75)[melt_ur75[,1]==i,][,3]))/ncol(norm_results)
          y<-y+1}
        y<-1
        x<-x+1      
      }
      
      rownames(cont)<-c("min","zsc","mx","dec")
      colnames(cont)<-c("min","zsc","mx","dec")
      
      matrix_aux[[3]]<-floor(cont*1000)/10
      #cairo_ps(width=11,height = 11 ,file="ur75.eps")
      #corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
      #dev.off()
    }
    
    { cont<-matrix(NA,nrow =length(levels),ncol = length(levels))
      y<-1;x<-1
      
      melt_ur100[,1]<-as.character(melt_ur100[,1])
      
      for (i in levels){
        for (j in levels){
          cont[x,y]<-sum(as.integer((melt_ur100)[melt_ur100[,1]==j,][,3]<(melt_ur100)[melt_ur100[,1]==i,][,3]))/ncol(norm_results)
          y<-y+1}
        y<-1
        x<-x+1      
      }
      
      rownames(cont)<-c("min","zsc","mx","dec")
      colnames(cont)<-c("min","zsc","mx","dec")
      
      matrix_aux[[4]]<-floor(cont*1000)/10
      #cairo_ps(width=11,height = 11 ,file="ur100.eps")
      #corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, max(cont)*100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
      #dev.off()
    }
    
    A<-as.matrix(matrix_aux[[1]])
    B<-as.matrix(matrix_aux[[2]])
    C<-as.matrix(matrix_aux[[3]])
    D<-as.matrix(matrix_aux[[4]])
    cairo_ps(width=11,height = 2.7 ,file='confusion_matrix_normalization_LOOCV.eps')
    par(mfrow = c(1,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
    corrplot(A,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
    corrplot(B,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
    corrplot(C,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
    corrplot(D,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=FALSE,cl.lim=c(0, 100),tl.col="black",tl.cex = 2,number.cex = 2,cl.pos="n",tl.srt=0)
    dev.off()
    
  }
  
  melt_imb[which.max(melt_imb[,3]),]
  melt_ur50[which.max(melt_ur50[,3]),]
  melt_ur75[which.max(melt_ur75[,3]),]
  melt_ur100[which.max(melt_ur100[,3]),]
  }


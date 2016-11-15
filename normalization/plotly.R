library(plotly)
{col<-c("#999999", "#E69F00", "#56B4E9", "#009E73")
p<-plot_ly(data=big_melt,x=~CV,y=~k,z=~`average precision`,color=~normalization,mode="lines",colors=col)
for (i in 2:29){
p<-add_trace(p,data=big_melt[which(big_melt$CV==i),],x=~CV,y=~k,z=~`average precision`,color=~normalization,mode="lines",colors=col,showlegend = FALSE)
big_high<-data.frame()
for (j in levels(big_melt$normalization)){
  big_high<-rbind(big_high,big_melt[big_melt$CV==i,][big_melt[big_melt$CV==i,]$normalization==j,][which.max(big_melt[big_melt$CV==i,][big_melt[big_melt$CV==i,]$normalization==j,][,3]),])
}
p<-add_trace(p,data=big_high,x=~CV,y=~k,z=~`average precision`,color=~normalization,mode="marker",marker =list(size = 1),colors=col,showlegend = FALSE)

}
{i<-30
p<-add_trace(p,data=big_melt[which(big_melt$CV==i),],x=~CV,y=~k,z=~`average precision`,color=~normalization,mode="lines",colors=col,showlegend = TRUE)
for (j in levels(big_melt$normalization)){
  big_high<-rbind(big_high,big_melt[big_melt$CV==i,][big_melt[big_melt$CV==i,]$normalization==j,][which.max(big_melt[big_melt$CV==i,][big_melt[big_melt$CV==i,]$normalization==j,][,3]),])
}
p<-add_trace(p,data=big_high,x=~CV,y=~k,z=~`average precision`,color=~normalization,mode="marker",marker =list(size = 1),colors=col,showlegend = FALSE)
}}

{library(plotly)
col<-c("#999999", "#E69F00", "#56B4E9", "#009E73")
p1<-plot_ly(data=big_melt_sd,x=~CV,y=~k,z=~sd,color=~normalization,mode="lines",colors=col)
for (i in 2:29){
p1<-add_trace(p1,data=big_melt_sd[which(big_melt_sd$CV==i),],x=~CV,y=~k,z=~sd,color=~normalization,mode="lines",colors=col,showlegend = FALSE)
}
{i<-30
p1<-add_trace(p1,data=big_melt_sd[which(big_melt_sd$CV==i),],x=~CV,y=~k,z=~sd,color=~normalization,mode="lines",colors=col,showlegend = TRUE)
}}
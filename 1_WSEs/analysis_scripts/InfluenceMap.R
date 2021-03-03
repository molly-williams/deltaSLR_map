########This script allows the user expore and map the influence of SLR and hydrology changes throughout the Delta
########GREAT CARE SHOULD BE EXCERCISED IN USING THIS SCRIPT, THIS IS EXPLORITORY, IT IS NOT A DYNAMIC SCRIPT TO BE USED
########IN GENERAL WORKFLOW.
########Writen by Andrew Schwarz, Delta Stewardship Council
########Last Updated 10/16/2020


####Create Map of SLR/Hydrology Influence in Delta
library(sf)
library(tidyverse)

levee_pts<- st_read("C:/GIS Data/DSM2NodesSLRstage.shp")
levee_pts$Node<- paste0("Node_",levee_pts$nodenum)
levee_lines<- st_read("C:/GIS Data/DLISIslands.shp")

CDFall<-data.frame()

for(SLR in c(0:7,10)){
  for(HYDRO in c("Hist", "2050", "2085")){
CDF<- read.csv(paste0("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DSC-Generated/WSE/CDF/CDF_", HYDRO,"_", SLR, ".csv"))
SaveCDF<-CDF[,c(1,2,19,20,21)]
SaveCDF$SLR<-SLR
SaveCDF$Hydro<-HYDRO
CDFall<-rbind(CDFall, SaveCDF)
  }
}
Hist_0<-subset(CDFall, SLR==0 & Hydro=="Hist")
colnames(Hist_0)<-c("Node", "Hist_0_0.99", "Hist_0_0.02","Hist_0_0.01", "Hist_0_0.005", "SLR", "Hydro")
CDFall<-cbind(CDFall, Hist_0[,2:5])              


CDFall$diff99<- CDFall$X0.99-CDFall$Hist_0_0.99
CDFall$diff02<-CDFall$X0.02-CDFall$Hist_0_0.02
CDFall$diff01<- CDFall$X0.01-CDFall$Hist_0_0.01
CDFall$diff005<- CDFall$X0.005-CDFall$Hist_0_0.005

CDFall$influence=NA
for(j in c(1:7,10)){
CDFall[which(CDFall$diff02>(.8*j) & CDFall$SLR==j & CDFall$Hydro=="Hist"), "SLRinfluence"]<-1
CDFall[which(CDFall$diff02<(.8*j) & CDFall$SLR==j & CDFall$Hydro=="Hist"), "SLRinfluence"]<-0

}
for(i in c("2050")){  #2085 not included because the entire delta goes up over a foot at end of century
  CDFall[which(CDFall$diff02>1 & CDFall$SLR==0 & CDFall$Hydro==i), "Hydroinfluence"]<-1
  CDFall[which(CDFall$diff02<=1 & CDFall$SLR==0 & CDFall$Hydro==i), "Hydroinfluence"]<-0
} 



Influence<-data.frame(matrix(nrow=429, ncol=2))
colnames(Influence)<-c("Node", "Influence")
Nodes<- unique(CDFall$Node)
Influence$Node<-Nodes
for(k in c(1:429)){
  TestNode<-subset(CDFall, Node==Nodes[k])
  TestNode<-TestNode[,c(1,12:18)]
  if(sum(TestNode$SLRinfluence, na.rm=T)<1&sum(TestNode$Hydroinfluence, na.rm=T)>=1){Influence[k,2]<-"Riverine"}
     if(sum(TestNode$SLRinfluence, na.rm=T)>=1&sum(TestNode$Hydroinfluence, na.rm=T)==0){Influence[k,2]<-"SLR"}
            if(sum(TestNode$SLRinfluence, na.rm=T)>=1&sum(TestNode$Hydroinfluence, na.rm=T)>=1){Influence[k,2]<-"Transition"}
}


levee_pts<- st_read("C:/GIS Data/DSM2NodesSLRstage.shp")
levee_pts$Node<- paste0("Node_",levee_pts$nodenum)
levee_pts<- left_join(levee_pts,Influence)

ggplot()+geom_sf(data=levee_lines)+geom_sf(data=levee_pts, aes(color=Influence))+
 ggtitle("Area of Strongest Climate Change Influence\nThroughout the Delta")+
 theme_void() +theme(plot.title = element_text(hjust = 0.5))
  
ggsave(file="./Output/ClimateChangeInfluence_50.png")

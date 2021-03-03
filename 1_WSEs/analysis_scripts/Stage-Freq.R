########This script allows the user to generate stage frequency curves for any or all DSM2 nodes throughout the Delta.
########the user can specify whether they want to use Deterministic or Probablistic scenarios for creating future condtions
########stage-frequency curves. 
########Writen by Andrew Schwarz, Delta Stewardship Council
########Last Updated 10/16/2020

library(ggplot2)
library(readxl)
library(gridExtra)

for(i in 1:nrow(Nodes)){
FindNode<-as.character(Nodes[i,1])#if running outside of for loop, write Node_XX in quotes
NodeName<-as.character(Nodes[i,1])#if running outside of for loop, you can add a descriptive name for the node that will be printed on graph
###User Input:
RunType<- "Det" #"Det" or "Prob"

M0<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DeterministicRunsData_081420/MC_Deterministic_M0_300000_SLR_0_CDF.xlsx")
Nodes<-M0[,1]
M0<-M0[,-1]
M0<-data.frame(t(M0))
colnames(M0)<- t(Nodes)
M0$NonExceed<-(1-as.numeric(rownames(M0)))
M0$Scenario<-"Baseline"

if(RunType=="Det"){
  #########################################################################
  ####################Determinstic Scenarios###############################
  #########################################################################
  
M1<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DeterministicRunsData_081420/MC_Deterministic_M1_300000_SLR_0.5_CDF.xlsx")
M1<-M1[,-1]
M1<-data.frame(t(M1))
colnames(M1)<- t(Nodes)
M1$NonExceed<-(1-as.numeric(rownames(M1)))
M1$Scenario<-"2030 0.5SLR"
M2<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DeterministicRunsData_081420/MC_Deterministic_M2_300000_SLR_1_CDF.xlsx")
M2<-M2[,-1]
M2<-data.frame(t(M2))
colnames(M2)<- t(Nodes)
M2$NonExceed<-(1-as.numeric(rownames(M2)))
M2$Scenario<-"2050 1SLR"
M3<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DeterministicRunsData_081420/MC_Deterministic_M3_300000_SLR_2_CDF.xlsx")
M3<-M3[,-1]
M3<-data.frame(t(M3))
colnames(M3)<- t(Nodes)
M3$NonExceed<-(1-as.numeric(rownames(M3)))
M3$Scenario<-"2050 2SLR"
M4<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/DeterministicRunsData_081420/MC_Deterministic_M4_300000_SLR_3.5_CDF.xlsx")
M4<-M4[,-1]
M4<-data.frame(t(M4))
colnames(M4)<- t(Nodes)
M4$NonExceed<-(1-as.numeric(rownames(M4)))
M4$Scenario<-"2050+ 3.5SLR"
DetScenarios<- rbind(M0,M1,M2,M3,M4)
DetScenarios<-DetScenarios[,c(431,430,1:429)]
rownames(DetScenarios)<-NULL
NodeNum<-which(colnames(DetScenarios)==FindNode)

graphdata<- DetScenarios[, c(1,2, NodeNum )]
graphdata$Scenario<-factor(graphdata$Scenario, levels= c("Baseline", "2030 0.5SLR", "2050 1SLR", "2050 2SLR", "2050+ 3.5SLR"))
colnames(graphdata)[3]<-"NodeX"
DataTable<- reshape(DetScenarios[,c(1:2,NodeNum)], idvar="NonExceed", timevar="Scenario", direction = "wide" )
colnames(DataTable)<-c("Non-Exceedance Probability", "Baseline", "2030 0.5SLR", "2050 1SLR", "2050 2SLR", "2050+ 3.5SLR")
DataTable[,2:6]<-round(DataTable[,2:6], 2)
}else{
  #########################################################################
  ####################Probablistic Scenarios###############################
  #########################################################################
  M5<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/ProbabilisticRunsData_081920/MC_Probabilistic_M5_2000000_2030_CDF.xlsx")
  M5<-M5[,-1]
  M5<-data.frame(t(M5))
  colnames(M5)<- t(Nodes)
  M5$NonExceed<-(1-as.numeric(rownames(M5)))
  M5$Scenario<-"2030"
  M6<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/ProbabilisticRunsData_081920/MC_Probabilistic_M6_2000000_2050_CDF.xlsx")
  M6<-M6[,-1]
  M6<-data.frame(t(M6))
  colnames(M6)<- t(Nodes)
  M6$NonExceed<-(1-as.numeric(rownames(M6)))
  M6$Scenario<-"2050"
  M7<-read_excel("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/ProbabilisticRunsData_081920/MC_Probabilistic_M7_2000000_2085_CDF.xlsx")
  M7<-M7[,-1]
  M7<-data.frame(t(M7))
  colnames(M7)<- t(Nodes)
  M7$NonExceed<-(1-as.numeric(rownames(M7)))
  M7$Scenario<-"2085"
  
  ProbScenarios<- rbind(M0,M5,M6,M7)
  ProbScenarios<-ProbScenarios[,c(431,430,1:429)]
  rownames(ProbScenarios)<-NULL
  NodeNum<-which(colnames(ProbScenarios)==FindNode)
  
  graphdata<- ProbScenarios[, c(1,2,NodeNum )]
  graphdata$Scenario<-factor(graphdata$Scenario, levels= c("Baseline", "2030", "2050", "2085"))
  colnames(graphdata)[3]<-"NodeX"
  DataTable<- reshape(DetScenarios[,c(1:2,NodeNum)], idvar="NonExceed", timevar="Scenario", direction = "wide" )
  colnames(DataTable)<-c("Non-Exceedance Probability", "Baseline", "2030", "2050", "2085")
  DataTable[,2:6]<-round(DataTable[,2:6], 2)
}
tab<-tableGrob(DataTable, row=NULL, theme=ttheme_default(colhead=list(fg_params = list(parse=TRUE)),base_size = 10,padding = unit(c(1, 1), "mm")))
fig<-ggplot(graphdata, aes(NodeX, NonExceed, color=Scenario))+geom_line()+scale_x_continuous(breaks=seq(round(min(graphdata$NodeX),1),max(graphdata$NodeX), by=1))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Water Surface Elevation (ft)")+ ylab("Non-Exceedance Probability")+ ggtitle(paste0("Stage Frequency ",NodeName))
png(paste0("C:/Users/aschwarz/deltacouncil/Delta Adapts Project - Documents/Data and Modeling/Flooding Analysis/Data/FlowFrequency/", FindNode,"_",RunType, ".png"), width=480, height= 680)
print(grid.arrange(fig,tab, nrow = 2, heights=c(6, 6)))
dev.off()
}


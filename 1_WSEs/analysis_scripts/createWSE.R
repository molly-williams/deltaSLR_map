########This script allows the user to emulate the monte carlo simulation process for Delta Adapts
########using previously developed boundary conditions using.  In other words this script allows 
########the user to choose between any of the 3 inflow boundary conditions and ANY SLR level between
########0 and 10 feet, and to establish flow caps on any of the tributaries.  The code will output
########a file providing calculated Water Surface Elevations (WSE) at each of the 427 DSM2 nodes throughout
########the Delta for a number of runs set by the user. 
########Writen by Andrew Schwarz, Delta Stewardship Council
########Last Updated 10/16/2020

library(ggplot2)
library(readxl)
library(parallel)
library(iterators)
library(foreach)
library(doParallel)

source('1_WSEs/analysis_scripts/functions.R')

NumRuns<-100000 # User input: any number 1-300000, generally 100,000 is a good compromise between speed and quality and will give you stable CDFs

####Load Historical, 2030, 2050, and 2085 inflow Data (No user input needed) 

FlowsHist<-read.csv(file="1_WSEs/Data/Node_18_300000_SLR_0_MC_Deterministic_output.csv")
Flows2050<-read.csv(file="1_WSEs/Data/Node_18_300000_SLR_1_MC_Deterministic_output.csv")
Flows2085<-read.csv(file="1_WSEs/Data/Node_18_300000_SLR_3.5_MC_Deterministic_output.csv")
##Clean up data files#################
FlowsHist<-FlowsHist[,c(-1:-2, -12:-13)]
Flows2050<-Flows2050[,c(-1:-2, -12)]
Flows2085<-Flows2085[,c(-1:-2)]

##User Input: Apply a flow cap to Delta Tributary inflows to limit in channel flows
SJRCap<-NA #110000 from CVFPP 2017 HEC-RAS
SacCap<-NA #as.numeric(quantile(FlowsHist$QSac, .995))
MokCap<-NA #as.numeric(quantile(FlowsHist$QMok, .995))
CalCap<-NA #as.numeric(quantile(FlowsHist$QMisc, .995))
CosCap<-NA #as.numeric(quantile(FlowsHist$QCos, .995))
  
###load Coefficient Data received from AECOM (7/3/2020) No User Input needed.
RegCoeff<-loadCoeff("1_WSEs/Data/CCVA Regression Analysis Coefficients.csv")


### User input: Assign Scenarios you want to explore.
Scenarios<-c("Hist_0_SACcapped", "2050_1_SACcapped", "2050_2_SACcapped")### Format is [1]climate (Hist, 2050, 2085), [2]Slr amount [0,0.5,1,2,3,3.5,...], [3]river capped [e.g., "SJRcapped"]

for(sen in Scenarios){
  clim<- strsplit(sen, "_")[[1]][1]
  SLRamt<-as.numeric(strsplit(sen, "_")[[1]][2])
  if((SLRamt - floor(SLRamt))>0){##If SLR is not an integer, need to interpolate between coefficient values
            lowSLR<-floor(SLRamt)
            highSLR<-ceiling(SLRamt)
            lowCoeff<-RegCoeff[(1+lowSLR*429):(429+lowSLR*429),1:10]
            highCoeff<-RegCoeff[(1+highSLR*429):(429+highSLR*429),1:10]
            NewCoeff<-data.frame(lowCoeff[,1])
            NewCoeff$SLR<-SLRamt
      for(i in 1:429){
        for(j in 3:10){
            NewCoeff[i,j]<-(highCoeff[i,j]-lowCoeff[i,j])/(highSLR-lowSLR)*(SLRamt-lowSLR)+lowCoeff[i,j]
        }
     }
  }else{##If SLR is an integer, use coefficient for that level of SLR
    NewCoeff<-subset(RegCoeff, SLR==SLRamt)}
  NewCoeff<-as.matrix(t(NewCoeff[,3:10]))
  WSE<-data.frame(matrix(NA, nrow = NumRuns, ncol = 429))
  colnames(WSE)<- as.character(RegCoeff[1:429,1])

  if(clim=="Hist"){FlowData<-FlowsHist[1:NumRuns,]}
  if(clim=="2085"){FlowData<-Flows2085[1:NumRuns,]}
  if(clim=="2050"){FlowData<-Flows2050[1:NumRuns,]}
  
  #Set up a matrix of flows organized by associated RegCoeff 
  FlowDataOrg<-data.frame(matrix(nrow=NumRuns, ncol=8))
  FlowDataOrg[,1]<-FlowData[,9]
  FlowDataOrg[,2]<-SLRamt
  #Set Up Sac Flow Values
  if(!is.na(SacCap)){FlowData[which(FlowData[,6]>SacCap),6]=SacCap}
  FlowDataOrg[,3]<-FlowData[,6]^0.67
  #Set Up Yolo Flow Values
  FlowDataOrg[,4]<-FlowData[,1]^0.67
  #Set Up SJR Flow Values
  if(!is.na(SJRCap)){FlowData[which(FlowData[,5]>SJRCap),5]=SJRCap}
  FlowDataOrg[,5]<-FlowData[,5]^0.67
  #Set Up Cos Flow Values
  if(!is.na(CosCap)){FlowData[which(FlowData[,3]>CosCap),3]=CosCap}
  FlowDataOrg[,6]<-FlowData[,3]^0.67
  #Set Up Mok Flow Values
  if(!is.na(MokCap)){FlowData[which(FlowData[,4]>MokCap),4]=MokCap}
  FlowDataOrg[,7]<-FlowData[,4]^0.67
  #Set Up Misc/Calaveras Flow Values
  if(!is.na(CalCap)){FlowData[which(FlowData[,2]>CalCap),2]=CalCap}
  FlowDataOrg[,8]<-FlowData[,2]^0.67
  FlowDataOrg<-as.matrix(FlowDataOrg)
  
  for(j in 1:429){
    for(i in 1:NumRuns){
      WSE[i,j]<-round(FlowDataOrg[i,] %*% (NewCoeff[,j]),2)
    }
    print(paste(sen,j))
  }

####This file path will need to be updated by the user to the location they want the file saved to. Do NOT CHANGE File name after last slash.
write.csv(WSE, file=paste0("1_WSEs/Data/WSE/",sen, ".csv"), row.names = F)
}

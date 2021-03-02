
#makes a vector with names for the scenario uses in producing
#the levee output data set

scennam <- function(scname){
  loopnames <-c()
  for (i in 1:length(scname)){
    loopnames[2*i-1] <- sprintf("%sWSE", scname[i])
    loopnames[2*i] <- sprintf("%sOvr", scname[i])
  }
  return(loopnames)
}

#makes a vector with names for the scenario uses in producing
#the levee output data set in the determinisic scenarios


ffnam <- function(scnname){
  ffloop <- c()
    for (i in 1:length(scnname)){
      ffloop[i] <- sprintf("%sFF", scnname[i])
  }
  return(ffloop)
}

#Makes the names for the output polygon shapefils

PolyName <- function(scn, plynm){
  if(scn=="deterministic") {
  shapes <-c()
  for (i in 1:length(plynm)){
  shapes[i] <- sprintf("%sDet", plynm[i])
  }
  return(shapes)
  } else if (scn=="probabilistic") {
    shapes <-c()
    for (i in 1:length(plynm)){
      shapes[i] <- sprintf("%sProb", plynm[i])
    }
    return(shapes)
  }
  
}


#automates a lot of the script for the mapping done in Delta Adapts
#it will prep most of the variable for you based on our inputs

SelectScenario <- function(scn){
  if(scn=="deterministic"){
    scndet <- c("Base", "Hlf", "One", "Two", "3_5")
    floodName<- scennam(scndet)
    Fldfightvar <- ffnam(scndet)
    shapename <- PolyName(scndet)
    WSEs <- read.csv("data/modelout/M0M1hundredyear.csv")
    funlist <- list("floodname" = floodName, "Fldfightvar" = Fldfightvar, "shapename" = shapename, "WSEs" = WSEs)
    } else if(scn=="M0"){
      #this is lazy coding but I am including scndet to create a dummy Fldfightvar because it is a requirement for
      #overtopanalysis() sorry to anyone in the future who this confuses
    scndet <- c("a", "b", "c", "d", "e")
    floodName<- c("M00_5WSE", "M00_5Ovr","M01WSE", "M01Ovr", 
                  "M02WSE", "M02Ovr", "M010WSE", "M010Ovr")
    Fldfightvar <- ffnam(scndet)
    shapename <- c("M0200yrpoly", "M0100yrpoly", "M050yrpoly", "M010yrpoly")
    WSEs <- read.csv("data/modelout/M0Prob.csv")
    funlist <- list("floodname" = floodName, "Fldfightvar" = Fldfightvar, "shapename" = shapename, "WSEs" = WSEs)
    } else if(scn=="M5"){
      scndet <- c("a", "b", "c", "d", "e")
      floodName<- c("M50_5WSE", "M50_5Ovr","M51WSE", "M51Ovr", 
                    "M52WSE", "M52Ovr", "M510WSE", "M510Ovr")
      Fldfightvar <- ffnam(scndet)
      shapename <- c("M5200yrpoly", "M5100yrpoly", "M550yrpoly", "M510yrpoly")
      WSEs <- read.csv("data/modelout/M5Prob.csv")
      funlist <- list("floodname" = floodName, "Fldfightvar" = Fldfightvar, "shapename" = shapename, "WSEs" = WSEs)
    } else if(scn=="M6"){
      scndet <- c("a", "b", "c", "d", "e")
      floodName<- c("M60_5WSE", "M60_5Ovr","M61WSE", "M61Ovr", 
                    "M62WSE", "M62Ovr", "M610WSE", "M610Ovr")
      Fldfightvar <- ffnam(scndet)
      shapename <- c("M6200yrpoly", "M6100yrpoly", "M650yrpoly", "M610yrpoly")
      WSEs <- read.csv("data/modelout/M6Prob.csv")
      funlist <- list("floodname" = floodName, "Fldfightvar" = Fldfightvar, "shapename" = shapename, "WSEs" = WSEs)
    } else if(scn=="M7"){
      scndet <- c("a", "b", "c", "d", "e")
      floodName<- c("M70_5WSE", "M70_5Ovr","M71WSE", "M71Ovr", 
                    "M72WSE", "M72Ovr", "M710WSE", "M710Ovr")
      Fldfightvar <- ffnam(scndet)
      shapename <- c("M7200yrpoly", "M7100yrpoly", "M750yrpoly", "M710yrpoly")
      WSEs <- read.csv("data/modelout/M7Prob.csv")
      funlist <- list("floodname" = floodName, "Fldfightvar" = Fldfightvar, "shapename" = shapename, "WSEs" = WSEs)
    } else {
      print("You must give the function SelectScenario() the argument: deterministic, M0, M5, M6, or M7")
    }
  
  return(funlist)
}


#A minor transformation of the levee data set for some downstream
#processes
LevProcess <- function(Lpath){
  print("loading in the levee data")
  
  LevLines <- shapefile(Lpath)
  NodesNamed <- c()
  
  for (i in 1:length(LevLines$DSMNode)){
   NodesNamed[i] <- sprintf("Node_%s", LevLines$DSMNode[i])
  
  }

  LevLines$StringNodes <- NodesNamed
  print("levee data loaded, it will produce a warning about SpatialRef 
        no matter what")
  
  return(LevLines)
}

#Identifies overtopped levee segments, takes wSE the levee
#shapefile and naming vectors as arguments

LevOver <- function(LevLines, floodNames, hundredyr, Fldfightnms, deterministic){
  print("processing levee overtopping")
  
  if(deterministic == TRUE){
    WSE <- c()
    over <- c()
    WSELevDif <- c()


    runl <- length(floodNames)/2

    for (q in 1:runl){
  
      flood100 <- hundredyr[,q+1]
  
      for (i in 1:length(LevLines$DSMNode)){
        WSE[i] <- flood100[LevLines$StringNodes[i]==hundredyr[,1]]
        if (WSE[i]>LevLines$Z_Mean_ft[i]){
          over[i] <- 1
        } else {
        over[i] <- 0
        }
        WSELevDif[i] <- WSE[i] - LevLines$Z_Mean_ft[i]
      }
      LevLines@data <- cbind(LevLines@data, WSE)
      names(LevLines@data)[length(names(LevLines@data))] <- floodNames[2*q-1]
      LevLines@data <- cbind(LevLines@data, over)
      names(LevLines@data)[length(names(LevLines@data))] <- floodNames[2*q]
      LevLines@data <- cbind(LevLines@data, WSELevDif)
      names(LevLines@data)[length(names(LevLines@data))] <- Fldfightnms[q]
    }
    print("deterministic levee overtopping data proccessed")
   }else if(deterministic == FALSE){
    WSE <- c()
    over <- c()
    
    
    runl <- length(floodNames)/2
    
    for (q in 1:runl){
      
      flood100 <- hundredyr[,q+1]
      
      for (i in 1:length(LevLines$DSMNode)){
        WSE[i] <- flood100[LevLines$StringNodes[i]==hundredyr[,1]]
        if (WSE[i]>LevLines$Z_Mean_ft[i]){
          over[i] <- 1
        } else {
          over[i] <- 0
        }
      }
      LevLines@data <- cbind(LevLines@data, WSE)
      names(LevLines@data)[length(names(LevLines@data))] <- floodNames[2*q-1]
      LevLines@data <- cbind(LevLines@data, over)
      names(LevLines@data)[length(names(LevLines@data))] <- floodNames[2*q]
    } 
    print("probabilistic levee overtopping data proccessed")
    
    } else { 
      
      print("You most give TRUE or FALSE to the deterministic argument")}

  return(LevLines)
}
    

#Takes the output levee data set and produces polygons

overtopanalysis <- function(LevLines, shapenam, floodNames, Fldfightnms, deterministic, polytype){
  
  if(polytype=="simplified"){
    
  print("loading in simplified polygons this is the longest part of the process, it may take a few minutes")
  varname <-c()
  
  for (i in 1:100){
    varname[i] <- sprintf("islands%ift", i)
    
  }
  
  for (i in 1:100){
    if (i <10){
      temppath <- sprintf("2_OvertopAnalysis/data/200924_simplified/inundation_poly_clip_0%i.shp", i)
      assign(varname[i], shapefile(temppath))
    } else {
      temppath <- sprintf("2_OvertopAnalysis/data/200924_simplified/inundation_poly_clip_%i.shp", i)
      assign(varname[i], shapefile(temppath))
    }
  }
  print("polygons loaded")
  } else if(polytype=="unsimplified"){
    
    print("loading in unsimplified polygons this is the longest part of the process, it may take a few minutes")
    varname <-c()
    
    for (i in 1:100){
      varname[i] <- sprintf("islands%ift", i)
      
    }
    
    for (i in 1:100){
      if (i <10){
        temppath <- sprintf("2_OvertopAnalysis/data/200924_unsimplified/inundation_poly_clip_0%i.shp", i)
        assign(varname[i], shapefile(temppath))
      } else {
        temppath <- sprintf("2_OvertopAnalysis/data/200924_unsimplified/inundation_poly_clip_%i.shp", i)
        assign(varname[i], shapefile(temppath))
      }
    }
    
    print("polygons loaded")
    #added this so that a the shapenames will indicate they are unsimplified if
    #the usr chooses that
    for (i in 1:length(shapenam)){
      shapenam[i] <- sprintf("unsimp%s", shapenam[i])
    }
    
  } else {
    print("You must give the polytype argument the value either simplified or unsimplified.")
  }
  
  
  if(deterministic==TRUE){  
    for(q in 1:length(shapenam)){
      
      LevOver <- LevLines[LevLines@data[,floodNames[2*q]]==1,]
      FloodIslands <- unique(LevOver$DLIS_Is)
      FloodIslands <- na.omit(FloodIslands)
      hunWSE <- LevOver@data[,floodNames[2*q-1]]
      LevWSEdif <- LevOver@data[,Fldfightnms[q]]
      
      print("identfying which islands flood")
      
      IntWSE <- c()
      Fldfi <- c()
        for (i in 1:length(FloodIslands)){
          IntWSE[i] <- round((max(na.omit(hunWSE[LevOver$DLIS_Is==FloodIslands[i]]))),0)
          highWSE <- max(na.omit(LevWSEdif[LevOver$DLIS_Is==FloodIslands[i]]))
          if ((highWSE < 0.5) && (highWSE >0)){
            Fldfi[i] <- 1
          } else {
            Fldfi[i] <- 0
          }
        }
      
        IntWSE[IntWSE>131] <- 131
      
        WSEscenarios <- unique(IntWSE)
      
        floodedpoly<-SpatialPolygons(list())
        crs(floodedpoly)<-crs(islands17ft)
      
        print ("identifying flood polgyons by WSE")
      
        for (i in 1:length(WSEscenarios)){
          Isles <- FloodIslands[IntWSE==WSEscenarios[i]]
          FloodArea <- eval(parse(text = sprintf("islands%ift", WSEscenarios[i])))
          subFloodArea<- FloodArea[FloodArea$NAME%in%Isles,]
          floodedpoly <- bind(floodedpoly, subFloodArea)
        }
      
        #append flood fighting to the polygon layers
        fldfight <-c()
        for (i in 1:length(floodedpoly)){
          gg <- which(FloodIslands == floodedpoly$NAME[i])
          fldfight[i] <- Fldfi[gg]
        }
      
        floodedpoly$fldfight <- fldfight
      
        #Write flooded Islands polygon layer
      
        writeOGR(floodedpoly, dsn = "2_OvertopAnalysis/output/FloodPoly" , layer = shapenam[q], driver="ESRI Shapefile")
      
        print(sprintf("wrote polygons named %s", shapenam[q]))
        
    }
  } else if(deterministic==FALSE){
    
    for(q in 1:length(shapenam)){
      
      LevOver <- LevLines[LevLines@data[,floodNames[2*q]]==1,]
      FloodIslands <- unique(LevOver$DLIS_Is)
      FloodIslands <- na.omit(FloodIslands)
      hunWSE <- LevOver@data[,floodNames[2*q-1]]
      
      print("identfying which islands flood")
      
      IntWSE <- c()
      for (i in 1:length(FloodIslands)){
        IntWSE[i] <- round((max(na.omit(hunWSE[LevOver$DLIS_Is==FloodIslands[i]]))),0)
      }
      
      IntWSE[IntWSE>131] <- 131
      
      WSEscenarios <- unique(IntWSE)
      
      floodedpoly<-SpatialPolygons(list())
      crs(floodedpoly)<-crs(islands17ft)
      
      print ("identifying flood polgyons by WSE")
      
      for (i in 1:length(WSEscenarios)){
        Isles <- FloodIslands[IntWSE==WSEscenarios[i]]
        FloodArea <- eval(parse(text = sprintf("islands%ift", WSEscenarios[i])))
        subFloodArea<- FloodArea[FloodArea$NAME%in%Isles,]
        floodedpoly <- bind(floodedpoly, subFloodArea)
      }
      
      
      #Write flooded Islands polygon layer
      
      writeOGR(floodedpoly, dsn = "2_OvertopAnalysis/output/FloodPoly" , layer = shapenam[q], driver="ESRI Shapefile")
      
      print(sprintf("wrote polygons named %s", shapenam[q]))
      
    }
    
  } else {
    
    print("You must provide a TRUE or FALSE value to the deterministic argument")
  }
}  


###load Coefficient Data received from AECOM (7/3/2020)
loadCoeff<-function(file){
  RegCoeffRaw<-read.csv(file.path(file), skip=1)
  colnames(RegCoeffRaw)[1]<- "Node"
  RegCoeff<- data.frame("Node"=rep(RegCoeffRaw$Node, 9))
  RegCoeff$SLR<- c(rep(0, 429), rep(1, 429), rep(2, 429), rep(3, 429), rep(4, 429), rep(5, 429), rep(6, 429), rep(7, 429), rep(10, 429))
  for(i in 0:8){
   RegCoeff[(i*429+1):(i*429+429), 3:10]<- RegCoeffRaw[1:429, (i*8+2):(i*8+9)]
  }
  colnames(RegCoeff)[3:10]<-c("a", "b", "c", "d", "e", "f", "g", "h")
  return(RegCoeff)
}

#########################################################################
## Author:    René van Dijk
## Creation:  9-10-2019
## Purpose:   This functions filters the data of 1 WAP
##            records without signal (0) for that WAP are excluded
##            WAP-columns without signal are excluded
#########################################################################

ExtractData1WAP <- function(DataSignals, WAP){
  #WAP="WAP070"
  #DataSignals <- DataAllBuildings
  # Extract ColumnID
  ColID <- which(colnames(DataAllBuildings)==WAP)
  
  #Filter where there is a signal for WAP (not 0)   
  DataSignals <- DataSignals %>% filter(DataSignals[,ColID]!=0) 

  #remove all columns without fingerprint signal (signal=100)
  LoopCol =ncol(DataSignals)
  
  while(LoopCol>0) {
    #if columname start with WAP... then change scale
    if(substr(colnames(DataSignals[LoopCol]),1,3)=="WAP") {
      SignalMax <- as.numeric(DataSignals %>% summarise(signal= max(DataSignals[,LoopCol])))
      
      #remove column if minimum signal strength = 0 (no signal)
      if(SignalMax==0) {
        DataSignals <- DataSignals[-LoopCol] 
      }
      
    }
    LoopCol=LoopCol-1
  }
  return(DataSignals)
  
}
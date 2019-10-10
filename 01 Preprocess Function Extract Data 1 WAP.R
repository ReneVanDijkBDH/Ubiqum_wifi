#########################################################################
## Author:    René van Dijk
## Creation:  9-10-2019
## Purpose:   This functions filters the data of 1 WAP
##            records without signal (0) for that WAP are excluded
##            WAP-columns without signal are excluded
#########################################################################

ExtractData1WAP <- function(DataSignals, WAP, MaxOnly){
  DataSignals<-training
  WAP="WAP158"
  MaxOnly<-0
  
  
  # Extract ColumnID of WAP
  ColID <- which(colnames(DataSignals)==WAP)
  
  # filter records  
 # ifelse(MaxOnly==0,
    #Filter every record where there is a signal for WAP (not 0)   
    DataSignals <- DataSignals %>% filter(DataSignals[,ColID]!=0)#,
    #Filter only records whith maxwap =wap
#  DataSignals <- DataSignals %>% filter(MaxWap==WAP))
    

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
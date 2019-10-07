ApplyMinimumSignal <- function(SignalData, MinSignal){
#SignalData <- DataBuilding0
#MinSignal <- 20

LoopCol =1

while(LoopCol< ncol(SignalData)) {
  #if columname start with WAP... then add to vertical dataframe
  if(substr(colnames(SignalData[LoopCol]),1,3)=="WAP") {
    
    SignalData[,LoopCol] <- with(SignalData,
                                   ifelse(SignalData[,LoopCol]< MinSignal,
                                          0,
                                          SignalData[,LoopCol]))
  }
  
  LoopCol=LoopCol+1
  
}
return(SignalData)
}
#########################################################################
## Author:    René van Dijk
## Creation:  4-10-2019
## Purpose:   This functions re-scales the RSSI-values in dbm to a better
##            usefull scale
##            current dbm           converted value
##               0 dbm (best)       105
##               :                  :
##            -104 dbm (poor)       1
##             100 dmb (no signal)  0
#########################################################################

RescaleRSSI <- function(DataBuilding){
  # loop through all columns
  
  
  LoopCol =ncol(DataBuilding)

  while(LoopCol>0) {
    #if columname start with WAP... then change scale
    if(substr(colnames(DataBuilding[LoopCol]),1,3)=="WAP") {
      DataBuilding[,LoopCol] <- with(DataBuilding,
                                        ifelse(DataBuilding[,LoopCol]==100,
                                               0,
                                               DataBuilding[,LoopCol]+105))
    }
    LoopCol=LoopCol-1
  }
  return(DataBuilding)
}
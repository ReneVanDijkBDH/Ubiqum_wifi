#########################################################################
## Author:    René van Dijk
## Creation:  4-10-2019
## Purpose:   This functions devides the dataset of 1 building into
##            a smaller dataset where unusefull columns are omitted 
#########################################################################

ExtractData1FloorSpace <- function(Data1Building, FloorID, Spacelist){

  #Filter data for required floor and spaces  
  DataFloorSpace <- Data1Building %>% 
                      filter(FLOOR==FloorID & SPACEID %in% Spacelist) 

  #remove all columns without fingerprint signal (signal=100)
  # loop through all WAP columns - 9 identifiers
  LoopCol =ncol(DataFloorSpace)-9

  while(LoopCol>0) {
    #calculate minimum signal strenght of the column
    SignalMin <- as.numeric(DataFloorSpace %>% summarise(signal= min(DataFloorSpace[LoopCol])))
  
    #remove column if minimum signal strength = 100 (no signal)
    if(SignalMin==100) {
      DataFloorSpace <- DataFloorSpace[-LoopCol] 
    }
    LoopCol=LoopCol-1
  }
  return(DataFloorSpace)
}
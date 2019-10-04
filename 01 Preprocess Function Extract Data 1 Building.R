#########################################################################
## Author:    René van Dijk
## Creation:  3-10-2019
## Purpose:   This functions devides the dataset of all buildings into
##            a smaller dataset where unusefull columns are omitted 
#########################################################################

ExtractData1Building <- function(DataAllBuildings, BuildingID){

  #Filter data for required building  
  DataBuilding <- DataAllBuildings %>% filter(BUILDINGID==BuildingID) 

  #remove all columns without fingerprint signal (signal=100)
  # loop through all 520 columns
  LoopCol =520

  while(LoopCol>0) {
    #calculate minimum signal strenght of the column
    SignalMin <- as.numeric(DataBuilding %>% summarise(signal= min(DataBuilding[LoopCol])))
  
    #remove column if minimum signal strength = 100 (no signal)
    if(SignalMin==100) {
      DataBuilding <- DataBuilding[-LoopCol] 
    }
    LoopCol=LoopCol-1
  }
  return(DataBuilding)
}
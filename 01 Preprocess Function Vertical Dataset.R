#########################################################################
## Author:    René van Dijk
## Creation:  10-10-2019
## Purpose:   convert dataframe to have all the WAP-signals vertically in 1 column.
##            There current dataframe has 2 different parts:
##            1) fixed attributes (user, phone etc) which should be in every record
##            2) 520 WAP's which should be converted from horizontal to vertical
#########################################################################
ConvertToVerticalData <- function(HData) {

  # Fix attributes that should be in every record
  # All except WAP-columns
  FixedAttributes <- HData %>% select(ObservationID,LONGITUDE,LATITUDE,FLOOR,BUILDINGID,
                                    SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP)

  #loop through all columns of the dataset
  LoopCol =1

  while(LoopCol< ncol(HData)) {
    #if columname start with WAP... then add to vertical dataframe
    if(substr(colnames(HData[LoopCol]),1,3)=="WAP") {
      print(colnames(HData[LoopCol]))
      tempData <- FixedAttributes
      tempData$WAP <- colnames(HData[LoopCol])
      tempData$WAPSignal <-  HData[,LoopCol]
      
      #remove all records without signal
      tempdata <- tempData %>% filter(WAPSignal>0)
      
      #rank data based on max signal
      #arrange(tempData,-WAPSignal)
      #tempData$Rank <- seq_len(nrow(tempData))
      
      #add to total dataset
      ifelse(LoopCol==1,
            VData <- tempData,
            VData <- rbind(VData, tempData))
      
    }
    LoopCol=LoopCol+1
  }
  return(VData)
}

  

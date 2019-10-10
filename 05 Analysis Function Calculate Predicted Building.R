#########################################################################
## Author:    René van Dijk
## Creation:  10-10-2019
## Purpose:   calculates the expected building based on predicted Longitude & latitude:
##            1) Add predicted Longitude and Latitude to testing set
##            2) Calculate errors compared to actual values
##            3) Calculate expected building: based on predicted Long & Lat.
##               uses a (linear) formula to determine location of buildings
##            4) Calculate error of predicted building vs actual building
#########################################################################

CalculatePredictedBuilding <- function(testing, PredictLong, PredictLat){
  
  CompareResults                  <- testing
  CompareResults$PredictLong      <- PredictLong
  CompareResults$PredictLat       <- PredictLat
  CompareResults$LongError        <- with(CompareResults,LONGITUDE - PredictLong)
  CompareResults$LatError         <- with(CompareResults,LATITUDE - PredictLat)
  CompareResults$PredictBuilding  <- with(CompareResults,
                                         ifelse(PredictLat > 4876350 + PredictLong*1.506,
                                                0,
                                                ifelse(PredictLat >4876000 + PredictLong* 1.506,
                                                       1,
                                                       2)
                                                )
                                         )
  CompareResults$BuildingError    <- with(CompareResults, 
                                          ifelse(BUILDINGID==PredictBuilding,0,1)) 
  return(CompareResults)
}
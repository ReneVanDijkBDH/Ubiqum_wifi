CalculatePredictedBuilding <- function(testing, PredictLong, PredictLat){
  CompareResults <- testing
  CompareResults$PredictLong <- PredictLong
  CompareResults$PredictLat <- PredictLat
  CompareResults$LongError <- with(CompareResults,LONGITUDE - PredictLong)
  CompareResults$LatError <- with(CompareResults,LATITUDE - PredictLat)
  
  CompareResults$PredictBuilding <- with(CompareResults,
                                         ifelse(PredictLat > 4876350 + PredictLong*1.506,
                                                0,
                                                ifelse(PredictLat >4876000 + PredictLong* 1.506,
                                                       1,
                                                       2)
                                         )
  )
  CompareResults$BuildingError <- with(CompareResults, ifelse(BUILDINGID==PredictBuilding,0,1)) 
  return(CompareResults)
}
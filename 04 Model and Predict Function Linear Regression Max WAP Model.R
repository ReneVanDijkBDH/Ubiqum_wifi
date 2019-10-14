LinearRegressionMaxWAPModel <- function(training, testing){

  testing <- DataProcessing
  
  
# List WAP's in test and train-set which are a maximum (and need to be trained)
TestingMaxWAP   <-  testing %>% 
  group_by(MaxWap) %>% 
  summarise(TimesTest = n())
TrainingMaxWAP  <-  training %>% 
  group_by(MaxWap) %>% 
  summarise(TimesTrain = n())
# add them together
TestingMaxWAP   <-  TestingMaxWAP %>% 
  left_join(TrainingMaxWap,"MaxWap")

# loop through all maximum WAP's of testing set
LoopRow <- 1
while(LoopRow<=nrow(TestingMaxWAP)) {
#while(LoopRow<=130) {
    # set WAP for which to do modelling and predicting
  WAP = as.character(TestingMaxWAP[LoopRow,1])
  print(WAP)
  
  # reduce trainingset based on current WAP
  Training1WAP <- ExtractData1WAP(training, WAP,0)
  if(nrow(Training1WAP)>0){
  
  # reduce testing set based on current WAP and structure training set
  TrainingAttributes <-colnames(Training1WAP)
  Testing1WAP <- FilterTestingForWAP(testing, TrainingAttributes,WAP)
  print(WAP)
  #apply linear model
  RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                       SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP )
  RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                      SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP)
  print(WAP)
  # predict values for test set
  RegPredictLong <- predict(RegModelLong,Testing1WAP)
  RegPredictLat <- predict(RegModelLat,Testing1WAP)  
  Testing1WAPBuilding <- CalculatePredictedBuilding(Testing1WAP, RegPredictLong, RegPredictLat)
  Testing1WAPSelection <- Testing1WAPBuilding %>% 
    select(ObservationID, PredictLong, PredictLat, LongError, LatError,
           PredictBuilding, BuildingError)
  ifelse(LoopRow==1,
         TestingAllWAP <- Testing1WAPSelection,
         TestingAllWAP <- rbind(TestingAllWAP, Testing1WAPSelection))
  }
  LoopRow=LoopRow+1
}
TestingResult <- testing %>% left_join(TestingAllWAP,"ObservationID")

return(TestingResult)
}
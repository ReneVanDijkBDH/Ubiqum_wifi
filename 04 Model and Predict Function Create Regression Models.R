CreateRegressionModels <- function(trainingData){

# create empty list for storing models
ModelList <- vector(mode="list", length=520)

# list all required models based on Maximum WAP
WAPList <- distinct(trainingData,MaxWap)
  
  
  
# loop through all required models
loopmodel <- 1
while(loopmodel<= nrow(WAPList)){
  
  WAP = as.character(WAPList[loopmodel,1])
  WAPNr = as.numeric(substr(WAP,4,6))
  print(WAP)
  
  # reduce trainingset based on current WAP
  Training1WAP <- ExtractData1WAP(trainingData, WAP,0)
  if(nrow(Training1WAP)>0){
    
    # reduce testing set based on current WAP and structure training set
    TrainingAttributes <-colnames(Training1WAP)
    #Testing1WAP <- FilterTestingForWAP(testing, TrainingAttributes,WAP)
    
  #  RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID -
  #                       SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP - 
  #                       - ObservationID - WAPSignal - ranking, Training1WAP )    
  #  
  #  RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - 
  #                      SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP- 
  #                      ranking - WAPSignal, Training1WAP)
    
    #apply linear model
    RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID -
                         SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP - 
                         - ObservationID - MaxWap - WAPSignal - ranking, Training1WAP )
    RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                        SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP- 
                        ranking - WAPSignal, Training1WAP)
    
    
    
    #set.seed(456)
    #trctrl <- trainControl(method="repeatedcv",repeats = 1) 
    #KNNModelLong <- train(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID, 
    #                      data = training, 
    #                      method = "knn", 
    #                      trControl = trctrl, 
    #                      preProcess = c("center","scale"), 
    #                      tuneLength = 10)
    
    #KNNModelLat <- train(LATITUDE ~ . , 
    #                     data = Training1WAP, 
    #                     method = "knn", 
    #                     trControl = trctrl, 
    #                     preProcess = c("center","scale"), 
    #                     tuneLength = 10)
  #Training1WAP <- Training1WAP %>% select(WAP496,WAP061, WAP012,WAP070,WAP077, WAP117,WAP249, WAP332, FLOOR)
    #Training1WAP <- Training1WAP[,-nearZeroVar(Training1WAP)]
    
    #KNNModelFloor <- train(FLOOR ~ .- LATITUDE - LONGITUDE  -
    #                         SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP - 
    #                         - ObservationID - MaxWap - WAPSignal - ranking , 
    #                     data = Training1WAP, 
    #                     method = "knn", 
    #                     trControl = trctrl, 
    #                     preProcess = c("center","scale"), 
    #                     tuneLength = 10)
    
    
    
    # store models  
    WAP1models =list(TrainingAttributes, RegModelLong, RegModelLat)
    ModelList[[WAPNr]] <- WAP1models
  }
  loopmodel=loopmodel+1
}

return(ModelList)
}
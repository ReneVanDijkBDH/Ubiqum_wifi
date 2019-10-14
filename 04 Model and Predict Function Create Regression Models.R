CreateRegressionModels <- function(trainingData){}

# create empty list for storing models
ModelList <- vector(mode="list", length=520)

# list all required models based on Maximum WAP
WAPList <- distinct(trainingData,MaxWap)
  
  
  
# loop through all required models
loopmodel <- 1
while(loopmodel<= nrow(WAPList)){
  
  WAP = as.character(WAPList[LoopRow,1])
  WAPNr = substr(WAP,4,6)
  print(WAP)
  
  # reduce trainingset based on current WAP
  Training1WAP <- ExtractData1WAP(trainingData, WAP,0)
  if(nrow(Training1WAP)>0){
    
    # reduce testing set based on current WAP and structure training set
    TrainingAttributes <-colnames(Training1WAP)
    #Testing1WAP <- FilterTestingForWAP(testing, TrainingAttributes,WAP)
    
    #apply linear model
    RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                         SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP )
    RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                        SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP)
    
  
    # store models  
    WAP1models[[]]=(TrainingAttributes, RegModelLong, RegModelLat)
    ModelList[[WAPNr]] <- WAP1models
}

return(ModelList)
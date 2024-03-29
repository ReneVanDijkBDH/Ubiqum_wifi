CreateFloorModel <-function(trainingFloor, ModelList, Building, ModelType){
  #trainingFloor <- training  
  #Building <- 0

  
  #set modelling parameters
  if(ModelType=="KNN") {
    MinTrainObs <- 10
    trctrl <- trainControl(method="repeatedcv",repeats = 1) 
  } else if (ModelType=="SVM") {
    MinTrainObs <- 40
    trctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)
    grid <- expand.grid(C = c(0.1, 1, 2.5, 5))
  } else if (ModelType=="RF") {
    MinTrainObs <- 100
    trctrl <- trainControl(method="repeatedcv",repeats = 1) 
  }

  # loop through all WAP's and create a model
  WAPNr <- 1
  while (WAPNr<=520){
    WAPCol <- colnames(trainingFloor[WAPNr])
    print(WAPCol)
  
    #select data for specific WAP
    WAPData           <- trainingFloor %>% 
                            filter(BUILDINGID==Building & trainingFloor[,WAPNr] >0) %>% 
                            select(ObservationID,FLOOR,LONGITUDE,LATITUDE, WAPCol)
    WAPData$FLOOR     <- as.factor(WAPData$FLOOR)
    names(WAPData)[5] <- "WAPSignal"
  
    #create model when enough records in selected data
    ModelFloor <- NULL
    if(nrow(WAPData)>MinTrainObs){
      set.seed(456)
      if(ModelType=="KNN") {
        ModelFloor <- train(FLOOR ~ . - ObservationID , 
                            data = WAPData, 
                            method = "knn", 
                            trControl = trctrl, 
                            preProcess = c("center","scale"), 
                            tuneGrid = expand.grid(k = c(3,5,7,9)),
                            tuneLength = 10)
      } else if (ModelType=="SVM") {    
        ModelFloor <- train(FLOOR ~ . - ObservationID ,  
                            data = WAPData, 
                            method = "svmLinear",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            #tuneGrid = grid,
                            tuneLength = 10)
      } else if (ModelType=="RF") {
        RFModel <- train(FLOOR ~ . - ObservationID, 
                         data = WAPData, 
                         method = "rf", 
                         trControl = trctrl, 
                         preProcess = c("center","scale"), 
                         tuneLength = 10)
      }
      #store resulting model in ModelList
      if(!is.na(ModelList)){
        ModelList[[WAPNr]] <- ModelFloor
      }
    }
    WAPNr = WAPNr +1
  }
return(ModelList)
}


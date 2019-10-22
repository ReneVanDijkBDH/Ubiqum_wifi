CreateFloorModelWAP <-function(VDataExt, ModelList,  ModelType){
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
    if(WAPNr<10) {
       WAPCol <- paste("WAP00", WAPNr,sep="")
    } else if (WAPNr<100) {
      WAPCol <- paste("WAP0", as.character(WAPNr), sep="")
    } else {
      WAPCol <- paste("WAP", as.character(WAPNr), sep="")
    }
    print(WAPCol)
  
    #select data for specific WAP
    WAPData           <- VDataExt %>% 
                            filter(WAPSignal >0 & WAP==WAPCol) %>% 
                            select(ObservationID,WAP,FLOOR,DistanceBucket, SignalBucket, Quadrant)
    WAPData$FLOOR     <- as.factor(WAPData$FLOOR)
    
    #create model when enough records in selected data
    ModelFloor <- NULL
    if(nrow(WAPData)>MinTrainObs){
      set.seed(456)
      if(ModelType=="KNN") {
        ModelFloor <- train(FLOOR ~ . - ObservationID -WAP , 
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


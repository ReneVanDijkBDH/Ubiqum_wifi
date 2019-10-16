CreateFloorModel <-function(trainingFloor, ModelList){
#trainingFloor <- training 

Building <- 0
MinTrainObs <- 10
trctrl <- trainControl(method="repeatedcv",repeats = 1) 
while(Building<=2){
  WAPNr <- 1
while (WAPNr<=520){
  WAPCol <- colnames(trainingFloor[WAPNr])
  print(WAPCol)
  WAPData <- trainingFloor %>% 
             filter(BUILDINGID==Building & trainingFloor[,WAPNr] >0) %>% 
              select(ObservationID,FLOOR, WAPCol)
  WAPData$FLOOR <- as.factor(WAPData$FLOOR)
  names(WAPData)[3] <- "WAPSignal"
  
  KNNModelFloor <-NULL
  if(nrow(WAPData)>MinTrainObs){
  set.seed(456)
  KNNModelFloor <- train(FLOOR ~ . - ObservationID , 
                             data = WAPData, 
                             method = "knn", 
                             trControl = trctrl, 
                             preProcess = c("center","scale"), 
                             tuneGrid = expand.grid(k = c(3,5,7,9)),
                             tuneLength = 10)
  
  #tempData2 <- testing %>% filter(BUILDINGID==Building & testing[,WAPNr] >0) %>% 
  #                        select(ObservationID,FLOOR, WAPCol)
  #tempData2$FLOOR <- as.factor(tempData2$FLOOR)
  
  #KNNPredictFloorTemp <- predict(KNNModelFloor, newdata = tempData2)
  #postResample(tempData2$FLOOR,KNNPredictFloorTemp)
  }
  
  tempModel <-ModelList[[WAPNr]]
  tempModel[[Building+1]] <- KNNModelFloor
  ModelList[[WAPNr]] <- tempModel
  
  WAPNr = WAPNr +1
}
Building = Building + 1
}
return(ModelList)
}

#####################################
#B0 <- training %>% filter(BUILDINGID==0)
#B0 <- B0[,-nearZeroVar(B0)]
#B0$FLOOR <- as.factor(B0$FLOOR)

# set.seed(456)
#trctrl <- trainControl(method="repeatedcv",repeats = 1) 

#B0Select <- B0 %>% select(WAP500,WAP404,WAP388,WAP380,WAP325, FLOOR)

#KNNModelFloor <- train(FLOOR ~ . , 
#                       data = B0Select, 
#                       method = "knn", 
#                       trControl = trctrl, 
#                       preProcess = c("center","scale"), 
#                       tuneLength = 10)

#KNN prediction & results
#KNNPredictFloor <- predict(KNNModelFloor, newdata = B0Testing)

#B0Testing <- testing %>% filter(BUILDINGID==0 & MaxWap!="WAP248")

#postResample(B0Testing$FLOOR,KNNPredictFloor)




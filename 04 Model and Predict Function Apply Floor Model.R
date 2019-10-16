ModelList3 <- ModelList2
TestFloorVert <- testingVert

# Rank vertical

TestFloorVert <- TestFloorVert  %>%
  group_by(ObservationID) %>%
  mutate(ranking = order(order(WAPSignal, decreasing=TRUE)))

# select top X
TopX <- 5
TestFloorVert <- TestFloorVert %>% filter(ranking<=TopX)

# Haal Predicted Building erbij

# distinct list WAP & Building
TestTopWAP   <-  TestFloorVert %>% 
  filter(WAPSignal>0) %>%
  group_by(BUILDINGID, WAP) %>% 
  summarise(TimesTest = n())


# loop throug distinct list 
loopWAP <- 1
while (loopWAP<nrow(TestTopWAP)) {
  Building <- as.numeric(TestTopWAP[loopWAP,1])
  WAPName <- as.character(TestTopWAP[loopWAP,2])
  WAPNr <- as.numeric(substr(WAPName,4,6))
  
  # select relevant records
  tempset <- TestFloorVert %>% 
    filter(BUILDINGID==Building & WAP==WAPName) %>%
    select(ObservationID, FLOOR, WAPSignal ) 
  tempset$FLOOR <- as.factor(tempset$FLOOR)
  
  # rename column to be identical to train model
  #names(tempset)[3] <- WAPName
  
  # get model
  tempmodel <- ModelList3[[WAPNr]]
  tempmodel <- tempmodel[[4+Building]]
  
  # predict floor
  if(!is.null(tempmodel)){

  FloorPredict <- predict(tempmodel,tempset)
  
  # add prediction to dataset
  tempset$PredictFloor <- FloorPredict 
  ifelse(loopWAP==1,
         tempAllVert <- tempset,
         tempAllVert <- rbind(tempAllVert, tempset))
  }
  loopWAP = loopWAP + 1 
}


 # predict floor

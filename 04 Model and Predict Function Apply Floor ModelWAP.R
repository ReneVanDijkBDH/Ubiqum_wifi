
#TestFloorVert <- TestFloorVert  %>%
#  group_by(ObservationID) %>%
#  mutate(ranking = order(order(WAPSignal, decreasing=TRUE)))
#TestFloorVert <- TestingVertTop10Ext
#TestFloorVert <- ValidationVertTop10Ext

ApplyFloorModelWAP <-function(TestFloorVert, WAPList){

# select top X WAP's for identifying f
TopX <- 5
TestFloorVert <- TestFloorVert %>% filter(ranking<=TopX)
TestFloorVert <- TestFloorVert %>% left_join(WAPList, "WAP")
TestFloorVert$Distance <- with(TestFloorVert,sqrt((Long_Max-PredictLong)^2+(Lat_Max-PredictLat)^2))
TestFloorVert$Quadrant <- with(TestFloorVert, ifelse(Long_Max>=LONGITUDE, 
                                           ifelse(Lat_Max>=LATITUDE,1,2),
                                           ifelse(Lat_Max>=LATITUDE,4,3)))
TestFloorVert$Quadrant <- as.factor(TestFloorVert$Quadrant)

# Haal Predicted Building erbij

#Building <- 0
#while(Building<=2){
# distinct list WAP & Building
TestTopWAP   <-  TestFloorVert %>% 
  filter(WAPSignal>0 ) %>%
  group_by( WAP) %>% 
  summarise(TimesTest = n()) %>%
  ungroup()

#if(Building==0){
#  FloorModel <- readRDS('../Data/clean_data/FloorModelB0.rds')
#} else if(Building==1){
#  FloorModel <- readRDS('../Data/clean_data/FloorModelB1.rds')
#} else if(Building==2){
#  FloorModel <- readRDS('../Data/clean_data/FloorModelB2.rds')
#}

FloorModel <- readRDS('../Data/clean_data/FloorModelWAPKNN.rds')
#FloorModel <- readRDS('../Data/clean_data/FloorModelWAPRF.rds')

# loop throug distinct list 
loopWAP <- 1
while (loopWAP<nrow(TestTopWAP)) {
  #Building <- as.numeric(TestTopWAP[loopWAP,1])
  WAPName <- as.character(TestTopWAP[loopWAP,1])
  WAPNr <- as.numeric(substr(WAPName,4,6))
  
  print(WAPName)
  # select relevant records
  tempset <- TestFloorVert %>% 
    filter(WAP==WAPName ) %>%
    select(ObservationID,FLOOR,BUILDINGID, LONGITUDE, LATITUDE, WAPSignal, Quadrant)
    #filter(WAP==WAPName & !is.na(Distance)) %>%  
    #select(ObservationID,FLOOR,Distance, WAPSignal, Quadrant)
  tempset$FLOOR <- as.factor(tempset$FLOOR)
  
  # rename column to be identical to train model
  #names(tempset)[3] <- WAPName
  
  # get model
  #tempmodel <- ModelList2[[WAPNr]]
  #tempmodel <- tempmodel[[4+Building]]
  tempmodel <- FloorModel[[WAPNr]]
  
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
#Building = Building+1
#}

tempFloorVert <- tempAllVert %>% 
  group_by(ObservationID,PredictFloor) %>% 
  summarise(aant=n()) %>%
  ungroup()

tempAllObs <- tempAllVert %>% group_by(ObservationID, FLOOR) %>%
  summarise(F0 = as.integer(sum(ifelse(PredictFloor==0,1,0))),
            F1 = as.integer(sum(ifelse(PredictFloor==1,1,0))),
            F2 = as.integer(sum(ifelse(PredictFloor==2,1,0))),
            F3 = as.integer(sum(ifelse(PredictFloor==3,1,0))),
            F4 = as.integer(sum(ifelse(PredictFloor==4,1,0)))) %>%
  ungroup()


tempAllObs <- tempAllObs %>% left_join(tempFloorVert %>% 
                           group_by(ObservationID) %>% 
                           summarise(maxFloorCount=max(aant)),
                         "ObservationID")
tempAllObs$Max1 <- as.integer(with(tempAllObs,ifelse(F0==maxFloorCount,0,
                                          ifelse(F1==maxFloorCount,1,
                                                 ifelse(F2==maxFloorCount,2,
                                                        ifelse(F3==maxFloorCount,3,4))))))
tempAllObs$Max2 <-as.integer(with(tempAllObs,ifelse(F1==maxFloorCount & Max1 < 1,1,
                                        ifelse(F2==maxFloorCount & Max1 < 2,2,
                                                ifelse(F3==maxFloorCount & Max1 < 3,3,
                                                       ifelse(F4==maxFloorCount & Max1<4,4,
                                                              -1))))))
tempAllObs$MaxDif <-as.integer(with(tempAllObs,ifelse(Max2==-1,0,Max2-Max1)))


tempAllObs$PredictedFloor <- with(tempAllObs,
                  case_when(
                      MaxDif==0 ~ Max1,
                      MaxDif==1 & Max2==1 ~ as.integer(1),
                      MaxDif==1 & Max2==2 ~ ifelse(F0>0,as.integer(1),as.integer(2)),
                      MaxDif==1 & Max2==3 ~ ifelse(F4>0,as.integer(3),as.integer(2)),
                      MaxDif==1 & Max2==4 ~ as.integer(3),
                      MaxDif==2 & Max2==2 ~ ifelse(F1>0,as.integer(1),as.integer(2)),
                      MaxDif==2 & Max2==3 ~ ifelse(F0>0,as.integer(1),ifelse(F2>0,as.integer(2),as.integer(3))),
                      MaxDif==2 & Max2==4 ~ ifelse(F3>0,as.integer(3),as.integer(2)),
                      MaxDif==3 & Max2==3 ~ ifelse(F1>0,as.integer(0),as.integer(3)),
                      MaxDif==3 & Max2==4 ~ ifelse(F3>0,as.integer(4),as.integer(1)),
                      MaxDif==4 ~ Max2))
tempAllObs$RealFloor <-  with(tempAllObs,ifelse(FLOOR=="0",as.integer(0),ifelse(FLOOR=="1",as.integer(1),
                              ifelse(FLOOR==2,as.integer(2),ifelse(FLOOR==3,as.integer(3), as.integer(4))))))
tempAllObs$FloorError <- with(tempAllObs,ifelse(RealFloor==PredictedFloor,0,1))
return(tempAllObs)
}

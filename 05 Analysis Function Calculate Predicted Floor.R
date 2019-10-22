CalculatePredictedFloor <- function(tempAllVert) {
  
tempFloorVert <- tempAllVert %>% 
  group_by(ObservationID,BUILDINGID,PredictFloor) %>% 
  summarise(aant=n()) %>%
  ungroup()

tempAllObs <- tempAllVert %>% group_by(ObservationID,BUILDINGID, FLOOR) %>%
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

tempAllObs$FloorError <- with(tempAllObs,ifelse(as.numeric(FLOOR)==PredictedFloor,0,1))
return(tempAllObs)
}

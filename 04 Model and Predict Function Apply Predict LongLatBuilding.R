
PredictLongLatBuilding <- function(VDataTop10,WAPList){
#VDataTop10 <- readRDS('../Data/clean_data/TestingVertTop10.rds')
#VDataTop10 <- readRDS('../Data/clean_data/ValidationVertTop10.rds')


# add information about WAP's
VDataTop10 <- VDataTop10 %>% left_join(WAPList, by=c("WAP"="WAP"))
VDataTop10$SignalDif <- with(VDataTop10, maxSignal - WAPSignal)

PredictObs <- VDataTop10 %>% filter(ranking<=3 & WAPSignal>0 & WAPSignal<80 &
                                      SignalDif<50 &
                                      !WAP %in% c("WAP248", "WAP508", "WAP212", "WAP018", "WAP362", 
                                                  "WAP305", "WAP413", "WAP172", "WAP449"))  %>% 
  group_by(ObservationID, LONGITUDE, LATITUDE, BUILDINGID, FLOOR) %>%
  summarise(PredictLong = sum(WAPSignal*Long_Max)/sum(WAPSignal),
            PredictLat = sum(WAPSignal*Lat_Max)/sum(WAPSignal),
            Long_Avg = sum(WAPSignal*Long_Avg)/sum(WAPSignal),
            Lat_Avg = sum(WAPSignal*Lat_Avg)/sum(WAPSignal),
            MaxSignal=max(WAPSignal)) %>%
  ungroup()

#PredictedFloor <- VDataTop10 %>% filter(ranking<=5& WAPSignal>0 & WAPSignal<80 & !is.na(PredictFloor) &
#                                          !WAP %in% c("WAP248", "WAP508", "WAP212", "WAP018", "WAP362", 
#                                                      "WAP305", "WAP413", "WAP172", "WAP449")) 
#PredictedFloor <- CalculatePredictedFloor(PredictedFloor)

#PredictObs <- VDataTop10 %>% filter(ranking<=5 & WAPSignal>0 &
#                    !WAP %in% c("WAP248", "WAP508", "WAP212", "WAP018", "WAP362", 
#                              "WAP305", "WAP413", "WAP172", "WAP449"))  %>% 
#  group_by(ObservationID, LONGITUDE, LATITUDE, BUILDINGID, FLOOR) %>%
#  summarise(PredictLong = sum(exp(WAPSignal)*Long_Max)/sum(exp(WAPSignal)),
#            PredictLat = sum(exp(WAPSignal)*Lat_Max)/sum(exp(WAPSignal)),
#            Long_Avg = sum(exp(WAPSignal)*Long_Avg)/sum(exp(WAPSignal)),
#            Lat_Avg = sum(exp(WAPSignal)*Lat_Avg)/sum(exp(WAPSignal))) %>%
#  ungroup()


#ggplot(PredictObs, aes(x=PredictLong, y=PredictLat)) +
#  geom_point(size=2, shape=23)

#ggplot(PredictObs, aes(x=Long_Avg, y=Lat_Avg)) +
#  geom_point(size=2, shape=23)


PredictObs$LongError        <- with(PredictObs,LONGITUDE - PredictLong)
PredictObs$LatError         <- with(PredictObs,LATITUDE - PredictLat)
PredictObs$PredictBuilding  <- with(PredictObs,
                                    ifelse(PredictLat > 4876350 + PredictLong*1.506,
                                           0,
                                           ifelse(PredictLat >4876000 + PredictLong* 1.506,
                                                  1,
                                                  2)
                                    )
)
PredictObs$BuildingError    <- with(PredictObs, 
                                    ifelse(BUILDINGID==PredictBuilding,0,1)) 

return(PredictObs)
}
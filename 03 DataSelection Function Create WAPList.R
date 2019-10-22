VData <- readRDS('../Data/clean_data/trainingVert.rds')

WAPList <- VData %>% filter(WAPSignal>0 &WAPSignal<80 & USERID!=14 & USERID!=6) %>% 
  group_by(WAP) %>% 
  summarise(aant=n(), maxSignal=max(WAPSignal)) %>% ungroup()

# plot max signal vs number of observations
ggplot(WAPList, aes(x=aant, y=maxSignal)) +
  geom_point(size=2, shape=23) 

# group by longitude and WAP 
VLong <- VData %>%  
            filter(WAPSignal>0 & WAPSignal<80 & USERID!=14 & USERID!=6 ) %>%
            group_by(WAP, LONGITUDE) %>% 
            summarise(avg= exp(mean(WAPSignal)), 
                      mx = exp(max(WAPSignal))) %>%
            ungroup()

# Weighted average 
WAPList <- WAPList %>% left_join((VLong %>% 
  group_by(WAP) %>% 
  summarise(Long_Max = sum(mx*LONGITUDE)/sum(mx),
            Long_Avg = sum(avg*LONGITUDE)/sum(avg)) %>%
  ungroup()), "WAP")

# group by latitude and WAP 
VLat <- VData %>%  
  filter(WAPSignal>0 & WAPSignal<80 &USERID!=14 &USERID!=6) %>%
  group_by(WAP, LATITUDE) %>% 
  summarise(avg= exp(mean(WAPSignal)), 
            mx = exp(max(WAPSignal))) %>%
  ungroup()

# Weighted average 
WAPList <- WAPList %>% left_join((VLat %>% 
                                    group_by(WAP) %>% 
                                    summarise(Lat_Max = sum(mx*LATITUDE)/sum(mx),
                                              Lat_Avg = sum(avg*LATITUDE)/sum(avg)) %>%
                                    ungroup()), "WAP")

# plot WAP's by Longitude and Latitude
ggplot(WAPList, aes(x=Long_Max, y=Lat_Max)) +
  geom_point(size=2, shape=23) + theme(panel.background = element_blank())
ggplot(WAPList, aes(x=Long_Avg, y=Lat_Avg)) +
  geom_point(size=2, shape=23) 
# plot actual Building
ggplot(DataAllBuildings, aes(x=LONGITUDE, y=LATITUDE)) +
  geom_point(size=2, shape=23) 

##############floor
VDataExt <- VData %>% left_join(WAPList, "WAP")
VDataExt$Distance <- with(VDataExt,sqrt((Long_Max-LONGITUDE)^2+(Lat_Max-LATITUDE)^2))
VDataExt$Quadrant <- with(VDataExt, ifelse(Long_Max>=LONGITUDE, 
                                           ifelse(Lat_Max>=LATITUDE,1,2),
                                            ifelse(Lat_Max>=LATITUDE,4,3)))
VDataExt$DistanceBucket <- with(VDataExt, ifelse(Distance<=10,"close", ifelse(Distance<=20,"near", ifelse(Distance<=30,"mwah",
                                                                      ifelse(Distance<=40,"oops","tsss")))))
VDataExt$SignalBucket <- with(VDataExt, ifelse(maxSignal==0,"no max", ifelse(WAPSignal/maxSignal>0.9,"very strong",
                                               ifelse(WAPSignal/maxSignal>0.7,"strong", 
                                                      ifelse(WAPSignal/maxSignal>0.4,"medium","weak")))))
VDataExt$FLOOR <- as.factor(VDataExt$FLOOR)
VDataExt$Quadrant <- as.factor(VDataExt$Quadrant)
VDataExt$BUILDINGID <- as.factor(VDataExt$BUILDINGID)
VDataExt$DistanceBucket <- as.factor(VDataExt$DistanceBucket)
VDataExt$SignalBucket <- as.factor(VDataExt$SignalBucket)
VDataExt <- VDataExt %>% filter(!is.na(aant))

ggplot(VDataExt %>% filter(WAP=="WAP179"&WAPSignal!=0 &WAPSignal<80), aes(x=Distance, y=WAPSignal, color=FLOOR)) +
  geom_point(size=2, shape=23) + facet_grid(. ~ Quadrant)

VFloor <- VData %>%  
  filter(WAPSignal>0 & WAPSignal<80 &USERID!=14 &USERID!=6) %>%
  group_by(WAP, BUILDINGID, FLOOR) %>% 
  summarise(avg= mean(WAPSignal), 
            mx = max(WAPSignal),
            aant = n()) %>%
  ungroup()

VFloor$Product <- with(VFloor, avg*mx*aant)

VFloorMax <- VFloor %>% group_by(WAP) %>% 
  summarise(maxprod = max(Product)) %>% ungroup()



VFloorMax <- VFloorMax %>% left_join(VFloor,by=c("WAP"="WAP", "maxprod"="Product")) %>% 
  select(WAP, BUILDINGID, FLOOR) %>% 
  rename("WAPBuilding"="BUILDINGID", "PredictFloor" = "FLOOR")

WAPList <- WAPList %>% left_join(VFloorMax, "WAP")

################################
VDataTop10 <- readRDS('../Data/clean_data/TestingVertTop10.rds')
VDataTop10 <- readRDS('../Data/clean_data/ValidationVertTop10.rds')


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

PredictedFloor <- VDataTop10 %>% filter(ranking<=5& WAPSignal>0 & WAPSignal<80 & !is.na(PredictFloor) &
                                          !WAP %in% c("WAP248", "WAP508", "WAP212", "WAP018", "WAP362", 
                                                      "WAP305", "WAP413", "WAP172", "WAP449")) 
PredictedFloor <- CalculatePredictedFloor(PredictedFloor)

#PredictObs <- VDataTop10 %>% filter(ranking<=5 & WAPSignal>0 &
#                    !WAP %in% c("WAP248", "WAP508", "WAP212", "WAP018", "WAP362", 
#                              "WAP305", "WAP413", "WAP172", "WAP449"))  %>% 
#  group_by(ObservationID, LONGITUDE, LATITUDE, BUILDINGID, FLOOR) %>%
#  summarise(PredictLong = sum(exp(WAPSignal)*Long_Max)/sum(exp(WAPSignal)),
#            PredictLat = sum(exp(WAPSignal)*Lat_Max)/sum(exp(WAPSignal)),
#            Long_Avg = sum(exp(WAPSignal)*Long_Avg)/sum(exp(WAPSignal)),
#            Lat_Avg = sum(exp(WAPSignal)*Lat_Avg)/sum(exp(WAPSignal))) %>%
#  ungroup()


ggplot(PredictObs, aes(x=PredictLong, y=PredictLat)) +
  geom_point(size=2, shape=23)

ggplot(PredictObs, aes(x=Long_Avg, y=Lat_Avg)) +
  geom_point(size=2, shape=23)


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

saveRDS(PredictObs,'../Data/clean_data/result test obv WAP met div filters.rds')
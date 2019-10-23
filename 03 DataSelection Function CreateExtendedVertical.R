CreateExtendedVertical <- function(VData, WAPList){
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
return(VDataExt)
}
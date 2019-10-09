HData <-DataAllBuildings

#add ID for row/observation

HData$ObservationID <- seq_len(nrow(DataModel))

#loop through all columns of the dataset
LoopCol =1

# Fix attributes that should be in every record
# All except WAP-columns
FixedAttributes <- HData %>% select(ObservationID,LONGITUDE,LATITUDE,FLOOR,BUILDINGID,
                                    SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP)

#loop through all columns of the dataset
LoopCol =1

while(LoopCol< ncol(HData)) {
  #if columname start with WAP... then add to vertical dataframe
  if(substr(colnames(HData[LoopCol]),1,3)=="WAP") {
    tempData <- FixedAttributes
    tempData$WAP <- colnames(HData[LoopCol])
    tempData$WAPSignal <-  HData[,LoopCol]
    ifelse(LoopCol==1,
         VData <- tempData,
         VData <- rbind(VData, tempData))
    print(LoopCol)
  
  }
  
  LoopCol=LoopCol+1
  
}

#data by WAP
VData %>% 
  group_by(WAP) %>% 
  filter(WAPSignal!=0) %>% 
  summarize(number=n(), min=min(WAPSignal),max=max(WAPSignal)) %>%
  arrange(-number)


#data by Longitude, Latitude,WAP,FLOOR,PHONEID, USERID
VData %>% 
  group_by(LONGITUDE,LATITUDE,WAP,FLOOR,PHONEID, USERID) %>% 
  filter(WAPSignal!=0) %>% 
  summarize(number=n(), min=min(WAPSignal),max=max(WAPSignal),avg=mean(WAPSignal)) %>%
  arrange(-number)

#data by  USERID
VData %>% 
  group_by(USERID) %>% 
  filter(WAPSignal!=0) %>% 
  summarize(number=n(), min=min(WAPSignal),max=max(WAPSignal),avg=mean(WAPSignal)) %>%
  arrange(-number)

ggplot(data=VData %>% filter(WAPSignal!=0),
       aes(x=factor(USERID), y=WAPSignal)) +
  geom_boxplot() 


#data by  PHONEID
VData %>% 
  group_by(PHONEID) %>% 
  filter(WAPSignal!=0) %>% 
  summarize(number=n(), min=min(WAPSignal),max=max(WAPSignal),avg=mean(WAPSignal)) %>%
  arrange(-number)
 
  ggplot(data=VData %>% filter(WAPSignal!=0),
         aes(x=factor(PHONEID), y=WAPSignal)) +
  geom_boxplot() 


  
  #data by  Observation
  VData %>% 
    group_by(ObservationID) %>% 
    filter(WAPSignal!=0) %>% 
    summarize(number=n(), min=min(WAPSignal),max=max(WAPSignal),avg=mean(WAPSignal)) %>%
    arrange(number)
  


  #specific location
  arrange(VData %>% filter(round(LONGITUDE,0)== -7395 & round(LATITUDE,0)==4864837 
                           & WAPSignal!=0 & FLOOR==3),-WAPSignal)
  
  #specific area
  arrange(VData %>% filter(LONGITUDE> -7450 & LONGITUDE< -7370 & 
                             LATITUDE>4864800 & LATITUDE<4864820 ),-WAPSignal)
  
  #max per obeservationID
  MaxSignals <- VData %>% group_by(ObservationID) %>% summarise(MaxSignal = max(WAPSignal)) 
  MaxWap <-MaxSignals %>% 
    filter(MaxSignal!=0) %>% 
    left_join(VData,c("ObservationID"="ObservationID","MaxSignal"="WAPSignal")) %>%
    select (ObservationID, MaxSignal, WAP, LONGITUDE, LATITUDE, BUILDINGID, FLOOR)
  
  # no signal records
  NoSignalIDs <- VData %>% 
                    group_by(ObservationID) %>% 
                    summarise(MaxSignal = max(WAPSignal)) %>%
                    filter(MaxSignal==0) %>%
                    select(ObservationID)
  
  NoSignal <-DataAllBuildings[NoSignalIDs$ObservationID,]
  
  DataAllBuildings <- DataAllBuildings[-NoSignalIDs$ObservationID,]
  
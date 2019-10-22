#data by WAP
VData <- readRDS('../Data/clean_data/trainingVert.rds')
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


# distribution Longitude of single WAP
WAPNR <- "WAP179"
VData$FLOOR <- as.factor(VData$FLOOR)

#LONGITUDE
ggplot(VData %>% filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 ), 
       aes(x=LONGITUDE, y=WAPSignal, color=FLOOR)) +
  geom_point(size=2, shape=23)  +  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))

#mean & median
VData %>%  filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 ) %>%
          group_by(WAP) %>% summarise(avg=mean(LONGITUDE), med=median(LONGITUDE))

#by longitude 
VLong <- VData %>%  filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 ) %>%
  group_by(WAP, LONGITUDE) %>% summarise(avg=exp(mean(WAPSignal)), 
                                         med=exp(median(WAPSignal)),
                                         mx = exp(max(WAPSignal)))

#ggplot(VLong, aes(x=LONGITUDE, y=mx)) +  geom_point(size=2, shape=23) 
#Weighted average 
VLong %>% group_by(WAP) %>% summarise(mxL = sum(mx*LONGITUDE)/sum(mx),
                                      avgL = sum(avg*LONGITUDE)/sum(avg),
                                      medL = sum(med*LONGITUDE)/sum(med))

#LATITUDE
ggplot(VData %>% filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 &USERID!=14 &USERID!=6), 
       aes(x=LATITUDE, y=WAPSignal, color=FLOOR)) +
  geom_point(size=2, shape=23) 

#mean & median
VData %>%  filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 ) %>%
  group_by(WAP) %>% summarise(avg=mean(LATITUDE), med=median(LATITUDE))

#by latitude 
VLat <- VData %>%  filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 ) %>%
  group_by(WAP, LATITUDE) %>% summarise(avg=exp(mean(WAPSignal)), 
                                        med=exp(median(WAPSignal)),
                                        mx = exp(max(WAPSignal)))

#ggplot(VLong, aes(x=LONGITUDE, y=mx)) +  geom_point(size=2, shape=23) 
#Weighted average 
VLat %>% group_by(WAP) %>% summarise(mxL = sum(mx*LATITUDE)/sum(mx),
                                      avgL = sum(avg*LATITUDE)/sum(avg),
                                      medL = sum(med*LATITUDE)/sum(med))

arrange(VData %>% filter(ObservationID==3994),-WAPSignal)


VData$USERID <- as.factor(VData$USERID)
ggplot(VData %>% filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80), 
       aes(x=LATITUDE, y=WAPSignal, color=USERID)) +
  geom_point(size=2, shape=23) + theme(panel.background = element_blank())

ggplot(VData %>% filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 &USERID!=14 &USERID!=6), 
       aes(x=LONGITUDE, y=LATITUDE, color=WAPSignal)) +
  geom_point(size=2, shape=23) 

ggplot(VData %>% filter(WAP==WAPNR & WAPSignal>0 &WAPSignal<80 &USERID!=14 &USERID!=6), 
       aes(x=LONGITUDE, y=LATITUDE, color=FLOOR)) +
  geom_point(size=2, shape=23)  +  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))


# Range of Longitude
arrange(VData %>% 
          filter(WAPSignal>0 & WAPSignal<80) %>% 
          group_by(WAP) %>% 
          summarise(LongRange = max(LONGITUDE)-min(LONGITUDE),
                    LatRange = max(LATITUDE)-min(LATITUDE),
                    CountObs  = n(), 
                    MaxSignal = max(WAPSignal)),
        -LongRange)

arrange(VData %>% filter(WAP=="WAP248") %>% select(TIMESTAMP, BUILDINGID), TIMESTAMP)

ggplot(VData %>% filter(WAP=="WAP248" & WAPSignal!=0) , aes(x=TIMESTAMP, y=BUILDINGID)) +  
  geom_point(size=2, shape=23)


ggplot(VData %>% filter(WAP=="WAP248" & WAPSignal!=0) , aes(x=TIMESTAMP, y=WAPSignal)) +  
  geom_point(size=2, shape=23)

# in time
ggplot(VData %>% filter( WAPSignal!=0) , aes(x=TIMESTAMP, y=BUILDINGID)) +  
  geom_point(size=2, shape=23)

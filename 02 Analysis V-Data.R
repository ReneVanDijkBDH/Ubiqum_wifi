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

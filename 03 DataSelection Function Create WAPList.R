CreateWAPList <- function(VData){
#VData <- readRDS('../Data/clean_data/trainingVert.rds')

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
return(WAPList)
}

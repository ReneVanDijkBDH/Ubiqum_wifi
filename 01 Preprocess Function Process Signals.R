ProcessSignals <- function(WAPData, VertData){
  #WAPData <- DataProcessing
  
  # find maximum signal per observationID
  MaxSignals <- VertData %>% 
    group_by(ObservationID) %>% 
    summarise(MaxSignal = max(WAPSignal)) 
  
  # Define ID's of records with no WAP signal 
  NoSignalIDs <- MaxSignals %>%
                    filter(MaxSignal==0) %>%
                    select(ObservationID)

  # remove records without signal from data
  if(nrow(NoSignalIDs)>0){
    WAPData <- WAPData[-NoSignalIDs$ObservationID,]
  }

  # find WAP's with maximum signal per observationID
  MaxWap <- MaxSignals %>% 
              filter(MaxSignal!=0) %>% 
              left_join(VertData,c("ObservationID"="ObservationID",
                       "MaxSignal"="WAPSignal")) %>%
              select (ObservationID, MaxSignal, WAP, LONGITUDE, LATITUDE, BUILDINGID, FLOOR)


  # Add WAP with maximum signal to full dataset. 
  # if more then 1 WAP with max signal, system chooses 1 
  MaxWapObs <- MaxWap %>% 
                  group_by(ObservationID) %>% 
                  filter(row_number()==1) %>% 
                  select(ObservationID,WAP) %>% 
                  rename(MaxWap = WAP) 

  WAPData <- WAPData %>% 
                left_join(MaxWapObs,"ObservationID")
  
  return(WAPData)
}  
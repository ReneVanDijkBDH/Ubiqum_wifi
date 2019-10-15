RankTesting <- function(WAPData, VertData, MaxWAPCount){
  #WAPData <- testing
  #VertData <- testingVert
  #WAPData <- DataProcessing
  #VertData <- validationVert  
  
  #Rank signals by observation
  VertData <- VertData %>%
    group_by(ObservationID) %>%
    mutate(ranking = order(order(WAPSignal, decreasing=TRUE)))
  
  # Define ID's of records with no WAP signal 
  NoSignalIDs <- VertData %>%
    filter(WAPSignal==0 & ranking==1) %>%
    select(ObservationID)
  
  # remove records without signal from data
  if(nrow(NoSignalIDs)>0){
    WAPData <- WAPData[-NoSignalIDs$ObservationID,]
  }
  
  # set minimum records in training-set required
  MinCount <- 20
  MaxWAPCount <- MaxWAPCount %>% filter(MaxCount>=MinCount)
  
  # remove all records in vert below minimum
  TopRankData <- VertData %>% left_join(MaxWAPCount, by=c("WAP" ="MaxWap")) %>%
    filter(!is.na(MaxCount))


  # only take into consideration top X ranked records 
  #TopRank <-5
  #TopRankData <- VertData %>% filter(ranking<=TopRank)
  
  # SelectCount Rank 1 for every WAP
  #WAPMAX <- VertData %>% filter(ranking==1) %>% group_by(WAP) %>% summarise(countmax = n())
  
  # add number of top rankings
  #TopRankData <- TopRankData %>% left_join(WAPMAX, "WAP")
  
  # find WAP with max number of top tankings in top 5
  TopRankData <- arrange(TopRankData, ObservationID, -WAPSignal)
  TopRankWAP <- TopRankData[!duplicated(TopRankData$ObservationID),]
  
  # add maximum WAP to data-set
  WAPData <- WAPData %>% 
    left_join(TopRankWAP %>% 
                select(ObservationID,WAP,WAPSignal) %>%
                rename(MaxWap=WAP),"ObservationID")
  
  # add column ranking (required for modelling purpose)
  WAPData$ranking <- 0
  
  # remove records where max signal is 0 
  WAPData <- WAPData %>% filter(WAPSignal>0)
  
  return(WAPData)
}
  
  
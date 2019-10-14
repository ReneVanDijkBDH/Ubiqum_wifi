RankTraining <- function(WAPData, VertData){
  #WAPData <- DataProcessing
  
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
  
  
  # only take into consideration top X ranked records 
  TopRank <-5
  TopRankData <- VertData %>% filter(ranking<=TopRank)
  
  # Count Rank 1 for every WAP
  WAPMAX <- VertData %>% filter(ranking==1) %>% group_by(WAP) %>% summarise(countmax = n())
  
  # add number of top rankings
  TopRankData <- TopRankData %>% left_join(WAPMAX, "WAP")
  
  # find WAP with max number of top tankings in top 5
  TopRankData <- arrange(TopRankData, ObservationID, -countmax)
  TopRankWAP <- TopRankData[!duplicated(TopRankData$ObservationID),]
  
  # add maximum WAP to data-set
  WAPData <- WAPData %>% 
    left_join(TopRankWAP %>% select(ObservationID,WAP,WAPSignal,ranking),"ObservationID")
  
  return(WAPData)
}
  
  
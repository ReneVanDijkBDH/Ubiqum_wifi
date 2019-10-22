VertTop10 <- function(Vert){

# return top 10 ranked signals per observation
ObsIDs <- Vert %>% group_by(ObservationID) %>% summarise(aant=n())
loopID <- 1
while(loopID<=nrow(ObsIDs)){
  print(loopID)
  ObsID <- as.numeric(ObsIDs[loopID,1])
  tempObs <- Vert %>% 
                filter(ObservationID==ObsID) %>% 
                top_n(10, WAPSignal)
  tempObs <- arrange(tempObs,-WAPSignal)
  tempObs$ranking <- seq_len(nrow(tempObs))
  ifelse(loopID==1,
         VertTop10 <- tempObs,
         VertTop10 <-rbind(VertTop10,tempObs))
  loopID=loopID+1
}
VertTop10 <- VertTop10 %>% filter(ranking<=10)

return(VertTop10)
}
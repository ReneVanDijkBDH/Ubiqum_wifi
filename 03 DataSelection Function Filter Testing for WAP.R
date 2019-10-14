#########################################################################
## Author:    René van Dijk
## Creation:  10-10-2019
## Purpose:   adjust testing data set:
##            1) only include records of current WAP
##            2) only include attributes also in training set 
#########################################################################
FilterTestingForWAP <- function(testing, TrainingAttributes, WAP) {

  
  
  # filter only records of current WAP
  Testing1WAP <- testing %>% filter(MaxWap==WAP)

  # create same data structure as training for testing
  LoopCol<-ncol(Testing1WAP)
  while (LoopCol>=1) {
    ifelse(colnames(Testing1WAP[LoopCol]) %in% TrainingAttributes,
           Testing1WAP <- Testing1WAP,
           Testing1WAP <- Testing1WAP[-LoopCol])
    LoopCol = LoopCol-1
  }
  return(Testing1WAP)
}
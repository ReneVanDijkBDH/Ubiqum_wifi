#########################################################################
## Author:    René van Dijk
## Creation:  3-10-2019
## Purpose:   Run process to locate position 
##            based on wifi-fingerprints
#########################################################################

#import libraries
library(dplyr)
library(caret)
library(corrplot)


#import data
DataAllBuildings <- read.csv('../Data/trainingData.csv')

#Divide dataset in smaller sets per building
DataBuilding0 <- ExtractData1Building(DataAllBuildings, BuildingID = 0)
DataBuilding1 <- ExtractData1Building(DataAllBuildings, BuildingID = 1)
DataBuilding2 <- ExtractData1Building(DataAllBuildings, BuildingID = 2)

#summarize data
DataB0_FloorSpaces <- DataBuilding0 %>% group_by(FLOOR, SPACEID) %>% summarise(obs=n())

#select small part of data
#Spacelist <- c(106,116,120)
#Spacelist <- c(106,116,120,107,110,111,112)
Spacelist <- c(106,116,120,107,110,111,112,113,114,115)
DataB0_F0_Spaces <- ExtractData1FloorSpace(DataBuilding0, FloorID = 0,Spacelist)

#rescale RSSI value
DataB0_F0_Spaces <- RescaleRSSI(DataB0_F0_Spaces)

#define dataset for modelling
DataModel <-DataB0_F0_Spaces

#remove attributes not needed for modelling
DataModel<- DataModel[, !names(DataModel) %in% 
                                c( "FLOOR", "BUILDINGID","SPACEID",
                                  "RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")]
#create correlation matrix
corrData <- cor(DataModel)
corrplot(corrData)

#create training and test set
set.seed(123)
trainSize<-round(nrow(DataModel)*0.75) 
training_indices<-sample(seq_len(nrow(DataModel)),size =trainSize)

# create training and test datasets
training <- DataModel[training_indices,]
testing <- DataModel[-training_indices,]

#apply linear model
RegModelLong <- lm(LONGITUDE ~ . -LATITUDE, training)
RegModelLat <- lm(LATITUDE ~ . -LONGITUDE, training)

# predict values for test set
RegPredictLong <- predict(RegModelLong,testing)
RegPredictLat <- predict(RegModelLat,testing)

#evaluate predicted values for test set
postResample(testing$LONGITUDE,RegPredictLong)
postResample(testing$LATITUDE,RegPredictLat)

# Analyse predictions and errors
CompareResults <- testing
CompareResults$RegPredictLong <- RegPredictLong
CompareResults$RegPredictLat <- RegPredictLat
CompareResults$RegLongError <- with(CompareResults,LONGITUDE - RegPredictLong)
CompareResults$RegLatError <- with(CompareResults,LATITUDE - RegPredictLat)

# ideas:
# Convert signal to log
# Only include strong signals
# Validate locations

# to do:
# plot errors
# Validate locations
# source function
# Rmarkdown


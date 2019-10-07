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
#DataB0_FloorSpaces <- DataBuilding0 %>% group_by(FLOOR, SPACEID) %>% summarise(obs=n())

#select small part of data
#Spacelist <- c(106,116,120)
#Spacelist <- c(106,116,120,107,110,111,112)
Spacelist <- c(106,116,120,107,110,111,112,113,114,115)
DataB0_F0_Spaces <- ExtractData1FloorSpace(DataBuilding0, FloorID = 0,Spacelist)

#rescale RSSI value
DataB0_F0_Spaces <- RescaleRSSI(DataB0_F0_Spaces)
DataBuilding0 <- RescaleRSSI(DataBuilding0)
DataAllBuildings <- RescaleRSSI(DataAllBuildings)

#Minimum level relevant
DataAllBuildings <- ApplyMinimumSignal(DataAllBuildings,20)  

#define dataset for modelling
DataModel <-DataAllBuildings

#remove attributes not needed for modelling
DataModel<- DataModel[, !names(DataModel) %in% 
                                c( "SPACEID",
                                  "RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")]

#create training and test set
set.seed(456)
trainSize<-round(nrow(DataModel)*0.75) 
training_indices<-sample(seq_len(nrow(DataModel)),size =trainSize)

# create training and test datasets
training <- DataModel[training_indices,]
testing <- DataModel[-training_indices,]

#apply linear model
RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID, training)
RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID, training)

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

CompareResults$PredictBuilding <- with(CompareResults,
                                  ifelse(RegPredictLat > 4876350 + RegPredictLong*1.506,
                                         0,
                                         ifelse(RegPredictLat >4876000 + RegPredictLong* 1.506,
                                              1,
                                              2)
                                        )
                                  )
CompareResults$BuildingError <- with(CompareResults, ifelse(BUILDINGID==PredictBuilding,0,1))   

# visualize errors
ggplot(CompareResults %>% filter(abs(RegLongError) > 0 ), 
       aes(x=LONGITUDE, y=LATITUDE, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)

ggplot(CompareResults %>% filter(abs(RegLongError) > 0 ), 
       aes(x=RegPredictLong, y=RegPredictLat, color=RegLongError)) +
  geom_point(size=2, shape=23) +
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)


ggplot(CompareResults %>% filter(abs(RegLongError) > 0 ), 
       aes(x=RegPredictLong, y=RegPredictLat, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+ 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)


# count errors in building
CompareResults %>% group_by(BUILDINGID) %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())
  
CompareResults %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())



CompareResults %>% filter(LONGITUDE> -7400 & LONGITUDE< -7350 & 
                            LATITUDE>4864830 & LATITUDE<4864850 &BuildingError==1)


set.seed(456)
trctrl <- trainControl(method="repeatedcv",repeats = 2) 
KNNModel <- train(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID, 
                  data = training, 
                  method = "knn", 
                  trControl = trctrl, 
                  preProcess = c("center","scale"), 
                  tuneLength = 10)

#KNN prediction & results
KNNPredictLong <- predict(KNNModel, newdata = testing)

postResample(testing$LONGITUDE,KKPredictLong)
postResample(testing$LATITUDE,RegPredictLat)



# ideas:
# Convert signal to log
# Only include strong signals
# Validate locations

# to do:
# plot errors
# Validate locations
# source function
# Rmarkdown



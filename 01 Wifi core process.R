#########################################################################
## Author:    René van Dijk
## Creation:  3-10-2019
## Purpose:   Run process to locate position 
##            based on wifi-fingerprints
##            01 preprocess
##            02 analysis
##            03 Data selection
##            04 Modelling & Prediction
##            05 Analysing
#########################################################################

#import libraries
library(dplyr)
library(caret)
library(corrplot)


#import data
DataAllBuildings <- read.csv('../Data/trainingData.csv')

# add unique row ID for reference
DataAllBuildings$ObservationID <- seq_len(nrow(DataAllBuildings))

# create vertical dataframe for analysing and query purposes


#Divide dataset in smaller sets per building
#DataBuilding0 <- ExtractData1Building(DataAllBuildings, BuildingID = 0)
#DataBuilding1 <- ExtractData1Building(DataAllBuildings, BuildingID = 1)
#DataBuilding2 <- ExtractData1Building(DataAllBuildings, BuildingID = 2)

#summarize data
#DataB0_FloorSpaces <- DataBuilding0 %>% group_by(FLOOR, SPACEID) %>% summarise(obs=n())

#select small part of data
#Spacelist <- c(106,116,120)
#Spacelist <- c(106,116,120,107,110,111,112)
#Spacelist <- c(106,116,120,107,110,111,112,113,114,115)
#DataB0_F0_Spaces <- ExtractData1FloorSpace(DataBuilding0, FloorID = 0,Spacelist)

#rescale RSSI value
#DataB0_F0_Spaces <- RescaleRSSI(DataB0_F0_Spaces)
#DataBuilding0 <- RescaleRSSI(DataBuilding0)
DataAllBuildings <- RescaleRSSI(DataAllBuildings)

# Define ID's of records with no WAP signal 
NoSignalIDs <- VData %>% 
  group_by(ObservationID) %>% 
  summarise(MaxSignal = max(WAPSignal)) %>%
  filter(MaxSignal==0) %>%
  select(ObservationID)

# records without signal
NoSignal <-DataAllBuildings[NoSignalIDs$ObservationID,]

#remove records without signal from data
DataAllBuildings <- DataAllBuildings[-NoSignalIDs$ObservationID,]


#Add WAP with maximum signal
MaxWapObs <- as.data.frame(MaxWap %>% group_by(ObservationID) %>% filter(row_number()==1) %>% 
  select(ObservationID,WAP)) %>% rename(MaxWap = WAP) #slice(1)
DataAllBuildings <- DataAllBuildings %>% left_join(MaxWapObs,"ObservationID")



#Minimum level relevant
#DataAllBuildings <- ApplyMinimumSignal(DataAllBuildings,20)  

WAP = "WAP496"
Data1WAP <- ExtractData1WAP(DataAllBuildings, WAP)

#define dataset for modelling
#DataModel <-DataAllBuildings
DataModel <- Data1WAP



#remove attributes not needed for modelling
DataModel<- DataModel[, !names(DataModel) %in% 
                                c( "SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")]

#create training and test set
set.seed(455)
trainSize<-round(nrow(DataModel)*0.75) 
training_indices<-sample(seq_len(nrow(DataModel)),size =trainSize)

# create training and test datasets
training <- DataModel[training_indices,]
testing <- DataModel[-training_indices,]

#apply linear model
RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap,
                   training )
RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap, 
                  training)

#predict only for records with same Max WAP
testing <- testing %>% filter(MaxWap==WAP)

# predict values for test set
RegPredictLong <- predict(RegModelLong,testing)
RegPredictLat <- predict(RegModelLat,testing)

#evaluate predicted values for test set
postResample(testing$LONGITUDE,RegPredictLong)
postResample(testing$LATITUDE,RegPredictLat)

# Analyse predictions and errors

TestingRegBuilding <- CalculatePredictedBuilding(testing, RegPredictLong, RegPredictLat)



#KNN
set.seed(456)
trctrl <- trainControl(method="repeatedcv",repeats = 1) 
KNNModelLong <- train(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID, 
                  data = training, 
                  method = "knn", 
                  trControl = trctrl, 
                  preProcess = c("center","scale"), 
                  tuneLength = 10)

KNNModelLat <- train(LATITUDE ~ . - LONGITUDE - FLOOR - BUILDINGID - ObservationID, 
                      data = training, 
                      method = "knn", 
                      trControl = trctrl, 
                      preProcess = c("center","scale"), 
                      tuneLength = 10)



#KNN prediction & results
KNNPredictLong <- predict(KNNModelLong, newdata = testing)
KNNPredictLat <- predict(KNNModelLat, newdata = testing)


postResample(testing$LONGITUDE,KNNPredictLong)
postResample(testing$LATITUDE,KNNPredictLat)

TestingKNNBuilding <- CalculatePredictedBuilding(testing, KNNPredictLong, KNNPredictLat)


######################
##Analyse errors

TestingResults <- TestingRegBuilding
#TestingResults <- TestingKNNBuilding

# count errors in building
TestingResults %>% group_by(BUILDINGID) %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())

TestingResults %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())


CompareResults %>% filter(LONGITUDE> -7450 & LONGITUDE< -7370 & 
                            LATITUDE>4864800 & LATITUDE<4864820 & BuildingError==1 )


CompareResults %>% group_by(LONGITUDE, LATITUDE, BuildingError) %>% 
  summarise(number =n()) %>% 
  filter(LONGITUDE> -7450 & LONGITUDE< -7370 & 
           LATITUDE>4864800 & LATITUDE<4864820 & BuildingError==1 )

CompareResults %>% filter(round(LONGITUDE,3)==-7404.836) %>% 
  select (BuildingError, LONGITUDE, LATITUDE,FLOOR, RegPredictLong, RegPredictLat, WAP484, WAP342, WAP398,WAP286, WAP082, WAP083)


# visualize errors
ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=LONGITUDE, y=LATITUDE, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)
  #xlim(-7600,-7200) +
  #ylim(4864600, 4865000)



ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=PredictLong, y=PredictLat, color=LongError)) +
  geom_point(size=2, shape=23) +
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)


ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=PredictLong, y=PredictLat, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+ 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)





# ideas:
# Convert signal to log
# Only include strong signals
# Validate locations

# to do:
# plot errors
# Validate locations
# source function
# Rmarkdown

006 195
450
425,430


meeste x maxwap
1 WAP011   504
2 WAP121   425
3 WAP496   397
4 WAP495   381
5 WAP203   369
6 WAP065   365
7 WAP489   361

****************
  met meerdere buildings
1 WAP046      2
2 WAP113      2
3 WAP172      2
4 WAP173      2
5 WAP175      2
6 WAP180      2
7 WAP181      2
8 WAP189      2
9 WAP248      3
10 WAP369      2
11 WAP503      2


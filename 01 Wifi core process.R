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
##            06 Validation
#########################################################################

# import libraries
library(dplyr)
library(caret)
library(corrplot)


# import data
DataAllBuildings <- read.csv('../Data/trainingData.csv')

# add unique row ID for reference
DataAllBuildings$ObservationID <- seq_len(nrow(DataAllBuildings))

# Divide dataset in smaller sets per building
#DataBuilding0 <- ExtractData1Building(DataAllBuildings, BuildingID = 0)
#DataBuilding1 <- ExtractData1Building(DataAllBuildings, BuildingID = 1)
#DataBuilding2 <- ExtractData1Building(DataAllBuildings, BuildingID = 2)

# summarize data
#DataB0_FloorSpaces <- DataBuilding0 %>% group_by(FLOOR, SPACEID) %>% summarise(obs=n())

# select small part of data
#Spacelist <- c(106,116,120)
#Spacelist <- c(106,116,120,107,110,111,112)
#Spacelist <- c(106,116,120,107,110,111,112,113,114,115)
#DataB0_F0_Spaces <- ExtractData1FloorSpace(DataBuilding0, FloorID = 0,Spacelist)

# rescale RSSI value
#DataB0_F0_Spaces <- RescaleRSSI(DataB0_F0_Spaces)
#DataBuilding0 <- RescaleRSSI(DataBuilding0)
DataAllBuildings <- RescaleRSSI(DataAllBuildings)

#define dataset for modelling
DataModel <-DataAllBuildings

#create training and test set
set.seed(455)
trainSize<-round(nrow(DataModel)*0.75) 
training_indices<-sample(seq_len(nrow(DataModel)),size =trainSize)

# create training and test datasets
training <- DataModel[training_indices,]
testing <- DataModel[-training_indices,]

# Vertical data-set of Training
trainingVert <- ConvertToVerticalData(training)

# identify WAP used for modelling
training <- RankTraining(training, trainingVert)
  





RegModelList <- vector(mode="list", length=520)



# create vertical dataframe for analysing and query purposes
VertData <- ConvertToVerticalData(DataAllBuildings)

# find maximum signal per observationID
MaxSignals <- VertData %>% 
                group_by(ObservationID) %>% 
                summarise(MaxSignal = max(WAPSignal)) 


# Define ID's of records with no WAP signal 
NoSignalIDs <- MaxSignals %>%
  filter(MaxSignal==0) %>%
  select(ObservationID)

# records without signal
#NoSignal <-DataAllBuildings[NoSignalIDs$ObservationID,]

# remove records without signal from data
DataAllBuildings <- DataAllBuildings[-NoSignalIDs$ObservationID,]

# find WAP's with maximum signal per observationID
MaxWap <- MaxSignals %>% 
            filter(MaxSignal!=0) %>% 
            left_join(VertData,c("ObservationID"="ObservationID",
                                 "MaxSignal"="WAPSignal")) %>%
            select (ObservationID, MaxSignal, WAP, LONGITUDE, LATITUDE, 
                    BUILDINGID, FLOOR)


# Add WAP with maximum signal to full dataset. 
# if more then 1 WAP with max signal, system chooses 1 
MaxWapObs <- MaxWap %>% 
                group_by(ObservationID) %>% 
                filter(row_number()==1) %>% 
                select(ObservationID,WAP) %>% 
                rename(MaxWap = WAP) 
DataAllBuildings <- DataAllBuildings %>% left_join(MaxWapObs,"ObservationID")

# Minimum level relevant
#DataAllBuildings <- ApplyMinimumSignal(DataAllBuildings,20)  

#define dataset for modelling
DataModel <-DataAllBuildings

#create training and test set
set.seed(455)
trainSize<-round(nrow(DataModel)*0.75) 
training_indices<-sample(seq_len(nrow(DataModel)),size =trainSize)

# create training and test datasets
training <- DataModel[training_indices,]
testing <- DataModel[-training_indices,]


#################################################

##### Linear Regression for MAX WAP


# List WAP's in test and train-set which are a maximum (and need to be trained)
TestingMaxWAP   <-  testing %>% 
                      group_by(MaxWap) %>% 
                      summarise(TimesTest = n())
TrainingMaxWAP  <-  training %>% 
                      group_by(MaxWap) %>% 
                      summarise(TimesTrain = n())
# add them together
TestingMaxWAP   <-  TestingMaxWAP %>% 
                      left_join(TrainingMaxWap,"MaxWap")

# loop through all maximum WAP's of testing set
LoopRow <- 1
while(LoopRow<=nrow(TestingMaxWAP)) {
  # set WAP for which to do modelling and predicting
  WAP = as.character(TestingMaxWAP[LoopRow,1])
  print(WAP)
  # reduce trainingset based on current WAP
  Training1WAP <- ExtractData1WAP(training, WAP,0)
  print(WAP)
  # reduce testing set based on current WAP and structure training set
  TrainingAttributes <-colnames(Training1WAP)
  Testing1WAP <- FilterTestingForWAP(testing, TrainingAttributes,WAP)
  #apply linear model
  RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                       SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP )
  RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap -
                       SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP, Training1WAP)
  # predict values for test set
  RegPredictLong <- predict(RegModelLong,Testing1WAP)
  RegPredictLat <- predict(RegModelLat,Testing1WAP)  
  Testing1WAPBuilding <- CalculatePredictedBuilding(Testing1WAP, RegPredictLong, RegPredictLat)
  Testing1WAPSelection <- Testing1WAPBuilding %>% 
                            select(ObservationID, PredictLong, PredictLat, LongError, LatError,
                                   PredictBuilding, BuildingError)
  ifelse(LoopRow==1,
    TestingAllWAP <- Testing1WAPSelection,
    TestingAllWAP <- rbind(TestingAllWAP, Testing1WAPSelection))
  LoopRow=LoopRow+1
}
TestingRegWAPBuilding <- testing %>% left_join(TestingAllWAP,"ObservationID")


#################################################

##### Linear Regression Full Dataset

#define dataset for modelling
DataModel <-DataAllBuildings

#remove attributes not needed for modelling
DataModel<- DataModel[, !names(DataModel) %in% 
                                c( "SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")]


#apply linear model
RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap,
                   training )
RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID - MaxWap, 
                  training)

# predict values for test set
RegPredictLong <- predict(RegModelLong,testing)
RegPredictLat <- predict(RegModelLat,testing)

#evaluate predicted values for test set
postResample(testing$LONGITUDE,RegPredictLong)
postResample(testing$LATITUDE,RegPredictLat)

# Calculate errors and building
TestingRegBuilding <- CalculatePredictedBuilding(testing, RegPredictLong, RegPredictLat)

#################################################

##### KNN Full Dataset


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
TestingResults <- TestingRegWAPBuilding
#TestingResults <- TestingKNNBuilding

# count errors per building
TestingResults %>% group_by(BUILDINGID) %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())

# count errors in total
TestingResults %>% 
  summarise(obs =n(), Buildingerrors=sum(BuildingError), rate=1-sum(BuildingError)/n())


# filter data on location
TestingResults %>% filter(LONGITUDE> -7450 & LONGITUDE< -7370 & 
                            LATITUDE>4864800 & LATITUDE<4864820 & BuildingError==1 )

# errors per specific location
TestingResults %>% group_by(LONGITUDE, LATITUDE, BuildingError) %>% 
  summarise(number =n()) %>% 
  filter(LONGITUDE> -7450 & LONGITUDE< -7370 & 
           LATITUDE>4864800 & LATITUDE<4864820 & BuildingError==1 )


# visualize errors

# actual location building
ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=LONGITUDE, y=LATITUDE, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)
  #xlim(-7600,-7200) +
  #ylim(4864600, 4865000)


#predicted location
ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=PredictLong, y=PredictLat, color=BuildingError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")+ 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)

#R2 Longitude & latitude
cor(TestingResults$LONGITUDE,TestingResults$PredictLong)^2
cor(TestingResults$LATITUDE,TestingResults$PredictLat)^2

# Results per building
ResultsB0 <- TestingResults %>% filter(BUILDINGID==0)
ResultsB1 <- TestingResults %>% filter(BUILDINGID==1)
ResultsB2 <- TestingResults %>% filter(BUILDINGID==2)

cor(ResultsB0$LATITUDE,ResultsB0$PredictLat)^2
cor(ResultsB1$LATITUDE,ResultsB1$PredictLat)^2
cor(ResultsB2$LATITUDE,ResultsB2$PredictLat)^2


# ideas:
# Convert signal to log
# Only include strong signals


# to do:
# user 6 & 14
# predict floor
# validation data
# source function
# Rmarkdown

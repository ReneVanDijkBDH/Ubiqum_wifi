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
#trainingVert <- ConvertToVerticalData(training)
#saveRDS(trainingVert,'../Data/clean_data/trainingVert.rds')
trainingVert <- readRDS('../Data/clean_data/trainingVert.rds')
#TrainVertTop10 <- VertTop10(TrainingVert)
#saveRDS(TrainVertTop10,'../Data/clean_data/TrainVertTop10.rds')

#Create list of WAP's
WAPList <- CreateWAPList(trainingVert)

# identify that WAP to use for modelling
training <- RankTraining(training, trainingVert)
MaxWAPCount <- training %>% group_by(MaxWap) %>% summarise(MaxCount=n())
saveRDS(MaxWAPCount,'../Data/clean_data/MaxWAPCount.rds')

# Creat model for Long and Lat 
#ModelList <- CreateRegressionModels(training)



# Vertical data-set of Testing
#testingVert  <- ConvertToVerticalData(testing)
TestingVert <- readRDS('../Data/clean_data/testingVert.rds')
#TestingVertTop10 <- VertTop10(TestingVert)
#saveRDS(TestingVertTop10,'../Data/clean_data/TestingVertTop10.rds')

# Predict Long, Lat and calculate Building
ModelList <-readRDS('../Data/clean_data/.rds')
testing <- RankTesting(testing, testingVert,MaxWAPCount)
#testingResult <- ApplyRegressionModels(testing, ModelList)


testingResult <- PredictLongLatBuilding(TestingVertTop10,WAPList) 

########FLOOR WAP
# Extend Vertical dataframe
TrainingVertExtended <- CreateExtendedVertical(trainingVert, WAPList)

FloorModelWAPKNN <- vector(mode="list", length=520)
#FloorModelWAPKNN <- CreateFloorModelWAP(TrainingVertExtended, FloorModelWAPKNN, "KNN")
saveRDS(FloorModelWAPKNN,'../Data/clean_data/FloorModelWAPKNN.rds')

FloorModelWAPRF <- vector(mode="list", length=520)
FloorModelWAPRF <- CreateFloorModelWAP(TrainingVertExtended, FloorModelWAPRF, "RF")
saveRDS(FloorModelWAPRF,'../Data/clean_data/FloorModelWAPRF.rds')
#error 388

TestingVertTop10 <- readRDS('../Data/clean_data/TestingVertTop10.rds')
testingResults <- readRDS('../Data/clean_data/result test obv WAP met div filters.rds')

TestingVertTop10Extended <- TestingVertTop10 %>% 
  left_join(testingResults %>% select(ObservationID,PredictLong, PredictLat), "ObservationID")
TestingVertTop10Extended$BUILDINGID<- as.factor(TestingVertTop10Extended$BUILDINGID)

TestingFloorResults <- ApplyFloorModelWAP(TestingVertTop10Extended, WAPList)
TestingFloorResults <- TestingFloorResults %>% left_join(testingResults %>% 
                                      select(ObservationID,PredictLong,PredictLat,PredictBuilding),
                                        "ObservationID")

#saveRDS(TestingFloorResults,'../Data/clean_data/TestingFloorResults WAPKNN 969.rds')





######################################################

#create models for floor: KNN
FloorModelB0 <- vector(mode="list", length=520)
FloorModelB0 <- CreateFloorModel(training, FloorModelB0, 0, "KNN")
saveRDS(FloorModelB0,'../Data/clean_data/FloorModelB0.rds')

FloorModelB1<- vector(mode="list", length=520)
FloorModelB1 <- CreateFloorModel(training, FloorModelB1, 1, "KNN")
saveRDS(FloorModelB1,'../Data/clean_data/FloorModelB1.rds')

FloorModelB2 <- vector(mode="list", length=520)
FloorModelB2 <- CreateFloorModel(training, FloorModelB2, 2, "KNN" )
saveRDS(FloorModelB2,'../Data/clean_data/FloorModelB2.rds')

#create models for floor: SVM
FloorModelB0SVM <- vector(mode="list", length=520)
FloorModelB0SWM  <- CreateFloorModel(training, FloorModelB0SVM, 0, "SVM")
saveRDS(FloorModelB0SVM,'../Data/clean_data/FloorModelB0SVM.rds')
# probleem WAP447

FloorModelB1SVM <- vector(mode="list", length=520)
FloorModelB1SWM  <- CreateFloorModel(training, FloorModelB1SVM, 1, "SVM")
saveRDS(FloorModelB1SVM,'../Data/clean_data/FloorModelB1SVM.rds')

#create models for floor: Random Forest
FloorModelB0RF <- vector(mode="list", length=520)
FloorModelB0RF  <- CreateFloorModel(training, FloorModelB0RF, 0, "RF")
saveRDS(FloorModelB0RF,'../Data/clean_data/FloorModelB0RF.rds')

FloorModelB1RF <- vector(mode="list", length=520)
FloorModelB1RF  <- CreateFloorModel(training, FloorModelB1RF, 1, "RF")
saveRDS(FloorModelB1RF,'../Data/clean_data/FloorModelB1RF.rds')

FloorModelB2RF <- vector(mode="list", length=520)
FloorModelB2RF  <- CreateFloorModel(training, FloorModelB2RF, 2, "RF")
saveRDS(FloorModelB2RF,'../Data/clean_data/FloorModelB2RF.rds')





#Predict Floor
TestingVertTop10 <- readRDS('../Data/clean_data/TestingVertTop10.rds')
TestingFloorResults <- ApplyFloorModel(TestingVertTop10)
saveRDS(TestingFloorResults,'../Data/clean_data/TestingFloorResults.rds')






##################################

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
RegModelLong <- lm(LONGITUDE ~ . - LATITUDE - FLOOR - BUILDINGID - ObservationID ,
                   training )
RegModelLat <- lm(LATITUDE ~ . -LONGITUDE - FLOOR - BUILDINGID - ObservationID , 
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

# ideas:
# Convert signal to log
# Only include strong signals


# to do:
# user 6 & 14
# predict floor
# validation data
# source function
# Rmarkdown

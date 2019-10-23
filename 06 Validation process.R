DataValidation <- read.csv('../Data/validationData.csv')

DataProcessing <- DataValidation

# add unique row ID for reference
DataProcessing$ObservationID <- seq_len(nrow(DataProcessing))

# rescale RSSI value
DataProcessing <- RescaleRSSI(DataProcessing)

# create vertical dataframe for analysing and query purposes
#validationVert <- ConvertToVerticalData(DataProcessing)

validationVert <- readRDS('../Data/clean_data/ValidationVert.rds')
MaxWAPCount <- readRDS('../Data/clean_data/MaxWAPCount.rds')

#OLD
#DataProcessing <- RankTesting(DataProcessing, validationVert,MaxWAPCount)
#ModelList <-readRDS('../Data/clean_data/ModelList.rds')
#validationResult <- ApplyRegressionModels(DataProcessing, ModelList)  


  


# remove records without signal, identify WAP with max signal 
#DataProcessing <- ProcessSignals(DataProcessing,VertData)

#ValidationResult <- LinearRegressionMaxWAPModel(training, DataProcessing)


ValidationVert <- readRDS('../Data/clean_data/validationVert.rds')
#ValidationVertTop10 <- VertTop10(ValidationVert)
#saveRDS(ValidationVertTop10,'../Data/clean_data/ValidationVertTop10.rds')


ValidationVertTop10 <- readRDS('../Data/clean_data/ValidationVertTop10.rds')
validationResults <- PredictLongLatBuilding(ValidationVertTop10,WAPList)
saveRDS(validationResults,'../Data/clean_data/validationResult.rds' )


validationResults <- readRDS('../Data/clean_data/result test obv WAP met div filters.rds')

ValidationVertTop10Ext <- ValidationVertTop10 %>% 
  left_join(validationResults %>% select(ObservationID,PredictLong, PredictLat), "ObservationID")
ValidationVertTop10Ext$BUILDINGID<- as.factor(ValidationVertTop10Ext$BUILDINGID)

ValidationFloorResults <- ApplyFloorModelWAP(ValidationVertTop10Ext, WAPList)
saveRDS(ValidationFloorResults,'../Data/clean_data/ValidationFloorResults.rds')

ValidationFloorResults$PredictedFloor <- as.factor(ValidationFloorResults$PredictedFloor)

ValidationFloorResults <- ValidationFloorResults %>% left_join(validationResults %>% 
                                       select(ObservationID,PredictLong,PredictLat,PredictBuilding),
                                     "ObservationID")
confusionMatrix(ValidationFloorResults$FLOOR, ValidationFloorResults$PredictedFloor)



TrainingVertExtended <- CreateExtendedVertical(TrainingVert, WAPList)



#Predict Floor
#ValidationVertTop10 <- readRDS('../Data/clean_data/ValidationVertTop10.rds')
#ValidationFloorResults <- ApplyFloorModel(ValidationVertTop10)
#saveRDS(ValidationFloorResults,'../Data/clean_data/ValidationFloorResults.rds')
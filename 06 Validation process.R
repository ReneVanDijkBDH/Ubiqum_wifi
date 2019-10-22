DataValidation <- read.csv('../Data/validationData.csv')

DataProcessing <- DataValidation

# add unique row ID for reference
DataProcessing$ObservationID <- seq_len(nrow(DataProcessing))

# rescale RSSI value
DataProcessing <- RescaleRSSI(DataProcessing)

# create vertical dataframe for analysing and query purposes
#validationVert <- ConvertToVerticalData(DataProcessing)

DataProcessing <- RankTesting(DataProcessing, validationVert,MaxWAPCount)
testingResult <- ApplyRegressionModels(DataProcessing, ModelList)  

# remove records without signal, identify WAP with max signal 
#DataProcessing <- ProcessSignals(DataProcessing,VertData)

#ValidationResult <- LinearRegressionMaxWAPModel(training, DataProcessing)


ValidationVert <- readRDS('../Data/clean_data/validationVert.rds')
#ValidationVertTop10 <- VertTop10(ValidationVert)
#saveRDS(ValidationVertTop10,'../Data/clean_data/ValidationVertTop10.rds')


#Predict Floor
ValidationVertTop10 <- readRDS('../Data/clean_data/ValidationVertTop10.rds')
ValidationFloorResults <- ApplyFloorModel(ValidationVertTop10)
saveRDS(ValidationFloorResults,'../Data/clean_data/ValidationFloorResults.rds')





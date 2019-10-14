DataValidation <- read.csv('../Data/validationData.csv')

DataProcessing <- DataValidation

# add unique row ID for reference
DataProcessing$ObservationID <- seq_len(nrow(DataProcessing))

# rescale RSSI value
DataProcessing <- RescaleRSSI(DataProcessing)

# create vertical dataframe for analysing and query purposes
VertData <- ConvertToVerticalData(DataProcessing)

# remove records without signal, identify WAP with max signal 
DataProcessing <- ProcessSignals(DataProcessing,VertData)

ValidationResult <- LinearRegressionMaxWAPModel(training, DataProcessing)

#problem WAP158. no records in training

  
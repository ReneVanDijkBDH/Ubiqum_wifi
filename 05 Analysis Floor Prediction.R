FloorPrediction <- readRDS('../Data/clean_data/TestingFloorResults.rds')
FloorPrediction <- readRDS('../Data/clean_data/ValidationFloorResults.rds')

confusionMatrix(FloorPrediction$FLOOR, as.factor(FloorPrediction$PredictedFloor))

Building <- 0
FloorPrediction1B <- FloorPrediction %>% filter(BUILDINGID==Building)
pred = factor(FloorPrediction1B$PredictedFloor,levels=c("0","1","2","3"))
truth = factor(FloorPrediction1B$FLOOR,levels=c("0","1","2","3"))
confusionMatrix(truth, pred)

Building <- 2
FloorPrediction1B <- FloorPrediction %>% filter(BUILDINGID==Building)
pred = factor(FloorPrediction1B$PredictedFloor,levels=c("0","1","2","3","4"))
truth = factor(FloorPrediction1B$FLOOR,levels=c("0","1","2","3","4"))
confusionMatrix(truth, pred)

confusionMatrix(FloorPrediction1B$FLOOR, as.factor(FloorPrediction1B$PredictedFloor))

FloorPrediction %>% group_by(BUILDINGID, FLOOR) %>% summarise(n())
FloorPrediction %>% group_by(BUILDINGID, PredictedFloor) %>% summarise(n())

FloorPrediction1B %>% filter(PredictedFloor==3 & FLOOR == 4)
FloorPrediction1B$FLOOR <- as.integer(FloorPrediction1B$FLOOR)
FloorPrediction <- readRDS('../Data/clean_data/TestingFloorResults.rds')
FloorPrediction <- readRDS('../Data/clean_data/ValidationFloorResults.rds')
FloorPrediction <- ValidationFloorResults
FloorPrediction <- TestingFloorResults

FloorPrediction %>% group_by(PredictBuilding, FLOOR) %>% 
              summarise(aant=n(), 
              fout=sum(FloorError),
              errorperc=1-sum(FloorError)/n())

FloorPrediction %>% 
  summarise(aant=n(), 
            fout=sum(FloorError),
            errorperc=1-sum(FloorError)/n())

confusionMatrix(FloorPrediction$FLOOR, as.factor(FloorPrediction$PredictedFloor))


pred = factor(FloorPrediction$PredictedFloor,levels=c("0","1","2","3"))
truth = factor(FloorPrediction$FLOOR,levels=c("0","1","2","3"))
confusionMatrix(truth, pred)



FloorPrediction %>% filter(FloorError==1)

TestingVertTop10Extended %>% filter(ObservationID==123)
WAPList %>% filter(WAP %in% c("WAP167","WAP166","WAP184","WAP123","WAP124"))

FloorModel <-readRDS('../Data/clean_data/FLOORModelWAPKNN.rds')

FloorPrediction1B %>% filter(PredictedFloor==3 & FLOOR == 4)
FloorPrediction1B$FLOOR <- as.integer(FloorPrediction1B$FLOOR)

#predicted location
ggplot(FloorPrediction, 
       aes(x=PredictLong, y=PredictLat, color=FloorError)) +
  geom_point(size=2, shape=1) + 
  scale_color_gradient(low="grey", high="red")+ 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100) +  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))
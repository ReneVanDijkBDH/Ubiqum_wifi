##Analyse errors

TestingResults <- TestingRegBuilding
TestingResults <- TestingRegWAPBuilding
TestingResults <- testingResult
TestingResults <- PredictObs
TestingResults <- readRDS('../Data/clean_data/TestingResults.rds')
TestingResults <- readRDS('../Data/clean_data/result test obv WAP zonder filter.rds')
TestingResults <- readRDS('../Data/clean_data/result test obv WAP met div filters.rds')
TestingResults <- validationResults

#TestingResults <- TestingKNNBuilding

TestingResults %>% filter(is.na(BuildingError))
TestingResults <- TestingResults %>% filter(!is.na(BuildingError))

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
  geom_point(size=2, shape=1) + 
  scale_color_gradient(low="grey", high="red")+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)+  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))

#xlim(-7600,-7200) +
#ylim(4864600, 4865000)


#predicted location
ggplot(TestingResults %>% filter(abs(LongError) > 0 ), 
       aes(x=PredictLong, y=PredictLat, color=BuildingError)) +
  geom_point(size=2, shape=1) + 
  scale_color_gradient(low="grey", high="red")+ 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+
  xlim(-7700,-7300) +
  ylim(4864700, 4865100) +  
  theme(panel.background = element_blank(),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))

# error distribution
hist(TestingResults$LongError)
hist(TestingResults$LatError)

#R2 Longitude & latitude
cor(TestingResults$LONGITUDE,TestingResults$PredictLong)^2
cor(TestingResults$LATITUDE,TestingResults$PredictLat)^2

#Root Mean Square error
RMSE(TestingResults$LONGITUDE,TestingResults$PredictLong)
RMSE(TestingResults$LATITUDE,TestingResults$PredictLat)



#standard deviation
sd(TestingResults$LongError)
sd(TestingResults$LatError)

#Euclidean Distance
sqrt(sum((TestingResults$LONGITUDE-TestingResults$PredictLong)^2))
sqrt(sum((TestingResults$LATITUDE-TestingResults$PredictLat)^2))

errorRange <- TestingResults %>% filter(LatError>-50 & LatError<50)
errorRange <- TestingResults %>% filter(abs(LatError)>20)
hist(errorRange$LatError)

# Plot Long error vc Lat error
ggplot(TestingResults, 
       aes(x=LongError, y=LatError, color=BUILDINGID)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="blue", high="red") + 
  xlim(-100,100) +
  ylim(-100, 100)



# Results per building
ResultsB0 <- TestingResults %>% filter(BUILDINGID==0)
ResultsB1 <- TestingResults %>% filter(BUILDINGID==1)
ResultsB2 <- TestingResults %>% filter(BUILDINGID==2)

cor(ResultsB0$LATITUDE,ResultsB0$PredictLat)^2
cor(ResultsB1$LATITUDE,ResultsB1$PredictLat)^2
cor(ResultsB2$LATITUDE,ResultsB2$PredictLat)^2


cor(ResultsB0$LONGITUDE,ResultsB0$PredictLong)^2
cor(ResultsB1$LONGITUDE,ResultsB1$PredictLong)^2
cor(ResultsB2$LONGITUDE,ResultsB2$PredictLong)^2

# Plot Lat-Error by LONGITUDE
ggplot(TestingResults, 
       aes(x=LONGITUDE, y=LatError, color=LongError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="green", high="red") + 
  theme(panel.background = element_blank())
  
arrange(TestingResults %>% filter(abs(LatError)>40), -abs(LatError))
arrange(TestingResults %>% filter(abs(LongError)>40), -abs(LongError))

VDataTop10 %>% filter(ObservationID==10133)


# Plot error by Max Signal
ggplot(TestingResults, 
       aes(x=MaxSignal, y=LatError, color=LongError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="green", high="red") + 
  theme(panel.background = element_blank())

ggplot(TestingResults, 
       aes(x=MaxSignal, y=LongError, color=LatError)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="green", high="red") + 
  theme(panel.background = element_blank())


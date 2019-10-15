B0 <- training %>% filter(BUILDINGID==0)
B1 <- training %>% filter(BUILDINGID==1)
B2 <- training %>% filter(BUILDINGID==2)
  
B0 <- B0[,-nearZeroVar(B0)]
B1 <- B1[,-nearZeroVar(B1)]
B2 <- B2[,-nearZeroVar(B2)]

B0$FLOOR <- as.factor(B0$FLOOR)

  set.seed(456)
trctrl <- trainControl(method="repeatedcv",repeats = 1) 

#B0Select <- B0 %>% select(WAP500,WAP404,WAP388,WAP380,WAP325, FLOOR)

KNNModelFloor <- train(FLOOR ~ .- LATITUDE - LONGITUDE  -
                         SPACEID - RELATIVEPOSITION - USERID - PHONEID - TIMESTAMP - 
                         - ObservationID - MaxWap - WAPSignal - ranking , 
                       data = B0, 
                       method = "knn", 
                       trControl = trctrl, 
                       preProcess = c("center","scale"), 
                       tuneLength = 10)

KNNModelFloor <- train(FLOOR ~ . , 
                       data = B0Select, 
                       method = "rf", 
                       trControl = trctrl, 
                       preProcess = c("center","scale"), 
                       tuneLength = 10)
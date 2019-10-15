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
                       method = "knn", 
                       trControl = trctrl, 
                       preProcess = c("center","scale"), 
                       tuneLength = 10)

#KNN prediction & results
KNNPredictFloor <- predict(KNNModelFloor, newdata = B0Testing)

B0Testing <- testing %>% filter(BUILDINGID==0 & MaxWap!="WAP248")

postResample(B0Testing$FLOOR,KNNPredictFloor)

FloorWAP <- trainingVert %>% filter(BUILDINGID==0 & WAPSignal!=0) %>% group_by(WAP,FLOOR) %>% 
       summarise(tot=n(),avg=mean(WAPSignal)) #%>% filter(avg!=0)

B0Select$Top1F0 <- B0Select %>% left_join(FloorWAP %>% filter(FLOOR==0),by=c("MaxWap"="WAP")) %>% 
  select(avg) %>% rename("Top1F0" = "avg")

B0Select$Top1F1 <- B0Select %>% left_join(FloorWAP %>% filter(FLOOR==1),by=c("MaxWap"="WAP")) %>% 
  select(avg) %>% rename("Top1F1" = "avg")

B0Select$Top1F2 <- B0Select %>% left_join(FloorWAP %>% filter(FLOOR==2),by=c("MaxWap"="WAP")) %>% 
  select(avg) %>% rename("Top1F2" = "avg")

B0Select$Top1F3 <- B0Select %>% left_join(FloorWAP %>% filter(FLOOR==3),by=c("MaxWap"="WAP")) %>% 
  select(avg) %>% rename("Top1F3" = "avg")


B0Select[is.na(B0Select)] <- 0 
B0Select$Top1F0[is.na(B0Select$Top1F0)] <-0
B0Select$Top1F1[is.na(B0Select$Top1F1)] <-0
B0Select$Top1F2[is.na(B0Select$Top1F2)] <-0
B0Select$Top1F3[is.na(B0Select$Top1F3)] <-0

B0Select$Top1F0 <- as.numeric(B0Select$Top1F0)


head(trainingVert %>% filter(BUILDINGID==0) %>% group_by(WAP) %>% summarise(n(),mean(WAPSignal)),20)


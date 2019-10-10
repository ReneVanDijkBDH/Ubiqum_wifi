# 
VDataNoZero   <- VertData %>% filter(WAPSignal!=0)
hist(VDataNoZero$WAPSignal)
HighBorder    <- 84
VDataHigh     <- VDataNoZero %>% filter(WAPSignal>HighBorder)
hist(VDataHigh$WAPSignal)

summary(VDataHigh)

# group data
VDataHigh %>% group_by(USERID) %>% summarise(aant=n())
VDataHigh %>% group_by(WAP) %>% summarise(aant=n())
VDataHigh %>% group_by(PHONEID) %>% summarise(aant=n())

# inspect specific users
user <-6
VDataUser6 <- VertData %>% filter(USERID==user & WAPSignal!=0)
hist(VDataUser6$WAPSignal)

user <-14
VDataUser14 <- VertData %>% filter(USERID==user & WAPSignal!=0)
hist(VDataUser14$WAPSignal)

VDataUser14 %>% filter(WAPSignal>HighBorder)
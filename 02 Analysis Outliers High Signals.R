# 
VDataNoZero   <- VData %>% filter(WAPSignal!=0)
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

ggplot(VData %>% filter(USERID==6 & WAPSignal>0 ), 
       aes(x=LONGITUDE, y=WAPSignal, color=BUILDINGID)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red") +  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"))


user <-14
VDataUser14 <- VertData %>% filter(USERID==user & WAPSignal!=0)
hist(VDataUser14$WAPSignal)

VDataUser14 %>% filter(WAPSignal>HighBorder)

# 
ggplot(VData %>% filter(USERID==14 & WAPSignal>0 ), 
       aes(x=LONGITUDE, y=WAPSignal, color=BUILDINGID)) +
  geom_point(size=2, shape=23) + 
  scale_color_gradient(low="grey", high="red")


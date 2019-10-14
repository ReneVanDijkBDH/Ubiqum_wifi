# Graphs

# all builings
ggplot(DataAllBuildings, aes(x=LONGITUDE, y=LATITUDE, color=BUILDINGID)) +
  geom_point(size=2, shape=23) + 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+ 
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)

# Building 0
ggplot(DataBuilding0, aes(x=LONGITUDE, y=LATITUDE, color=FLOOR)) +
  geom_point(size=2, shape=23)

# Building 1
ggplot(DataBuilding1, aes(x=LONGITUDE, y=LATITUDE, color=FLOOR)) +
  geom_point(size=2, shape=23)

# Building 2
ggplot(DataBuilding2, aes(x=LONGITUDE, y=LATITUDE, color=FLOOR)) +
  geom_point(size=2, shape=23)

# correlation matrix
corrData <- cor(DataModel)
corrplot(corrData)

# position of MAxWap's
ggplot(training, aes(x=LONGITUDE, y=LATITUDE, color=MaxWap)) +
  geom_point(size=2, shape=23) + 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+ 
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)+
  theme(legend.position = "none")


# position of MAxWap's - specific location
ggplot(training %>% filter(LONGITUDE< -7600 & LATITUDE>4864900), aes(x=LONGITUDE, y=LATITUDE, color=MaxWap)) +
  geom_point(size=2, shape=23) + 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+ 
  xlim(-7700,-7600) +
  ylim(4864900, 4865100)+
  theme(legend.position = "none")

# position of MAxWap's - specific WAP's
ggplot(training %>% filter(MaxWap=="WAP075"), aes(x=LONGITUDE, y=LATITUDE, color=MaxWap)) +
  geom_point(size=2, shape=23) + 
  geom_abline(intercept = 4876000, slope = 1.506, color="blue", size=0.5)+ 
  geom_abline(intercept = 4876350, slope = 1.506, color="blue", size=0.5)+ 
  xlim(-7700,-7300) +
  ylim(4864700, 4865100)+
  theme(legend.position = "none")
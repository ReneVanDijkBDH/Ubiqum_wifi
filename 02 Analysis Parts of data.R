# Divide dataset in smaller sets per building
#DataBuilding0 <- ExtractData1Building(DataAllBuildings, BuildingID = 0)
#DataBuilding1 <- ExtractData1Building(DataAllBuildings, BuildingID = 1)
#DataBuilding2 <- ExtractData1Building(DataAllBuildings, BuildingID = 2)

# summarize data
#DataB0_FloorSpaces <- DataBuilding0 %>% group_by(FLOOR, SPACEID) %>% summarise(obs=n())

# select small part of data
#Spacelist <- c(106,116,120)
#Spacelist <- c(106,116,120,107,110,111,112)
#Spacelist <- c(106,116,120,107,110,111,112,113,114,115)
#DataB0_F0_Spaces <- ExtractData1FloorSpace(DataBuilding0, FloorID = 0,Spacelist)

# rescale RSSI value
#DataB0_F0_Spaces <- RescaleRSSI(DataB0_F0_Spaces)
#DataBuilding0 <- RescaleRSSI(DataBuilding0)
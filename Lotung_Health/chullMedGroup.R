#apply chull to MedGroup

rm(list=ls())

library(rgdal)
library(leaflet)

Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre1 <- subset(Addre, Addre$HomeDocGroupID == 1340202042)
GroupID <- unique(Addre1$HomeDocGroupID)
GroupName <- unique(Addre1$HomeDocGroupName)
GroupN <- length(GroupID)

m <- leaflet(Addre1) %>%
  addTiles() %>%
  addCircleMarkers(~Response_X, ~Response_Y, radius = 16, stroke = F, color="coral", fillOpacity = 0.6)

for (i in 1:1){
  C <- Addre1[Addre1$HomeDocGroupID %in% GroupID[i],2:3]
  hpts <- chull(C)
  C <- C[hpts,]
  m <- m %>% addPolygons(data=C, ~Response_X, ~Response_Y, weight = 1, color ="coral", fillColor = "coral", highlightOptions = highlightOptions(color = "coral", weight = 5), label = paste0(GroupName[i],"醫療群"), group = )
}

m

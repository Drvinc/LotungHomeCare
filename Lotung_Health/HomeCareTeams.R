# Home Care Teams Data



# Home Care MAP

library(leaflet)
library(rgdal)

rm(list=ls())

dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
py <- function(x){
  y <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% x])
  return(y)
}
Lotung <- py("Lotung")
PopuDensity65 <- py("PopuDensity65")
persons <- py("persons")
AllClin <- py("AllClin")
HICare <- py("HICare")
tenKM <- py("tenKM")
twoKM <- py("twoKM")
Hosps <- py("Hosps")

ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])
ILan_villag$VILLNAME <- dsl(ILan_villag$VILLNAME)

ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.character(townList[,3])

ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILan_villag$PopuDens65 <- as.numeric(ILanAgeData$PopuDens65)
ILan_villag$sum65 <- as.numeric(ILanAgeData$sum65abov)
ILan_villag$labelsDens65 <- paste0(ILan_villag$TOWNNAME, gsub("/","",ILan_villag$VILLNAME),": ",round(ILan_villag$PopuDens65,digits = 2)," (",ILan_villag$sum65,persons,")")

Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$ClinName <- dsl(Addre$ClinName)
AddreW <- Addre[Addre$ClinType %in% c(1,2),]
Addre_ILanGroup <- Addre[is.na(Addre$HomeDocGroupID) == FALSE,]
Addre_ILanHICare <- Addre[Addre$HomeInteCareGroupID1 != "",]
Addre_ILanHICare$HICarelabels <- paste0(Addre_ILanHICare$ClinName, ": ", Addre_ILanHICare$HomeInteCareGroupName1, ifelse(Addre_ILanHICare$HomeInteCareGroupName2 == "", "", paste0(", ", Addre_ILanHICare$HomeInteCareGroupName2)))

H_Addre <- read.csv("ILanHospFull.csv", fileEncoding = "UTF-8")
H_Addre$HospName <- dsl(H_Addre$HospName)

palDens65 <- colorBin("YlOrRd", domain = ILan_villag$PopuDens65, bins = c(0,85,174,383,1635,ceiling(max(ILan_villag$PopuDens65))))

leaflet(data=ILan_villag, options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  fitBounds(121.7475,24.6641,121.8018,24.7069) %>%
  addPolygons(fillColor = ~palDens65(PopuDens65), weight=0.1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8, label=~labelsDens65, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = palDens65, values = ~PopuDens65, opacity = 0.7,
            title = PopuDensity65, position = "bottomright") %>%
  addPolygons(data = ILan_town, fill = FALSE, weight = 2) %>%
  addCircles(data = Addre_ILanHICare, ~Response_X, ~Response_Y,
             radius = 2000, color = "green", stroke = T, weight = 3, dashArray = "10,10",
             fillOpacity = 0.05,
             group = twoKM) %>%
  addCircles(data = Addre_ILanHICare, ~Response_X, ~Response_Y,
             radius = 10000, color = "green", stroke = T, weight = 1, dashArray = "10,10",
             fillOpacity = 0.03,
             group = tenKM) %>%
  addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y,
                   radius = 2, color = "blue", stroke = T,
                   label = paste0(AddreW$ClinName),
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = AllClin) %>%
  addCircleMarkers(data = Addre_ILanHICare, ~Response_X, ~Response_Y,
                   radius = 8, color = "green", stroke = T, opacity = 0.8,
                   fillColor = "green", fillOpacity = 0.5,
                   label = ~HICarelabels,
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = HICare) %>%
  addCircleMarkers(data = H_Addre, ~Response_X, ~Response_Y,
                   label=~HospName, labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   radius = ~ifelse(Level == 1, 10, 12),
                   color = ~ifelse(Level == 1, "orange", "red"), stroke = T,
                   group = Hosps) %>%
  addLayersControl(
    #baseGroups = c("A","B"),
    overlayGroups = c(tenKM, twoKM, HICare, AllClin, Hosps),
    options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
  hideGroup(tenKM)


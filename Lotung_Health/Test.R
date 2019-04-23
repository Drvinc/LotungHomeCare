#install.packages("RColorBrewer")
rm(list=ls())

library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)

Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$Response_X <- as.numeric(Addre$Response_X)
Addre$Response_Y <- as.numeric(Addre$Response_Y)
Addre$ClinicName <- as.character(Addre$ClinicName)
Addre_LotungGroup <- subset(Addre, Addre$HomeDocGroupID == "1340202042")
Addre_ILanGroup <- subset(Addre, Addre$HomeDocGroupID != "")

HICare <- read.csv("HomeIntegratedCare.csv", fileEncoding = "UTF-8")

Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
MedGroup <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "MedGroup"])
HomeIntegratedCare <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "HomeIntegratedCare"])

#Merge HomeDocGroup and ILanClinicAddre
HDGroup <- read.csv("ILanHomeDocGroup.csv", fileEncoding = "UTF-8")
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$HomeDocGroupID <- ""
Addre$HomeDocGroupName <- ""
k <- 0
for(i in c(1:nrow(HDGroup))){
  ClinicName <- as.character(HDGroup$診所名稱[i])
  j <- which(Addre$ClinicName == ClinicName)
  if (j > 0){
    Addre$HomeDocGroupID[j] <- as.character(HDGroup$醫療群代號[i])
    Addre$HomeDocGroupName[j] <- as.character(HDGroup$醫療群名稱[i])
    k <- k + 1
  }
}
#Merge HomeInteCareGroup and ILanClinicAddre
HICare <- read.csv("HomeIntegratedCare.csv", fileEncoding = "UTF-8")
Addre$HomeInteCareGroupID1 <- ""
Addre$HomeInteCareGroupName1 <- ""
Addre$HomeInteCareGroupID2 <- ""
Addre$HomeInteCareGroupName2 <- ""
Addre$HomeInteCareGroupID3 <- ""
Addre$HomeInteCareGroupName3 <- ""
k <- 0
for(i in c(1:nrow(HICare))){
  ClinicName <- as.character(HICare$醫事機構名稱[i])
  j <- which(Addre$ClinicName == ClinicName)
  if ((length(j) > 0) & (substr(HICare$醫事機構地址[i], start=1, stop=2)=="宜蘭")){
    k <- k + 1
    if (Addre$HomeInteCareGroupID1[j] == ""){
      Addre$HomeInteCareGroupID1[j] <- as.character(HICare$整合團隊代碼[i])
      Addre$HomeInteCareGroupName1[j] <- as.character(HICare$整合團隊名稱[i])
    }else{
      if (Addre$HomeInteCareGroupID2[j] == ""){
        Addre$HomeInteCareGroupID2[j] <- as.character(HICare$整合團隊代碼[i])
        Addre$HomeInteCareGroupName2[j] <- as.character(HICare$整合團隊名稱[i])
      }else{
        Addre$HomeInteCareGroupID3[j] <- as.character(HICare$整合團隊代碼[i])
        Addre$HomeInteCareGroupName3[j] <- as.character(HICare$整合團隊名稱[i])
      }
    }
  }
}
write.csv(Addre, "Test.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Map
outline <- quakes[chull(quakes$long, quakes$lat),]

map <- leaflet(quakes) %>%
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )
map

#readSHP
ILan_villag <- readOGR(dsn = "ILan_villages", layer = "ILan_villages")
Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")
villagList <- read.csv("LotungVillTable.csv", fileEncoding = "UTF-8")
Lotung_villag$VILLNAME <- as.character(villagList[,4])
#ReadClinicData
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$Response_X <- as.numeric(Addre$Response_X)
Addre$Response_Y <- as.numeric(Addre$Response_Y)
Addre$ClinicName <- as.character(Addre$ClinicName)

Lotung_villag$VILLNAME
leaflet() %>%
  addTiles() %>%
  addPolygons(data=Lotung_villag, weight=1, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.08, label=~as.character(Lotung_villag$VILLNAME))
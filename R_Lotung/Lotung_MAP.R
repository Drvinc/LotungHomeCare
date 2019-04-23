library(rgdal)
library(leaflet)

rm(list=ls())

#get ILan village shapefiles
#villag <- readOGR(dsn = "TWN_villages", layer = "VILLAGE_MOI_1051214", use_iconv = TRUE, encoding = "UTF-8")
villag <- readOGR(dsn = "TWN_villages", layer = "VILLAGE_MOI_1060503", use_iconv = TRUE, encoding = "UTF-8")
ILan_villag <- subset(villag, villag$COUNTYID == "G")
plot(ILan_villag)

writeOGR(ILan_villag, ".", "ILan_villages", driver="ESRI Shapefile", encoding= "UTF-8")

#ILan TOWN shapefiles
town <- readOGR(dsn = "TWN_towns", layer = "TOWN_MOI_1060503", use_iconv = TRUE, encoding = "UTF-8")
ILan_town <- subset(town, town$COUNTYID == "G")
plot(ILan_town)

writeOGR(ILan_town, ".", "ILan_towns", driver="ESRI Shapefile", encoding= "UTF-8")

#get Ilan village Codes and Names
TownID <- as.character(ILan_villag$TOWNID)
TownCode <- as.character(ILan_villag$TOWNCODE)
TownName <- as.character(ILan_villag$TOWNNAME)
VillagCode <- as.numeric(ILan_villag$VILLCODE)
VillagEng <- as.character(ILan_villag$VILLENG)
VillagName <- as.character(ILan_villag$VILLNAME)
ILanVillagTable <- cbind(TownID, TownCode, TownName, VillagCode, VillagEng, VillagName)

write.csv(ILanVillagTable, "ILanVillTable.csv", fileEncoding = "UTF-8")

#get Lotung village shapefiles
Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")
Villag1 <- subset(Lotung_villag, as.numeric(Lotung_villag$VILLCODE) == 39)
VillagCode <- as.numeric(Lotung_villag$VILLCODE)
VillagEng <- as.character(Lotung_villag$VILLENG)
VillagName <- as.character(Lotung_villag$VILLNAME)
LotungVillagTable <- cbind(VillagCode, VillagEng, VillagName)
write.csv(LotungVillagTable,"LotungVillTable.csv",fileEncoding = "UTF-8")
read.csv("LotungVillTable.csv", fileEncoding = "UTF-8")
#plot(Lotung_villag)
plot(Lotung_villag, map.textlabels=as.character(Lotung_villag$VILLENG))

leaflet(Lotung_villag) %>% addTiles() %>%
  addPolygons(data=Lotung_villag, label=~as.character(Lotung_villag$VILLENG))

#ILanClinicAddre <- read.csv("ILanClinicAddress.csv", sep =",")
#ILanClinicList <- read.csv("ILanClinic.csv", sep=",")
#Addre <- ILanClinicAddre
#Addre$Response_X <- as.numeric(sub("_.*", "", Addre$Response_X))
#Addre$Response_Y <- as.numeric(sub("_.*", "", Addre$Response_Y))
#Addre$ClinicName <- as.character(ILanClinicList$Name)
#write.csv(Addre,"ILanClinicFull.csv")

Addre <- read.csv("ILanClinicFull.csv",fileEncoding = "UTF-8")
Addre <- Addre[Addre$Type == 1,]
Addre$Response_X <- as.numeric(Addre$Response_X)
Addre$Response_Y <- as.numeric(Addre$Response_Y)
Addre$ClinicName <- as.character(Addre$ClinicName)

leaflet(Addre) %>%
  addTiles() %>%
  #addPolygons(data=Lotung_villag, weight=1, smoothFactor = 0.5) %>%
  addMarkers(~Response_X, ~Response_Y, popup=~as.character(ClinicName), clusterOptions = markerClusterOptions())


#get Ilan Town Codes and Names
TownID <- as.character(ILan_town$TOWNID)
TownCode <- as.character(ILan_town$TOWNCODE)
TownName <- as.character(ILan_town$TOWNNAME)
TownEng <- as.character(ILan_town$TOWNENG)
ILanTownTable <- cbind(TownID, TownCode, TownName, TownEng)

write.csv(ILanTownTable, "ILanTownTable.csv", fileEncoding = "UTF-8", row.names=FALSE)
ILanTownTable <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")


#ILan Map with spplot
#ggcolor pellete
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
library(rgdal) #OGR-SHP operations
library(rgeos) #gCentroid
ILan_town <- readOGR(dsn = ".", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.factor(townList[,3])
col = gg_color_hue(12)
c1 = gCentroid(ILan_town, byid = TRUE)
for (i in 1:12){
  x <- ILan_town@polygons[[i]]@labpt[1]
  y <- ILan_town@polygons[[i]]@labpt[2]
  c1@coords[i,1] <- x
  c1@coords[i,2] <- y
}
sl <- list('sp.text', c1@coords, txt=ILan_town$TOWNNAME, cex=1)
spplot(ILan_town, "TOWNNAME", col="white", col.regions=col, colorkey = FALSE,
       xlim=c(121.301473, 122.037963), ylim=c(24.283926,25.015574),
       sp.layout = list(sl))

#get centroids
library(rgdal)
ILan_town <- readOGR(dsn = ".", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.factor(townList[,3])

for (i in 1:12){
  x <- ILan_town@polygons[[i]]@labpt[1]
  y <- ILan_town@polygons[[i]]@labpt[2]
  townList$labpt_x[i] <- as.numeric(x)
  townList$labpt_y[i] <- as.numeric(y)
}

write.csv(townList, "ILanTownTable.csv", fileEncoding = "UTF-8", row.names = FALSE)
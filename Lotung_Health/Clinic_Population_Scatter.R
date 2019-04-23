# Clinic and Aged Population
rm(list=ls())

library(dplyr)
library(ggplot2)
rm(list=ls())
ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
d <- ILanAgeData
d$ID <- seq.int(nrow(d))
d <- arrange(d, desc(PopuDens65)) %>%
  mutate(
    cumsum = cumsum(sum65abov),
    freq = round(sum65abov / sum(sum65abov), 3),
    cum_freq = cumsum(freq)
  )
d$DensIndex <- c("")
for (i in 1:nrow(d)){
  id <- ceiling(d$cum_freq[i]*5)
  d$DensIndex[i] <- if_else(id > 5, 5, id)
}

d <- arrange(d, ID)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

library(rgdal)
library(leaflet)
dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.factor(townList$TownName)
ILan_villag$DensIndex <- d$DensIndex
v40 <- subset(ILan_villag, ILan_villag$DensIndex %in% c(1,2))
v60 <- subset(ILan_villag, ILan_villag$DensIndex %in% c(1,2,3))

happyC <- read.csv("HappyLearnCentFull.csv", fileEncoding = "UTF-8")
happyC$STName <- dsl(happyC$STName)


Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$ClinName <- dsl(Addre$ClinName)
AddreW <- Addre[Addre$ClinType %in% c(1,2),]
for (i in 1:nrow(AddreW)){
  isINT <- sapply(c("家醫","內科","不分"),grepl,AddreW$Speci[i])
  AddreW$isIN[i] <- as.logical(any(isINT))
}
AddreIN <- AddreW[AddreW$isIN == TRUE,]
AddreGroup <- Addre[is.na(Addre$HomeDocGroupID) == FALSE,]
AddreHome <- Addre[Addre$HomeInteCareGroupID1 != "",]

pal <- colorFactor(palette = gg_color_hue(12), domain = ILan_town$TOWNNAME)

leaflet(options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.4)) %>%
  addPolygons(data = ILan_town, color = "white", weight = 1, fillColor = ~pal(TOWNNAME), fillOpacity = 0.1) %>%
  addPolygons(data = v40, fillColor = "coral", weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, group = "40% population") %>%
  addPolygons(data = v60, fillColor = "coral", weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, group = "60% population") %>%
  addCircleMarkers(data = happyC, ~Response_X, ~Response_Y,
                   label = ~STName,
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Happy Learn Centers") %>%
  addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, color = "green",
                   label = ~ClinName,
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Clinics") %>%
  addCircleMarkers(data = AddreIN, ~Response_X, ~Response_Y,
                   label = ~ClinName, color = "purple",
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "GP Clinic") %>%
  addCircleMarkers(data = NoClinVill10h, ~labpt_X, ~labpt_Y, color = "coral",
                   radius = ~sqrt(sum65abov),
                   label = ~paste0(TownName, VillagName),
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Vills") %>%
  addCircles(data = AddreHome, ~Response_X, ~Response_Y, color = "gold",
             radius = 5000,
             label = ~ClinName,
             labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
             group = "GP radius") %>%
  addLayersControl(
    baseGroups = c("40% population", "60% population", "Vills"),
                   overlayGroups = c("Happy Learn Centers","Clinics", "GP Clinic", "GP radius"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))


villGeo <- read.csv("ILanVillGeo.csv", fileEncoding = "UTF-8")
NoClinVill2i <- villGeo[villGeo$inTwoKM_INT == F, ]
NoClinVill3i <- villGeo[villGeo$inThrKM_INT == F, ]
NoClinVill5i <- villGeo[villGeo$inFivKM_INT == F, ]
NoClinVill10i <- villGeo[villGeo$inTenKM_INT == F, ]
NoClinVill2h <- villGeo[villGeo$inTwoKM_HOM == F, ]
NoClinVill3h <- villGeo[villGeo$inThrKM_HOM == F, ]
NoClinVill5h <- villGeo[villGeo$inFivKM_HOM == F, ]
NoClinVill10h <- villGeo[villGeo$inTenKM_HOM == F, ]
NoClinVill2g <- villGeo[villGeo$inTwoKM_GRP == F, ]
NoClinVill3g <- villGeo[villGeo$inThrKM_GRP == F, ]
NoClinVill5g <- villGeo[villGeo$inFivKM_GRP == F, ]
NoClinVill10g <- villGeo[villGeo$inTenKM_GRP == F, ]

#Map 2 home care team and coveragee
leaflet(options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.4)) %>%
  addPolygons(data = ILan_town, color = "white", weight = 1, fillColor = ~pal(TOWNNAME), fillOpacity = 0.1) %>%
  addMarkers(data = AddreHome, ~Response_X, ~Response_Y,
             label = ~ClinName,
             labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
             group = "Home Care Teams") %>%
  addCircleMarkers(data = NoClinVill2h, ~labpt_X, ~labpt_Y, color = "coral",
                   radius = ~sqrt(sum65abov),
                   label = ~paste0(TownName),
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Unreached Villages") %>%
  addCircles(data = AddreHome, ~Response_X, ~Response_Y, color = "gold",
             radius = 2000,
             label = ~ClinName,
             labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
             group = "Service Coverage") %>%
  addLayersControl(
    overlayGroups = c("Unreached Villages","Home Care Teams", "Service Coverage"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))



# get gCentroid of villages
villList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
villList$VillagName <- dsl(villList$VillagName)
ILan_villag$TOWNNAME <- villList$TownName
ILan_villag$VILLNAME <- villList$VillagName

library(rgeos) #gCentroid
library(geosphere) #distHaversine
villGeo <- cbind(villList, ILanAgeData[,c("summ","sum65abov")])
villGeo$Center_X <- as.numeric("")
villGeo$Center_Y <- as.numeric("")
villGeo$inTwoKM_HOM <- as.logical(F)
villGeo$inTwoKM_INT <- as.logical(F)
villGeo$inTwoKM_GRP <- as.logical(F)
villGeo$inThrKM_HOM <- as.logical(F)
villGeo$inThrKM_INT <- as.logical(F)
villGeo$inThrKM_GRP <- as.logical(F)
villGeo$inFivKM_HOM <- as.logical(F)
villGeo$inFivKM_INT <- as.logical(F)
villGeo$inFivKM_GRP <- as.logical(F)
villGeo$inTenKM_HOM <- as.logical(F)
villGeo$inTenKM_INT <- as.logical(F)
villGeo$inTenKM_GRP <- as.logical(F)

ClinIsNear <- function (df, x, y, d){  # is there clinic within dist d
  d <- as.numeric(d)
  r <- (d+1)/111.3 #safety bound square
  df <- subset(df, df$Response_X > (x-r))
  df <- subset(df, df$Response_X < (x+r))
  df <- subset(df, df$Response_Y > (y-r))
  df <- subset(df, df$Response_Y < (y+r))

  isNear <- FALSE
  if (nrow(df) > 0){
    for (j in 1:nrow(df)){
      x1 <- df$Response_X[j]
      y1 <- df$Response_Y[j]
      dist <- distHaversine(c(x,y),c(x1,y1))/1000
      print(c(j,x,y,x1,y1,dist,d))
      if (dist < d){
        isNear <- TRUE
        break
      }
    }
  }
  isNear
}

for (i in 1:233){
  town <- villList$TownName[i]
  vill <- villList$VillagName[i]
  villSHP <- subset(ILan_villag, ILan_villag$TOWNNAME %in% town)
  villSHP <- subset(villSHP, villSHP$VILLNAME %in% vill)
  villcent <- gCentroid(villSHP)
  x <- villcent@coords[1]
  y <- villcent@coords[2]
  villGeo$Center_X[i] <- x
  villGeo$Center_Y[i] <- y
  x <- villSHP@polygons[[1]]@labpt[1]
  y <- villSHP@polygons[[1]]@labpt[2]
  if (i == 92L){ #Guishan Vill
    x <- 121.898888
    y <- 24.944099
  }
  villGeo$labpt_X[i] <- x
  villGeo$labpt_Y[i] <- y
  isNear2i <- ClinIsNear(AddreIN, x, y, 2)
  isNear3i <- ClinIsNear(AddreIN, x, y, 3)
  isNear5i <- ClinIsNear(AddreIN, x, y, 5)
  isNear10i <- ClinIsNear(AddreIN, x, y, 10)
  isNear2g <- ClinIsNear(AddreGroup, x, y, 2)
  isNear3g <- ClinIsNear(AddreGroup, x, y, 3)
  isNear5g <- ClinIsNear(AddreGroup, x, y, 5)
  isNear10g <- ClinIsNear(AddreGroup, x, y, 10)
  isNear2h <- ClinIsNear(AddreHome, x, y, 2)
  isNear3h <- ClinIsNear(AddreHome, x, y, 3)
  isNear5h <- ClinIsNear(AddreHome, x, y, 5)
  isNear10h <- ClinIsNear(AddreHome, x, y, 10)
  villGeo$inTwoKM_INT[i] <- isNear2i
  villGeo$inThrKM_INT[i] <- isNear3i
  villGeo$inFivKM_INT[i] <- isNear5i
  villGeo$inTenKM_INT[i] <- isNear10i
  villGeo$inTwoKM_GRP[i] <- isNear2g
  villGeo$inThrKM_GRP[i] <- isNear3g
  villGeo$inFivKM_GRP[i] <- isNear5g
  villGeo$inTenKM_GRP[i] <- isNear10g
  villGeo$inTwoKM_HOM[i] <- isNear2h
  villGeo$inThrKM_HOM[i] <- isNear3h
  villGeo$inFivKM_HOM[i] <- isNear5h
  villGeo$inTenKM_HOM[i] <- isNear10h
}


coveredsum <- function(x){
  sum65 <- 68579 - sum(x$sum65abov)
  perce <- sum65/68579
  c(sum65,perce)
}


# create a dataset, count coverage by radius and clin type
coverag_dist=c(rep("2 km radius" , 3) , rep("3 km radius" , 3) , rep("5 km radius" , 3) , rep("10 km radius" , 3) )
clinic_type=rep(c("Home Care Team" , "Community Group" , "General Practitioner") , 4)
popu65=vector(mode = "numeric", length = 12)
ratio=vector(mode = "numeric", length = 12)
data=data.frame(coverag_dist,clinic_type,popu65,ratio)

values <- rbind(coveredsum(NoClinVill2h),
coveredsum(NoClinVill2g),
coveredsum(NoClinVill2i),
coveredsum(NoClinVill3h),
coveredsum(NoClinVill3g),
coveredsum(NoClinVill3i),
coveredsum(NoClinVill5h),
coveredsum(NoClinVill5g),
coveredsum(NoClinVill5i),
coveredsum(NoClinVill10h),
coveredsum(NoClinVill10g),
coveredsum(NoClinVill10i))

data$popu65 <- values[,1]
data$ratio <- values[,2]

data$coverag_dist <- factor(data$coverag_dist, levels = c("2 km radius", "3 km radius", "5 km radius", "10 km radius"))
data$clinic_type <- factor(data$clinic_type, levels = c("Home Care Team" , "Community Group" , "General Practitioner"))

#write.csv(data, "cvrgData.csv", row.names = FALSE, fileEncoding = "UTF-8")

xlab <- "Coverage distance (km)"
ylab <- "Covered population (%)"
ggplot(data, aes(fill=clinic_type, y=ratio*100, x=coverag_dist, label = paste0(format(round(ratio*100,2))," %"))) +
  geom_bar(position="dodge", stat="identity", width = 0.85) +
  geom_text(size = 3, position = position_dodge(width = 0.85), vjust = -1.1) +
  labs(x = xlab, y = ylab, title = "Percent Population Coverage by Clinic Type and Service Distance") +
  guides(fill=guide_legend(title="Clinic Type")) +
  coord_cartesian(ylim = c(40, 100)) +
  theme(title=element_text(size=13),
        axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))
  geom_path(x = c(1,1,2,2), y = c(61,63,63,61)) + annotate("text",x=1.5,y=64,label="p=0.012")


#write.csv(villGeo, "ILanVillGeo.csv", row.names = FALSE, fileEncoding = "UTF-8")













# Clinic and TOTAL Population
rm(list=ls())

library(dplyr)
library(ggplot2)
rm(list=ls())
ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
d <- ILanAgeData
d$ID <- seq.int(nrow(d))
d <- arrange(d, desc(PopuDens)) %>%
  mutate(
    cumsum = cumsum(summ),
    freq = round(summ / sum(summ), 3),
    cum_freq = cumsum(freq)
  )
d$DensIndex <- c("")
for (i in 1:nrow(d)){
  idx <- ceiling(d$cum_freq[i]*5)
  idx <- ifels(idx > 5, 5, idx)
  d$DensIndex[i] <- idx

}

d <- arrange(d, ID)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

library(rgdal)
library(leaflet)
dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.factor(townList$TownName)
ILan_villag$DensIndex <- d$DensIndex
v40 <- subset(ILan_villag, ILan_villag$DensIndex %in% c(1,2))
v60 <- subset(ILan_villag, ILan_villag$DensIndex %in% c(1,2,3))

happyC <- read.csv("HappyLearnCentFull.csv", fileEncoding = "UTF-8")
happyC$STName <- dsl(happyC$STName)
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$ClinName <- dsl(Addre$ClinName)
AddreW <- Addre[Addre$ClinType %in% c(1,2),]

pal <- colorFactor(palette = gg_color_hue(12), domain = ILan_town$TOWNNAME)
leaflet(options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.4)) %>%
  addPolygons(data = ILan_town, color = "white", weight = 1, fillColor = ~pal(TOWNNAME), fillOpacity = 0.1) %>%
  addPolygons(data = v40, fillColor = "coral", weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, group = "40% population") %>%
  addPolygons(data = v60, fillColor = "coral", weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, group = "60% population") %>%
  addCircleMarkers(data = happyC, ~Response_X, ~Response_Y,
                   label = ~STName,
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Happy Learn Centers") %>%
  addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, color = "green",
                   label = ~ClinName,
                   labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                   group = "Clinics") %>%
  addLayersControl(baseGroups = c("40% population", "60% population"),
                   overlayGroups = c("Happy Learn Centers","Clinics"),
                   options = layersControlOptions(collapsed = FALSE))
#ClinType to numbers
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")

Addre$ClinType <- ifelse(grepl("牙",as.character(Addre$ClinType)),4,as.character(Addre$ClinType))
Addre$ClinType <- ifelse(grepl("中",as.character(Addre$ClinType)),3,as.character(Addre$ClinType))
Addre$ClinType <- ifelse(grepl("衛生所",as.character(Addre$ClinName)),2,as.character(Addre$ClinType))

for (i in c(1:nrow(Addre))){
  type <- as.character(Addre$ClinType[i])
  if (nchar(type) != 1) {
    Addre$ClinType[i] <- 1
  }
}

write.csv(Addre, "Test.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Add website
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$WebSite <- paste0("<a href='","http://www.nhi.gov.tw/query/query3_detail.aspx?HospID=",Addre$ClinCode,"' target='_blank'>診所詳細資料</a>")

write.csv(Addre, "ILanClinicFull.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Clinic numbers per villages, per sqkm, and per persons
rm(list=ls())

dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

library(rgdal)
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")

ILanAges <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILanVills <-read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
AddreW <- Addre[Addre$ClinType %in% c(1,2),]
#AddreD <- Addre[Addre$ClinType %in% 4,]

ILanClinWGeo <- ILanVills[,c(4,7)]
ILanClinWGeo$VillagName <- dsl(ILanClinWGeo$VillagName)
ILanClinWGeo$vill_area <- ILanAges$vill_area
ILanClinWGeo$popN <- ILanAges$summ
ILanClinWGeo$popDens <- ILanAges$PopuDens

ILan_villag$TOWNNAME <- ILanClinWGeo$TownName
ILan_villag$VILLNAME <- ILanClinWGeo$VillagName

library(geosphere) #dist2Line #gContains
#dist2Line(p = c(121.759596,24.677995),line = LT)

for (i in 1:nrow(ILanClinWGeo)){
 town <- as.character(ILanClinWGeo$TownName[i])
 vill <- as.character(ILanClinWGeo$VillagName[i])
 Addre1 <- AddreW[AddreW$Town == town,]
 Addre1 <- Addre1[Addre1$Vill == vill,]
 NW <- nrow(Addre1)
 VA <- ILanClinWGeo$vill_area[i]
 PN <- ILanClinWGeo$popN[i]

 ILanClinWGeo$ClinNW[i] <- NW
 ILanClinWGeo$ClinNW_villarea[i] <- NW/VA
 ILanClinWGeo$ClinNW_popN[i] <- NW/PN

 # count in clinics within 200 meters to vills
 villSHP <- subset(ILan_villag, ILan_villag$TOWNNAME %in% town)
 villSHP <- subset(villSHP, villSHP$VILLNAME %in% vill)
 NW100 <- NW
 NW200 <- NW
 for (j in 1:nrow(AddreW)){
   p <- c(AddreW$Response_X[j],AddreW$Response_Y[j])
   if (!(AddreW$Town[j] == town & AddreW$Vill[j] == vill)){
     d <- dist2Line(p = p,line = villSHP)
     d <- as.numeric(d[1,1])
     if (d < 200){ NW200 <- NW200 + 1
       if (d < 100){ NW100 <- NW100 + 1 }
     }
   }
 }
 ILanClinWGeo$ClinNW200[i] <- NW200
 ILanClinWGeo$ClinNW200_villarea[i] <- NW200/VA
 ILanClinWGeo$ClinNW200_popN[i] <- NW200/PN
 ILanClinWGeo$ClinNW100[i] <- NW100
 ILanClinWGeo$ClinNW100_villarea[i] <- NW100/VA
 ILanClinWGeo$ClinNW100_popN[i] <- NW100/PN
}

write.csv(ILanClinWGeo, "ClinWGeo.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Clin number by Towns
library(rgdal)
ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
ClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")
ILanAgeTown <- read.csv("ILanAgeTown.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- ILanAgeTown$TownName[1:12]
d <- ClinWGeo
e <- ILanAgeTown
x <- c("Town","ClinN","Pop65","Area","ClinDens","Pop65Dens","N_pop")
df <- as.data.frame(matrix(0,0,length(x)))
names(df) <- x
for (i in 1:12){
  town <- as.character(e$TownName[i])
  t <- d[d$TownName %in% town,]
  townSHP <- subset(ILan_town, ILan_town$TOWNNAME %in% town)
  n <- sum(t$ClinNW)
  p <- e$sum65abov[i]
  a <- as.numeric(townSHP@polygons[[1]]@area)*10000
  c <- c(town, n, p, a, (n/a), (p/a), (n/p))
  df[i,] <- c
}

df$Pop65Dens <- as.numeric(df$Pop65Dens)
#bar and pareto
h <- sum(as.numeric(df$ClinN))
df$ClinN <- as.numeric(df$ClinN)
df$ClinDens <- as.numeric(df$ClinDens)
df <- arrange(df, desc(ClinN)) %>% #cum sum and freq for pareto chart
  mutate(
    cumsum = cumsum(ClinN),
    freq = round(ClinN / sum(ClinN), 3),
    cum_freq = cumsum(freq)
  )
df$sortByClinN <- seq.int(nrow(df))
df$sortByClinN <- 13-df$sortByClinN
df$TownbySum <- with(df, reorder(Town, sortByClinN)) #sort by ClinDens

p <- ggplot(data = df, aes(x = df$TownbySum, y = df$ClinN, fill = Town))
p <- p + theme(legend.position="none")
p <- p + geom_bar(stat="identity",width = 0.7)
p <- p + geom_text(data = df, aes(x = df$TownbySum, y = df$ClinN, label= df$ClinN, vjust=-0.5, hjust=1.4))
p <- p + geom_line(data = df, aes(x = df$TownbySum, y = df$cumsum, group ="1"))
p <- p + geom_point(data = df, aes(x = df$TownbySum, y = df$cumsum, group ="1"))
p <- p + geom_text(data = df, aes(x = df$TownbySum, y = df$cumsum, label= paste0(round(df$cum_freq*100), "%"), group ="1"), vjust = -0.7, hjust = -0.1)
p <- p + labs(x = "", y = "診所數（家）")
p <- p + geom_text(aes(0, h, label = paste0("宜蘭縣：", h, "家"), group = "2", hjust=-0.2, vjust=2.5, size=12))
p <- p + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
p

#Correlation
library(ggplot2)
library(ggrepel)
library(grid)
DF <- data.frame(VAR1=as.numeric(df$Pop65Dens), VAR2=as.numeric(df$ClinDens), grp=df$Town)
#reg <- lm(log(VAR2)~log(VAR1),data=DF)
reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],3), "x ", ifelse(coeff[1]<0, "− ", "+ "), abs(round(coeff[1],3)))
r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
xlab <- "Aged Population Density (persons aged 65 and above/sqkm)"
ylab <- "Clinic Density (1/sqkm)"
fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}
p <- ggplot(DF,aes(VAR1, VAR2, label = grp))
#p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
#p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$grp))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to 65 Population density: Yilan County")
y <- layer_scales(p)$y$range$range[2]
x <- layer_scales(p)$x$range$range[1]
p <- p + geom_text(aes(x = x, y = y, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
p <- p + geom_text(aes(x = x, y = y, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
p # ILan -- ClinDens to PopDens (Logged)


# Dentistry
rm(list=ls())

dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

ILanAges <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILanVills <-read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
AddreW <- Addre[Addre$ClinType %in% c(1,2),]
AddreD <- Addre[Addre$ClinType %in% 4,]

ILanClinDGeo <- ILanVills[,c(4,7)]
ILanClinDGeo$VillagName <- dsl(ILanClinDGeo$VillagName)
ILanClinDGeo$vill_area <- ILanAges$vill_area
ILanClinDGeo$popN <- ILanAges$summ
ILanClinDGeo$popDens <- ILanAges$PopuDens

library(rgdal)
library(geosphere) #dist2Line
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
ILan_villag$TOWNNAME <- ILanClinDGeo$TownName
ILan_villag$VILLNAME <- ILanClinDGeo$VillagName

for (i in 1:nrow(ILanClinDGeo)){
  town <- as.character(ILanClinDGeo$TownName[i])
  vill <- as.character(ILanClinDGeo$VillagName[i])
  Addre1 <- AddreD[AddreD$Town == town,]
  Addre1 <- Addre1[Addre1$Vill == vill,]
  ND <- nrow(Addre1)
  VA <- ILanClinDGeo$vill_area[i]
  PN <- ILanClinDGeo$popN[i]

  ILanClinDGeo$ClinND[i] <- ND
  ILanClinDGeo$ClinND_villarea[i] <- ND/VA
  ILanClinDGeo$ClinND_popN[i] <- ND/PN

  # count in clinics within 200 meters to vills
  villSHP <- subset(ILan_villag, ILan_villag$TOWNNAME %in% town)
  villSHP <- subset(villSHP, villSHP$VILLNAME %in% vill)
  ND100 <- ND
  ND200 <- ND
  for (j in 1:nrow(AddreD)){
    p <- c(AddreD$Response_X[j],AddreD$Response_Y[j])
    if (!(AddreD$Town[j] == town & AddreD$Vill[j] == vill)){
      d <- dist2Line(p = p,line = villSHP)
      d <- as.numeric(d[1,1])
      if (d < 200){ ND200 <- ND200 + 1
      if (d < 100){ ND100 <- ND100 + 1 }
      }
    }
  }
  ILanClinDGeo$ClinND200[i] <- ND200
  ILanClinDGeo$ClinND200_villarea[i] <- ND200/VA
  ILanClinDGeo$ClinND200_popN[i] <- ND200/PN
  ILanClinDGeo$ClinND100[i] <- ND100
  ILanClinDGeo$ClinND100_villarea[i] <- ND100/VA
  ILanClinDGeo$ClinND100_popN[i] <- ND100/PN
}

#write.csv(ILanClinDGeo, "ClinDGeo.csv", row.names = FALSE, fileEncoding = "UTF-8")


# TCM and Dentistry
rm(list=ls())

dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}

ILanAges <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILanVills <-read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
AddreCD <- Addre[Addre$ClinType %in% c(3,4),]

ILanClinCDGeo <- ILanVills[,c(4,7)]
ILanClinCDGeo$VillagName <- dsl(ILanClinCDGeo$VillagName)
ILanClinCDGeo$vill_area <- ILanAges$vill_area
ILanClinCDGeo$popN <- ILanAges$summ
ILanClinCDGeo$popDens <- ILanAges$PopuDens

library(rgdal)
library(geosphere) #dist2Line
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
ILan_villag$TOWNNAME <- ILanClinCDGeo$TownName
ILan_villag$VILLNAME <- ILanClinCDGeo$VillagName

for (i in 1:nrow(ILanClinCDGeo)){
  town <- as.character(ILanClinCDGeo$TownName[i])
  vill <- as.character(ILanClinCDGeo$VillagName[i])
  Addre1 <- AddreCD[AddreCD$Town == town,]
  Addre1 <- Addre1[Addre1$Vill == vill,]
  NCD <- nrow(Addre1)
  VA <- ILanClinCDGeo$vill_area[i]
  PN <- ILanClinCDGeo$popN[i]

  ILanClinCDGeo$ClinNCD[i] <- NCD
  ILanClinCDGeo$ClinNCD_villarea[i] <- NCD/VA
  ILanClinCDGeo$ClinNCD_popN[i] <- NCD/PN

  # count in clinics within 200 meters to vills
  villSHP <- subset(ILan_villag, ILan_villag$TOWNNAME %in% town)
  villSHP <- subset(villSHP, villSHP$VILLNAME %in% vill)
  NCD100 <- NCD
  NCD200 <- NCD
  for (j in 1:nrow(AddreCD)){
    p <- c(AddreCD$Response_X[j],AddreCD$Response_Y[j])
    if (!(AddreCD$Town[j] == town & AddreCD$Vill[j] == vill)){
      d <- dist2Line(p = p,line = villSHP)
      d <- as.numeric(d[1,1])
      if (d < 200){ NCD200 <- NCD200 + 1
      if (d < 100){ NCD100 <- NCD100 + 1 }
      }
    }
  }
  ILanClinCDGeo$ClinNCD200[i] <- NCD200
  ILanClinCDGeo$ClinNCD200_villarea[i] <- NCD200/VA
  ILanClinCDGeo$ClinNCD200_popN[i] <- NCD200/PN
  ILanClinCDGeo$ClinNCD100[i] <- NCD100
  ILanClinCDGeo$ClinNCD100_villarea[i] <- NCD100/VA
  ILanClinCDGeo$ClinNCD100_popN[i] <- NCD100/PN
}

write.csv(ILanClinCDGeo, "ClinCDGeo.csv", row.names = FALSE, fileEncoding = "UTF-8")



#Correlations

ClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")
LTClinWGeo <- ClinWGeo[ClinWGeo$TownName %in% "羅東鎮",]
ILClinWGeo <- ClinWGeo[ClinWGeo$TownName %in% "宜蘭市",]

library(ggplot2)
library(ggrepel)
library(grid)
DF <- LTClinWGeo
#DF <- DF[DF$popDens > 5000,]
DF <- data.frame(VAR1=DF$popDens, VAR2=DF$ClinNW_villarea, arealab=paste0(DF$VillagName, ": ", DF$ClinNW200))
DF <- DF[DF$VAR2 > 0,]
reg <- lm(log(VAR2)~log(VAR1),data=DF)
reg1 <- reg
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],3), "x ", ifelse(coeff[1]<0, "− ", "+ "), abs(round(coeff[1],3)))
r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
xlab <- "Population Density (persons/sqkm)"
ylab <- "Clinic Density (1/sqkm)"
fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}
p <- ggplot(DF,aes(VAR1, VAR2, label = DF$arealab))
p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$arealab))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Population density: Luodong Town")
yy <- exp(layer_scales(p)$y$range$range[2])
xx <- exp(layer_scales(p)$x$range$range[1])
p <- p + geom_text(aes(x = xx, y = yy, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
p <- p + geom_text(aes(x = xx, y = yy, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
p # Luodong -- ClinDens to PopDens by NW



ClinDGeo <- read.csv("ClinDGeo.csv", fileEncoding = "UTF-8")
LTClinDGeo <- ClinDGeo[ClinDGeo$TownName %in% "羅東鎮",]
ILClinDGeo <- ClinDGeo[ClinDGeo$TownName %in% "宜蘭市",]

library(ggplot2)
library(ggrepel)
library(grid)
DF <- ILClinWGeo
#DF <- DF[DF$popDens > 5000,]
DF <- data.frame(VAR1=DF$popDens, VAR2=DF$ClinNW100_villarea, arealab=paste0(DF$VillagName, ": ", DF$ClinNW200))
DF <- DF[DF$VAR2 > 0,]
reg <- lm(log(VAR2)~log(VAR1),data=DF)
reg1 <- reg
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],3), "x ", ifelse(coeff[1]<0, "− ", "+ "), abs(round(coeff[1],3)))
r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
xlab <- "Population Density (persons/sqkm)"
ylab <- "Clinic Density (1/sqkm)"
fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}
p <- ggplot(DF,aes(VAR1, VAR2, label = DF$arealab))
p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$arealab))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Population density: Yilan City")
yy <- exp(layer_scales(p)$y$range$range[2])
xx <- exp(layer_scales(p)$x$range$range[1])
p <- p + geom_text(aes(x = xx, y = yy, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
p <- p + geom_text(aes(x = xx, y = yy, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
p # Yilan -- ClinDens to PopDens by NW



ClinCDGeo <- read.csv("ClinCDGeo.csv", fileEncoding = "UTF-8")
LTClinCDGeo <- ClinCDGeo[ClinCDGeo$TownName %in% "羅東鎮",]
ILClinCDGeo <- ClinCDGeo[ClinCDGeo$TownName %in% "宜蘭市",]

library(ggplot2)
library(ggrepel)
library(grid)
DF <- LTClinCDGeo
#DF <- DF[DF$popDens > 5000,]
#DF <- DF[DF$ClinNW100 > 0,]
DF <- data.frame(VAR1=DF$popDens , VAR2=DF$ClinNCD200_villarea, arealab=paste0(DF$VillagName, ": ", DF$ClinNCD200))
reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],4), "*x ", ifelse(coeff[1]<0, "- ", "+ "), abs(round(coeff[1],3)))
r_sq = paste0("R^2 = ", round(summary(reg)$adj.r.squared, 2))
xlab <- "Population Density (ppl./sqkm)"
ylab <- "Clinic Density (1/sqkm)"
p <- ggplot(DF,aes(VAR1, VAR2, label = DF$arealab))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$arealab))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Population density: Yilan County")
annot <- grobTree(textGrob(eq, x=0.05, y=0.9, hjust=0, gp=gpar(col="black", fontsize=13, fontface="bold")),
                  textGrob(r_sq, x=0.05, y=0.85, hjust=0 , gp=gpar(col="black", fontsize=13, fontface="bold")))
p <- p+ annotation_custom(annot)
p # Luodong -- ClinDens to PopDens by NW0


# Clinic number map ClinNW
library(rgdal)
library(leaflet)

ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])
ILan_villag$VILLNAME <- dsl(ILan_villag$VILLNAME)
ILan_villag$ClinNW <- as.numeric(ILanClinGeo$ClinNW)
ILan_villag$ClinNW_villarea <- as.numeric(ILanClinGeo$ClinNW_villarea)
ILan_villag$labelsCN <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",ILan_villag$ClinNW)
ILan_villag$labelsNWVA <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",ILan_villag$ClinNW_villarea)

palCN <- colorBin("YlOrRd", domain = ILan_villag$ClinNW, bins = c(0,1,2,3,5,10,16))
palNWVA <- colorBin("YlOrRd", domain = ILan_villag$ClinNW_villarea)

leaflet(data=ILan_villag, options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  fitBounds(121.7475,24.6641,121.8018,24.7069) %>%
  addPolygons(fillColor = ~palCN(ClinNW), weight=0.1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8, label=~labelsCN, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = palCN, values = ~ClinNW, opacity = 0.7, title = "ClinNW", position = "topright", layerId = "l") %>%
  addEasyButton(easyButton(
    icon="fa-home", title="Lotung",
    onClick=JS("function(btn, map){ map.fitBounds([[24.6641,121.7475],[24.7069,121.8018]]); }"))) %>%
  addPolygons(data = ILan_town, fill = FALSE, weight = 2) %>%
  addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, radius=1) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))

leaflet(data=ILan_villag, options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  fitBounds(121.7475,24.6641,121.8018,24.7069) %>%
  addPolygons(fillColor = ~palNWVA(ClinNW_villarea), weight=0.1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8, label=~labelsNWVA, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = palNWVA, values = ~ClinNW_villarea, opacity = 0.7, title = "ClinNWVA", position = "topright", layerId = "l") %>%
  addEasyButton(easyButton(
    icon="fa-home", title="Lotung",
    onClick=JS("function(btn, map){ map.fitBounds([[24.6641,121.7475],[24.7069,121.8018]]); }"))) %>%
  addPolygons(data = ILan_town, fill = FALSE, weight = 2) %>%
  addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, radius=1) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))

hist(ILanClinGeo$ClinNW, breaks = 15)
hist(ILanClinGeo$ClinNW_villarea, breaks = 100)
hist(ILanClinGeo$ClinNW_popN, breaks = 100)

library(lattice)
xyplot(ClinNW_villarea ~ popDens, data = ILanClinGeo)


# Clinic number map ClinND
library(rgdal)
library(leaflet)

ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])
ILan_villag$VILLNAME <- dsl(ILan_villag$VILLNAME)
ILan_villag$ClinND <- as.numeric(ILanClinGeo$ClinND)
ILan_villag$labelsCND <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",ILan_villag$ClinND)
palCND <- colorBin("YlOrRd", domain = ILan_villag$ClinND)
                   #bins = c(0,1,2,3,5,10,16))

leaflet(data=ILan_villag, options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  fitBounds(121.7475,24.6641,121.8018,24.7069) %>%
  addPolygons(fillColor = ~palCND(ClinND), weight=0.1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8, label=~labelsCND, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = palCND, values = ~ClinND, opacity = 0.7, title = "ClinND", position = "topright", layerId = "l") %>%
  addEasyButton(easyButton(
    icon="fa-home", title="Lotung",
    onClick=JS("function(btn, map){ map.fitBounds([[24.6641,121.7475],[24.7069,121.8018]]); }"))) %>%
  addPolygons(data = ILan_town, fill = FALSE, weight = 2) %>%
  addCircleMarkers(data = AddreD, ~Response_X, ~Response_Y, radius=1)

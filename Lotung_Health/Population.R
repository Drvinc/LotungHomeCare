rm(list=ls())

#10603 population dataset
Popu <- read.csv("opendata10603M030.csv", fileEncoding = "UTF-8")

#老人扶養比(old-age dependency ratio)
#The elderly dependency rate is defined as the ratio between the elderly population and the working age (15-64 years) population. (OECD)

#village age calcs.
ncoll <- ncol(Popu)
nroww <- nrow(Popu)
summ <- rowSums(Popu[,8:ncoll])
age15m <- which(names(Popu)=="people_age_015_m")
age15m <- which(names(Popu)=="people_age_015_m")
age65m <- which(names(Popu)=="people_age_065_m")
age65f <- which(names(Popu)=="people_age_065_f")
sum14belo <- rowSums(Popu[,8:(age15m-1)])
sum15to64 <- rowSums(Popu[,age15m:(age65m-1)])
sum65abov <- rowSums(Popu[,age65m:ncoll])
EDR <- sum65abov/sum15to64 #老人扶養比
AgedR <- sum65abov/summ #老人比例
AgIndex <- sum65abov/sum14belo #老化指數

ILanAgeData <- cbind(Popu[,2:3], summ, sum14belo, sum15to64, sum65abov, EDR, AgedR, AgIndex)

library(rgdal)
library(raster)
library(geosphere)

ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
villagList$VillagName <- gsub("/","",as.character(villagList[,7]))
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])

#ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
nroww <- nrow(ILanAgeData)
for (i in 1:nroww){
  a <- substr(ILanAgeData$site_id[i], 4, 6)
  TownSHP <- subset(ILan_villag, ILan_villag$TOWNNAME == a)
  b <- ILanAgeData$village[i]
  VillSHP <- subset(TownSHP, TownSHP$VILLNAME == b)
  ILanAgeData$vill_area[i] <- as.numeric(VillSHP@polygons[[1]]@area)*10000
  ILanAgeData$vill_area1[i] <- areaPolygon(VillSHP)/1000000
}

ILanAgeData$PopuDens <- (ILanAgeData$summ/ILanAgeData$vill_area)
ILanAgeData$PopuDens65 <- (ILanAgeData$sum65abov/ILanAgeData$vill_area)
ILanAgeData <- ILanAgeData[,-11]

#按照SHP檔排序

for (i in 1:nroww){
  a <- villagList[i,7]
  b <- as.character(villagList[i,4])
  town <- ILanAgeData[substr(ILanAgeData$site_id,4,6) %in% b,]
  j <- which(town$village == a)
  villagList$site_id[i] <- as.character(town$site_id[j])
  villagList$village[i] <- as.character(town$village[j])
  villagList$summ[i] <- town$summ[j]
  villagList$sum14belo[i] <- town$sum14belo[j]
  villagList$sum15to64[i] <- town$sum15to64[j]
  villagList$sum65abov[i] <- town$sum65abov[j]
  villagList$EDR[i] <- town$EDR[j]
  villagList$AgedR[i] <- town$AgedR[j]
  villagList$AgIndex[i] <- town$AgIndex[j]
  villagList$vill_area[i] <- town$vill_area[j]
  villagList$PopuDens[i] <- town$PopuDens[j]
  villagList$PopuDens65[i] <- town$PopuDens65[j]
}

ILanAgeData <- villagList[,-c(1:7)]

write.csv(ILanAgeData, "ILanAgeData.csv", row.names = FALSE, fileEncoding = "UTF-8")

#town age calcs
ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILanAgeData$TOWN <- substr(ILanAgeData$site_id, 4, 6)

townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILanAgeTown <- townList[,c(1:3)]

for (i in 1:nrow(townList)){
  town <- townList$TownName[i]
  townPopu <- ILanAgeData[ILanAgeData$TOWN %in% town,]
  summ <- sum(townPopu[,3])
  sum14belo <- sum(townPopu[,4])
  sum15to64 <- sum(townPopu[,5])
  sum65abov <- sum(townPopu[,6])
  ILanAgeTown$summ[i] <- summ
  ILanAgeTown$sum14belo[i] <- sum14belo
  ILanAgeTown$sum15to64[i] <- sum15to64
  ILanAgeTown$sum65abov[i] <- sum65abov
  ILanAgeTown$EDR[i] <- sum65abov/sum15to64 #老人扶養比
  ILanAgeTown$AgedR[i] <- sum65abov/summ #老人比例
  ILanAgeTown$AgIndex[i] <- sum65abov/sum14belo #老化指數
}

summ <- sum(ILanAgeTown[,4])
sum14belo <- sum(ILanAgeTown[,5])
sum15to64 <- sum(ILanAgeTown[,6])
sum65abov <- sum(ILanAgeTown[,7])
EDR <- sum65abov/sum15to64
AgedR <- sum65abov/summ
AgIndex <- sum65abov/sum14belo
ILanAgeTown$TownName <- as.character(ILanAgeTown$TownName)
ILanAge <- c("","","宜蘭縣",summ,sum14belo,sum15to64,sum65abov,EDR,AgedR,AgIndex)
ILanAgeTown <- rbind(ILanAgeTown, ILanAge)
ILanAgeTown[, 4:10] <- sapply(ILanAgeTown[, 4:10], as.numeric)

write.csv(ILanAgeTown, "ILanAgeTown.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Town Age plots
ILanAgeTown <- read.csv("ILanAgeTown.csv", fileEncoding = "UTF-8")

library(ggplot2)
library(dplyr)

#Fig1 ILan TotPop
h <- ILanAgeTown$summ[13]/1000
df <- ILanAgeTown[c(1:12),]
df$summ <- as.numeric(df$summ)
df <- arrange(df, desc(summ)) %>% #cum sum and freq for pareto chart
  mutate(
    cumsum = cumsum(summ),
    freq = round(summ / sum(summ), 3),
    cum_freq = cumsum(freq)
  )
df$TownbySum <- with(df, reorder(TownName, summ)) #sort by summ
c <- ggplot(data = df, aes(x = df$TownbySum, y = df$summ/1000, fill = TownName))
c <- c + theme(legend.position="none")
c <- c + geom_bar(stat="identity",width = 0.7)
c <- c + geom_text(data = df, aes(x = df$TownbySum, y = df$summ/1000, label= format(round(df$summ/1000, 1), nsmall = 1)), vjust=1.5)
c <- c + geom_line(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, group ="1"))
c <- c + geom_point(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, group ="1"))
c <- c + geom_text(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, label= paste0(round(df$cum_freq*100), "%"), group ="1"), vjust = -0.7, hjust = -0.1)
c <- c + labs(x = "", y = "總人口數（千人）")
c <- c + geom_text(aes(5, h, label = paste0("← 宜蘭縣：", format(round(h,1),nsmall = 1), " 千人"), group = "2", vjust = 0.5, hjust=0, size=12))
c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
c

#Fig 2 ILan 65Pop
h <- ILanAgeTown$sum65abov[13]/1000
df <- ILanAgeTown[c(1:12),]
df$sum65abov <- as.numeric(df$sum65abov)
df <- arrange(df, desc(sum65abov)) %>% #cum sum and freq for pareto chart
  mutate(
    cumsum = cumsum(sum65abov),
    freq = round(sum65abov / sum(sum65abov), 3),
    cum_freq = cumsum(freq)
  )
df$TownbySum65 <- with(df, reorder(TownName, sum65abov)) #sort by sum65abov
c <- ggplot(data = df, aes(x = df$TownbySum65, y = df$sum65abov/1000, fill = TownName))
c <- c + theme(legend.position="none")
c <- c + geom_bar(stat="identity",width = 0.7)
c <- c + geom_text(data = df, aes(x = df$TownbySum65, y = df$sum65abov/1000, label= format(round(df$sum65abov/1000, 1), nsmall = 1)), vjust=1.5)
c <- c + geom_line(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, group ="1"))
c <- c + geom_point(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, group ="1"))
c <- c + geom_text(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, label= paste0(round(df$cum_freq*100), "%"), group ="1"), vjust = -0.7, hjust = -0.1)
c <- c + labs(x = "", y = "65歲以上老年人口總數（千人）")
c <- c + geom_text(aes(0, h, label = paste0("宜蘭縣：", format(round(h,1),nsmall = 1), " 千人"), group = "2", hjust=-0.2, vjust=2.5, size=12))
c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
c

#Fig 3 AgedR
df <- ILanAgeTown[c(1:12),]
df$TownbyAgedR <- with(df, reorder(TownName, AgedR))
h <- ILanAgeTown$AgedR[13]*100
c <- ggplot(df, aes(x = df$TownbyAgedR , y = df$AgedR*100, fill = TownName))
c <- c + coord_cartesian(ylim = c(5, 20))
c <- c + theme(legend.position="none")
c <- c + geom_bar(stat="identity",width = 0.7)
c <- c + geom_text(data = df, aes(x = df$TownbyAgedR, y = df$AgedR*100, label= format(round(df$AgedR*100, 1))), vjust=1.5)
c <- c + labs(x = "", y = "老年人口比例（%）")
c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
c <- c + geom_hline(yintercept = h, linetype="dashed", color = "blue", size=0.8)
c <- c + geom_text(aes(0, h, label = paste0("宜蘭縣：", format(round(h,1)), " %"), hjust=-0.2, vjust=-1, size=12))
c

#Fig 4 aged dependence ratio
df <- ILanAgeTown[c(1:12),]
df$TownbyEDR <- with(df, reorder(TownName, EDR))
h <- ILanAgeTown$EDR[13]
c <- ggplot(df, aes(x = df$TownbyEDR , y = df$EDR, fill = TownName))
c <- c + coord_cartesian(ylim = c(0.1, 0.27))
c <- c + theme(legend.position="none")
c <- c + geom_bar(stat="identity",width = 0.7)
c <- c + geom_text(data = df, aes(x = df$TownbyEDR, y = df$EDR, label= format(round(df$EDR, 2))), vjust=1.5)
c <- c + geom_text(data = df, aes(x = df$TownbyEDR, y = df$EDR, label= paste0("(", format(round(1/df$EDR, 1)), ")")), vjust=3, group = "1")
c <- c + labs(x = "", y = "老人扶養比")
c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
c <- c + geom_hline(yintercept = h, linetype="dashed", color = "blue", size=0.8)
c <- c + geom_text(aes(0, h, label = paste0("宜蘭縣：", format(round(h,2),nsmall = 2), " (", format(round(1/h,1)), ")"), hjust=-0.2, vjust=-1, size=12))
c

#Fig 5 dens above 383....
library(dplyr)
library(ggplot2)
rm(list=ls())
ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
d <- ILanAgeData
d$Town <- substr(d$site_id,4,6)
d$Vill <- paste0(substr(d$site_id,4,6),d$village)
d$ID <- seq.int(nrow(d))
d <- arrange(d, desc(PopuDens65)) %>%
  mutate(
    cumsum = cumsum(sum65abov),
    freq = round(sum65abov / sum(sum65abov), 3),
    cum_freq = cumsum(freq)
  )
d <- d[1:20,]

c <- ggplot(data = d, aes(x = Vill, y = sum65abov/1000, fill = Town))
c <- c + theme(legend.position="none")
c <- c + geom_bar(stat="identity",width = 0.7, angle=45)
c <- c + geom_text(data = d, aes(x = Vill, y = sum65abov/1000, label= format(round(d$sum65abov/1000, 1), nsmall = 1)), vjust=1.5)
c <- c + geom_line(data = d, aes(x = Vill, y = cumsum/1000, group ="1"))
c <- c + geom_point(data = d, aes(x = Vill, y = cumsum/1000, group ="1"))
c <- c + geom_text(data = d, aes(x = Vill, y = cumsum/1000, label= paste0(round(d$cum_freq*100), "%"), group ="1"), vjust = -0.7, hjust = -0.1)
c <- c + labs(x = "", y = "65歲以上老年人口總數（千人）")
c


# Find n cut intervals by TotalPOP
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

freqcut <- function(x, bins){
  grp <- ceiling(x*bins)
  grp <- ifelse(grp > bins, bins, grp)
  grp
}

bins <- 10L
for (i in 1:nrow(d)){
  grp <- freqcut(d$cum_freq[i],bins)
  d$DensIndex[i] <- grp
}

d <- arrange(d, ID)

ClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")
ClinWGeo$DensIndex <- as.numeric(d$DensIndex)
ClinWGeo$summ <- d$summ

c <- data.frame(id = numeric(0), clinN = numeric(0), pop = numeric(0), area = numeric(0), clind = numeric(0), popd = numeric(0), N_pop = numeric(0))
for (i in 1:max(as.numeric(d$DensIndex))){
  target <- ClinWGeo[ClinWGeo$DensIndex == i,]
  s <- sum(target$ClinNW)
  p <- sum(target$summ)
  a <- sum(target$vill_area)
  c1 <- c(i, s, p, a, s/a, p/a, s/p)
  c <- rbind(c, c1)
}
names(c) <- c("id","ClinN", "pop", "villarea", "ClinDens", "popDens", "N_pop")

library(ggplot2)
library(ggrepel)
library(grid)
DF <- c[c$ClinN != 0,]
DF <- data.frame(VAR1=DF$popDens, VAR2=DF$ClinDens, grp=DF$id)
#DF <- data.frame(VAR1=DF$popDens, VAR2=DF$ClinDens, grp=DF$id)
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
p <- ggplot(DF,aes(VAR1, VAR2, label = DF$grp))
p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$grp))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Population density: Yilan County")
y <- exp(layer_scales(p)$y$range$range[2])
x <- exp(layer_scales(p)$x$range$range[1])
p <- p + geom_text(aes(x = x, y = y, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
p <- p + geom_text(aes(x = x, y = y, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
p # ILan -- ClinDens to PopDens


# Find n cut intervals by 65 Pop
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

bins <- 20
for (i in 1:nrow(d)){
  cumf <- d$cumsum[i]/max(d$cumsum)
  grp <- ceiling(cumf*bins)
  if (i > 1){
    cumf1 <- d$cumsum[i-1]/max(d$cumsum)
    grp1 <- ceiling(cumf1*bins)
    grp <- ifelse(grp - grp1 == 1, grp1, grp)
  }
  d$DensIndex[i] <- grp
}

d <- arrange(d, ID)

ClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")
ClinWGeo$DensIndex <- as.numeric(d$DensIndex)
ClinWGeo$sum65abov <- d$sum65abov

c <- data.frame(id = numeric(0), clinN = numeric(0), pop65 = numeric(0), area = numeric(0), clind = numeric(0), pop65d = numeric(0), N_pop = numeric(0))
for (i in 1:max(as.numeric(d$DensIndex))){
  target <- ClinWGeo[ClinWGeo$DensIndex == i,]
  s <- sum(target$ClinNW)
  p <- sum(target$sum65abov)
  a <- sum(target$vill_area)
  #c1 <- c(i, s, p, a, ((s/a) + 0.00000001), p/a, s/p)
  c1 <- c(i, s, p, a, (s/a), p/a, s/p)
  c <- rbind(c, c1)
}
names(c) <- c("id","ClinN", "pop65", "villarea", "ClinDens", "pop65Dens", "N_pop")

library(ggplot2)
library(ggrepel)
library(grid)
DF <- c
DF_reg <- DF
#DF_reg <- DF[-bins,]
DF_reg <- DF_reg[DF_reg$ClinN != 0,]
DF_reg <- data.frame(VAR1=DF_reg$pop65Dens, VAR2=DF_reg$ClinDens, grp=DF_reg$id)
DF <- DF[DF$ClinN != 0,]
DF <- data.frame(VAR1=DF$pop65Dens, VAR2=DF$ClinDens, grp=DF$id)
DF_OL <- c[c$ClinN == 0,]
DF_OL <- data.frame(VAR1=DF_OL$pop65Dens, grp=DF_OL$id)
#qplot(log(VAR1), log(VAR2), data = DF)
#qplot(VAR1, VAR2, data = DF, geom = c("point", "smooth"))
reg <- lm(log(VAR2)~log(VAR1),data=DF_reg)
#reg <- lm(VAR2~VAR1,data=DF)
coeff <- coefficients(reg)
eq = paste0("y = ", round(coeff[2],3), "x ", ifelse(coeff[1]<0, "− ", "+ "), abs(round(coeff[1],3)))
r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
xlab <- "Aged Population Density (persons aged 65 and above/sqkm)"
ylab <- "Clinic Density (1/sqkm)"
fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}
p <- ggplot(DF,aes(VAR1, VAR2, label = grp))
p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$grp))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Aged population (65 years and above) density: Yilan County")
p <- p + theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold"),
           #panel.background = element_rect(fill = "transparent",colour = NA))
           plot.background = element_rect(fill = "transparent",colour = NA))
y <- exp(layer_scales(p)$y$range$range[2])
x <- exp(layer_scales(p)$x$range$range[1])
y_OL <- exp(layer_scales(p)$y$range$range[1])
if (nrow(DF_OL) > 0){
p <- p + geom_point(data = DF_OL, aes(x= VAR1, y= y_OL, group = "1"))
p <- p + geom_text_repel(data = DF_OL, size=3, aes(x= VAR1, y= y_OL, label=grp, group = "1"))
}
#y <- layer_scales(p)$y$range$range[2]
#x <- layer_scales(p)$x$range$range[1]
p <- p + geom_text(aes(x = x, y = y, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
p <- p + geom_text(aes(x = x, y = y, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
p # ILan -- ClinDens to PopDens (Logged)


DF <- c
DF <- data.frame(VAR1=DF$pop65Dens, VAR2=DF$ClinDens, grp=DF$id)
#qplot(log(VAR1), log(VAR2), data = DF)
#qplot(VAR1, VAR2, data = DF, geom = c("point", "smooth"))
xlab <- "Aged Population Density (persons aged 65 and above/sqkm)"
ylab <- "Clinic Density (1/sqkm)"
p <- ggplot(DF,aes(VAR1, VAR2, label = DF$grp))
p <- p + geom_point() + geom_smooth(method='auto', formula = y ~ x, fill='lightblue', color='darkblue', size=1)
p <- p + geom_label_repel(size=3,aes(label=DF$grp))
p <- p + labs(x = xlab, y = ylab, title = "Clinic density to 65 Population density: Yilan County")
p # ILan -- ClinDens to PopDens


library(rgdal)
library(leaflet)
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.factor(townList$TownName)

NOClinWID <- which(c$ClinDens %in% 0.00000001)
ILan_villag$DensIndex  <- as.factor(d$DensIndex)
for (i in 1:233){
  index <- ILan_villag$DensIndex[i]
  Dens65 <- c[c$id %in% index, 6]
  ILan_villag$Dens65byIndex[i] <- Dens65
}
NOClinWArea <- subset(ILan_villag, ILan_villag$DensIndex %in% NOClinWID)


pal <- colorBin(palette = "Blues", domain = NOClinWArea$Dens65byIndex)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gpal <- colorFactor(palette = gg_color_hue(12), domain = ILan_town$TOWNNAME)

leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = ILan_town, color = "white", weight = 1, fillColor = ~gpal(TOWNNAME), fillOpacity = 0.1) %>%
  addPolygons(data = NOClinWArea, fillColor = ~pal(Dens65byIndex), weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, label=~DensIndex, labelOptions = labelOptions(textsize = "13px",clickable = TRUE)) %>%
  addLegend(pal = pal, values = NOClinWArea$Dens65byIndex, opacity = 0.7, title = "Group by PopDens", position = "topright")


#LotungAgeMap

library(rgdal)
library(leaflet)

ILan_villag <- readOGR(dsn = "ILan_villages", layer = "ILan_villages")
Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")
villagList <- read.csv("LotungVillTable.csv", fileEncoding = "UTF-8")
Lotung_villag$VILLNAME <- as.character(villagList[,4])

ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
LotungAgeData <- subset(ILanAgeData, substr(ILanAgeData$site_id,4,6)=="羅東鎮")
Lotung_villag$EDR <- LotungAgeData$EDR
Lotung_villag$AGR <- LotungAgeData$AgedR
Lotung_villag$AGI <- LotungAgeData$AgIndex
Lotung_villag$labelsEDR <- paste0(Lotung_villag$VILLNAME,": ",round(Lotung_villag$EDR,digits = 3))
Lotung_villag$labelsAGR <- paste0(Lotung_villag$VILLNAME,": ",round(Lotung_villag$AGR*100,digits = 2), " %")
Lotung_villag$labelsAGI <- paste0(Lotung_villag$VILLNAME,": ",round(Lotung_villag$AGI,digits = 3))

Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
EDRati <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "EDRati"])
AGRate <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "AGRate"])
AGIndx <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "AGIndx"])

palEDR <- colorBin("YlOrRd", domain = Lotung_villag$EDR)
palAGR <- colorBin("YlOrRd", domain = Lotung_villag$AGR*100)
palAGI <- colorBin("YlOrRd", domain = Lotung_villag$AGI)

leaflet(Lotung_villag, options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~palEDR(EDR), weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, label=~labelsEDR, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3), group=EDRati) %>%
  addLegend(pal = palEDR, values = ~EDR, opacity = 0.7, title = EDRati, position = "bottomright")

#ILanAgeMap

library(rgdal)
library(leaflet)
library(scales)

ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])

ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
ILan_villag$EDR <- ILanAgeData$EDR
#ILan_villag$AGR <- ILanAgeData$AgedR
#ILan_villag$AGI <- ILanAgeData$AgIndex
ILan_villag$labelsEDR <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$EDR,digits = 3))
#ILan_villag$labelsAGR <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$AGR*100,digits = 2), " %")
#ILan_villag$labelsAGI <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$AGI,digits = 3))

Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
EDRati <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "EDRati"])
#AGRate <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "AGRate"])
#AGIndx <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "AGIndx"])

palEDR <- colorBin("YlOrRd", domain = sqrt_trans(ILan_villag$EDR))
#palAGR <- colorBin("YlOrRd", domain = ILan_villag$AGR*100)
#palAGI <- colorBin("YlOrRd", domain = ILan_villag$AGI)

leaflet(ILan_villag) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(121.3151,24.3236,121.9684,24.9908) %>%
  addPolygons(fillColor = ~palEDR(EDR), weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1,
              label=~labelsEDR, labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
              highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = palEDR, values = ~EDR, opacity = 0.7, title = EDRati, position = "bottomright")


# Lotung Population Density Map
library(rgdal)
library(leaflet)

ILan_villag <- readOGR(dsn = "ILan_villages", layer = "ILan_villages")
Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")
villagList <- read.csv("LotungVillTable.csv", fileEncoding = "UTF-8")
Lotung_villag$VILLNAME <- as.character(villagList[,4])

ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
LotungAgeData <- subset(ILanAgeData, substr(ILanAgeData$site_id,4,6)=="羅東鎮")
Lotung_villag$PopuDens <- as.numeric(LotungAgeData$PopuDens)
Lotung_villag$PopuDens65 <- as.numeric(LotungAgeData$PopuDens65)

#羅東人口密度地圖
pal <- colorBin("YlOrRd", domain = Lotung_villag$PopuDens)
Lotung_villag$labels <- paste0(Lotung_villag$VILLNAME,": ",round(Lotung_villag$PopuDens,digits = 1))

leaflet(Lotung_villag, options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(PopuDens), weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, label=~labels, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = pal, values = ~PopuDens, opacity = 0.7, title = "人口密度 (人/平方公里)", position = "bottomright")

#羅東老年人口密度地圖
pal <- colorBin("YlOrRd", domain = Lotung_villag$PopuDens65)
Lotung_villag$labels <- paste0(Lotung_villag$VILLNAME,": ",round(Lotung_villag$PopuDens65,digits = 1))

leaflet(Lotung_villag, options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(PopuDens65), weight=1, smoothFactor = 0.5, opacity = 1, fillOpacity = 1, label=~labels, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
  addLegend(pal = pal, values = ~PopuDens65, opacity = 0.7, title = "老年人口密度 (人/平方公里)", position = "bottomright")

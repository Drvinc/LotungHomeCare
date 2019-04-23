rm(list=ls())
Hosp1 <- read.csv("Hosp1.csv", fileEncoding = "UTF-8")
Hosp2 <- read.csv("Hosp2.csv", fileEncoding = "UTF-8")
Hosp3 <- read.csv("Hosp3.csv", fileEncoding = "UTF-8")

Hosptable <- function(x){
  y <- as.data.frame(x)
  y <- y[,c(1:5,7:10)]
  colnames(y) <- c("HospID", "HospName", "HostType", "Phone", "Address", "Level", "Service", "Speci", "Terminated")
  L <- c(1,2,3)
  names(L) <- c("地區醫院", "區域醫院", "醫學中心")
  y$Level <- L[as.character(y$Level)]
  Level <- y$Level[1]
  if (y$Level[1] != 3){
    y <- y[y$Terminated == "",]
  }
  y <- y[,-9]
  y
}

Hosp1 <- Hosptable(Hosp1)
Hosp2 <- Hosptable(Hosp2)
Hosp3 <- Hosptable(Hosp3)

TWHosp <- rbind(Hosp1, Hosp2, Hosp3)
ILanHosp <- TWHosp[substr(TWHosp$Address, 1, 2) == "宜蘭",]

#write.csv(ILanHosp, "ILanHosp.csv", fileEncoding = "UTF-8", row.names = FALSE)


#combine coordinates
ILanHosp <- read.csv("ILanHosp.csv", fileEncoding = "UTF-8")
HospAddre <- read.csv("HospAddress_Finish.csv", fileEncoding = "UTF-8")

ILanHosp$FullAddre <- HospAddre$Response_Address
ILanHosp$Town <- substr(HospAddre$Response_Address, 4, 6)
ILanHosp$Vill <- substr(HospAddre$Response_Address, 7, 9)
ILanHosp$Response_X <- HospAddre$Response_X
ILanHosp$Response_Y <- HospAddre$Response_Y
ILanHosp$WebSite <- paste0("<a href='","http://www.nhi.gov.tw/query/query3_detail.aspx?HospID=",ILanHosp$HospID,"' target='_blank'>醫院詳細資料</a>")

write.csv(ILanHosp, "ILanHospFull.csv", fileEncoding = "UTF-8", row.names = FALSE)


library(rgdal)
library(leaflet)

leaflet(data = HospAddre) %>%
  addTiles() %>%
  addMarkers(~Response_X, ~Response_Y, icons())
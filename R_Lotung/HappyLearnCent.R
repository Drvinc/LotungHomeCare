
# Happy Learn: json to csv
library(jsonlite)
library(leaflet)
rm(list=ls())
HappyLearnCent <- jsonlite::fromJSON("JsonYiHappyLearnCenteral.json")
df <- HappyLearnCent
write.csv(df, "HappyLearnCent.csv", fileEncoding = "UTF-8", row.names = FALSE)

# merge address data
df <- read.csv("HappyLearnCent.csv", fileEncoding = "UTF-8")
df_addre <- read.csv("HappyLearnCent_Address.csv", fileEncoding = "UTF-8")

HappyLearnCent <- df[,c(3:5,7)]
HappyLearnCent$full_addre <- df_addre$Response_Address
HappyLearnCent$Town <- substr(df_addre$Response_Address, 4, 6)
HappyLearnCent$Vill <- substr(df_addre$Response_Address, 7, 9)
HappyLearnCent$Response_X <- df_addre$Response_X
HappyLearnCent$Response_Y <- df_addre$Response_Y

write.csv(HappyLearnCent, "HappyLearnCentFull.csv", fileEncoding = "UTF-8", row.names = FALSE)

leaflet() %>%
  addTiles() %>%
  addMarkers(data=HappyLearnCent, ~Response_X, ~Response_Y)
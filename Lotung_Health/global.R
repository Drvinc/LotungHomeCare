#emulate ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#Pinyin to ChinChar
#function py: pinyin to chinese characters
Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
py <- function(x){
  y <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% x])
  return(y)
}
Lotung <- py("Lotung")
Yilan <- py("Yilan")
MedGroup <- py("MedGroup")
MedGroup_ <- py("MedGroup_")
LocateMe <- py("LocateMe")
BaseClinic <- py("BaseClinic")
WestClinic <- py("WestClinic")
HealthCent <- py("HealthCent")
ChinClinic <- py("ChinClinic")
DentClinic <- py("DentClinic")
persons <- py("persons")
OSM <- py("OSM")
Terrain <- py("Terrain")
clickshow <- py("clickshow")
JiaYi <- py("JiaYi")
NeiKe <- py("NeiKe")
BuFen <- py("BuFen")

fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}

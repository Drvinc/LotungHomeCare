

library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)

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
#Addre_LotungGroup <- subset(Addre, Addre$HomeDocGroupID == "1340202042")
Addre_ILanGroup <- subset(Addre, Addre$HomeDocGroupID != "")
IDLevels <- levels(Addre_ILanGroup$HomeDocGroupID)
Addre_ILanGroup$gID <- which(Addre_ILanGroup$HomeDocGroupID == IDLevels)
Addre_ILanHICare <- subset(Addre, Addre$HomeInteCareGroupID1 != "")
#Pinyin to ChinChar
Pinyin <- read.csv("Pinyin.csv", fileEncoding = "UTF-8")
MedGroup <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "MedGroup"])
HomeIntegratedCare <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "HomeIntegratedCare"])
MedGroup_ <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "MedGroup_"])
LocateMe <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "LocateMe"])
BaseClinic <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "BaseClinic"])
WestClinic <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "WestClinic"])
HealthCent <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "HealthCent"])
ChinClinic <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "ChinClinic"])
DentClinic <- as.character(Pinyin$ChinChar[Pinyin$Pinyin %in% "DentClinic"])
#ClinicIcon
icons <- makeIcon(iconUrl = "image/icon-clinicmap.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 30)

fmt_dcimals <- function(decimals=0){
  function(x) as.character(format(round(x,decimals)))
}

server <- function(input, output, session) {

  output$ClinPopRegression <- renderPlot({
    bin <- as.numeric(input$CutNumber)

    d <- ILanAgeData
    d$ID <- seq.int(nrow(d))
    d <- arrange(d, desc(PopuDens)) %>%
      mutate(
        cumsum = cumsum(summ),
        freq = round(summ / sum(summ), 3),
        cum_freq = cumsum(freq)
      )
    d$DensIndex <- c("")

    bins <- bin
    for (i in c(1:nrow(d))){
      grp <- freqcut(d$cum_freq[i],bins)
      d$DensIndex[i] <- grp
    }

    d <- arrange(d, ID)

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

    DF <- c[c$ClinN != 0,]
    DF <- data.frame(VAR1=DF$popDens, VAR2=DF$ClinDens, grp=DF$id)
    reg <- lm(log(VAR2)~log(VAR1),data=DF)
    reg1 <- reg
    coeff <- coefficients(reg)
    eq = paste0("y = ", round(coeff[2],3), "x ", ifelse(coeff[1]<0, "− ", "+ "), abs(round(coeff[1],3)))
    r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
    xlab <- "Population Density (persons/sqkm)"
    ylab <- "Clinic Density (1/sqkm)"

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
    p
  })
}




ui <- dashboardPage(
  dashboardHeader(title = "羅東鎮醫療地圖"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("基層診所地圖", tabName = "ClinicMap", icon = icon("medkit"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ClinicMap",
              h3("一、西醫基層診所密度"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("clindensmap")),tags$br(),
                       box(width = NULL, solidHeader = TRUE,
                           plotOutput("ClinPopRegression"))
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("CDIndexSelect"),
                           radioButtons("PIIndexType", "Populaton Density by:", choices = c("All" = 1, "Aged people" = 2),
                                        selected = 2),
                           radioButtons("CDIndexType", "index", choices = c("per area" = 1, "per person" = 2),
                                        selected = 2),
                           sliderInput("CutNumber",
                                       "按人口密度順序將人口__等分",
                                       min = 4,
                                       max = 100,
                                       value = 20,step=2)
                       )
                )
              )
      )
    )
  )
)

shinyApp(ui, server)
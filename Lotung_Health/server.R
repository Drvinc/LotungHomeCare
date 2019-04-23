library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(grid)

#delete slashes in names -- fxck UTF-8
dsl <- function(x){
  x <- gsub("/", "", as.character(x))
}
#readSHP
ILan_villag <- readOGR(dsn = "www", layer = "ILan_villages")
villagList <- read.csv("ILanVillTable.csv", fileEncoding = "UTF-8")
ILan_villag$TOWNNAME <- as.character(villagList[,4])
ILan_villag$VILLNAME <- as.character(villagList[,7])
ILan_villag$VILLNAME <- dsl(ILan_villag$VILLNAME)
Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")

ILan_town <- readOGR(dsn = "www", layer = "ILan_towns")
townList <- read.csv("ILanTownTable.csv", fileEncoding = "UTF-8")
ILan_town$TOWNNAME <- as.character(townList[,3])
ggpal <- colorFactor(palette = gg_color_hue(12), domain = ILan_town$TOWNNAME)

ClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")

#Read Clinic Data
Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
Addre$ClinName <- dsl(Addre$ClinName)
AddreW <- Addre[Addre$ClinType %in% c(1,2),]
Addre_ILanGroup <- Addre[is.na(Addre$HomeDocGroupID) == FALSE,]
Addre_ILanHICare <- Addre[Addre$HomeInteCareGroupID1 != "",]

#Read Hosp Data
H_Addre <- read.csv("ILanHospFull.csv", fileEncoding = "UTF-8")
H_Addre$HospName <- dsl(H_Addre$HospName)
H1_Addre <- H_Addre[H_Addre$Level %in% 1,]
H2_Addre <- H_Addre[H_Addre$Level %in% 2,]

#ClinicIcon
icons <- makeIcon(iconUrl = "icon-clinicmap.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 30)
H_icons <- iconList(
  L1 = makeIcon(iconUrl = "H1-ico.png", iconWidth = 34, iconHeight = 34, iconAnchorX = 17, iconAnchorY = 34),
  L2 = makeIcon(iconUrl = "H2-ico.png", iconWidth = 34, iconHeight = 34, iconAnchorX = 17, iconAnchorY = 34)
)

server <- function(input, output, session) {

  # Clinic Map in Tab1
  Addre1 <- Addre[Addre$ClinType %in% c(1,2),]

  SM <- reactiveValues(ShowM1 = FALSE, Show2_2 = FALSE, Show2_3 = FALSE, Show2_4 = FALSE, Show3_1 = FALSE, Show3_3 = FALSE, Show4_1 = FALSE)

    observeEvent(input$showmap, {
    SM$ShowM1 <- input$showmap
    leafletProxy("clinicmap") %>%
      clearTiles() %>%
      addTiles(group = py("OSM"), options = tileOptions(opacity = 0.8)) %>%
      addProviderTiles(providers$Stamen.Terrain, group = py("Terrain"), options = providerTileOptions(opacity = 0.7)) %>%
      addLayersControl(baseGroups = c(py("OSM"),py("Terrain")), options = layersControlOptions(collapsed = FALSE))
  })

  m0 <- leaflet(options = leafletOptions(minZoom = 9)) %>%
    fitBounds(121.7475,24.6641,121.8018,24.7069) %>%
    addEasyButtonBar(
      easyButton(
        icon="fa-home", title=Lotung,
        onClick=JS("function(btn, map){ map.fitBounds([[24.6641,121.7475],[24.7069,121.8018]]); }")),
      easyButton(
        icon="fa-globe", title=Yilan,
        onClick=JS("function(btn, map){ map.fitBounds([[24.3268,121.3130],[24.9914,121.9708]]); }"))) %>%
    addPolygons(data = ILan_town, fillColor = ~ggpal(TOWNNAME), fillOpacity = 0.2, weight = 1, color = "white",
                label=clickshow, labelOptions=labelOptions(clickable=TRUE, textsize="14px", textOnly = TRUE)) %>%
    addLabelOnlyMarkers(data = townList, ~labpt_x, ~labpt_y, label = ~TownName, labelOptions = labelOptions(noHide = T, direction = 'right', offset = c(-12,-6), textOnly = T, textsize = "12px"))

  output$clinicmap <- renderLeaflet({
    m0 %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title= LocateMe,
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))
  })

  data_of_click <- reactiveValues(clickedMarker=NULL)

  observeEvent(input$clinicmap_marker_click,{ #convex hull for community group
    data_of_click$clickedMarker <- input$clinicmap_marker_click

    pal <- colorFactor(palette = rainbow(8), domain = Addre_ILanGroup$HomeDocGroupID, reverse = FALSE)
    id <- as.character(data_of_click$clickedMarker$id)
    r <- which(Addre_ILanGroup$ClinCode == id)
    if (length(r) > 0 & as.numeric(input$ClinicService)==2){
      CG_ID <- as.factor(Addre_ILanGroup$HomeDocGroupID[r])
      CG_Name <- Addre_ILanGroup$HomeDocGroupName[r]
      AddreCG <- Addre_ILanGroup[Addre_ILanGroup$HomeDocGroupID == CG_ID,c(10:11)]
      C <- AddreCG
      hpts <- chull(C)
      C <- C[hpts,]
      leafletProxy("clinicmap") %>%
        addPolygons(data=C, ~Response_X, ~Response_Y, weight = 1, color = pal(CG_ID), fillColor = pal(CG_ID), highlightOptions = highlightOptions(color = pal(CG_ID), weight = 5),
                    label = paste0(CG_Name, MedGroup), labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                    layerId = "CG")
    }
  })

  output$clininfo <- renderUI({
    id <- as.character(data_of_click$clickedMarker$id)
    r <- which(Addre$ClinCode == id)
    h <- which(H_Addre$HospID == id)
    if (length(r) > 0){
      CG <- ifelse(Addre$HomeDocGroupName[r] == "", "nil.", paste0(as.character(Addre$HomeDocGroupName[r]), " ",MedGroup))
      tagList(
        "Clinic ID: ", id, tags$br(),
        "Clinic Name: ", Addre$ClinName[r], tags$br(),
        "Phone: ", Addre$Phone[r], tags$br(),
        "Address: ", Addre$Addre[r], tags$br(),
        "Services: ", Addre$Speci[r], tags$br(),
        "Area: ", Addre$Town[r], Addre$Vill[r], tags$br(),
        "NHI Webpage: ", HTML(as.character(Addre$WebSite[r])), tags$br(),
        "Community Group: ", CG, tags$br(),
        "Home Care Team: ", Addre$HomeInteCareGroupName1[r], Addre$HomeInteCareGroupName2[r]
      )
    }else{
      if (length(h) > 0){
        tagList(
          "Hospital ID: ", id, tags$br(),
          "Hospital Name: ", H_Addre$HospName[h], tags$br(),
          "Phone: ", H_Addre$Phone[h], tags$br(),
          "Address: ", H_Addre$Addre[h], tags$br(),
          "Services: ", H_Addre$Speci[h], tags$br(),
          "Area: ", H_Addre$Town[h], Addre$Vill[h], tags$br(),
          "NHI Webpage: ", HTML(as.character(H_Addre$WebSite[h])), tags$br(),
          "Level: ", H_Addre$Level[h], tags$br()
        )
      }else{
        tagList("nothing selected.")
      }
    }
  })


  observe({
    if (SM$ShowM1 == FALSE) return()
    Ser <- as.numeric(input$ClinicService)
    CC <- input$Clust_Contour
    if (Ser != 1){updateCheckboxGroupInput(session,"Clust_Contour", selected=CC[CC!=1])}

    Typ <- input$ClinicType
    Clu <- ifelse(1 %in% CC, 1, NA)
    Clu <- switch(Clu,markerClusterOptions())
    Cnt <- 2 %in% CC

    m <- leafletProxy("clinicmap") %>% clearShapes() %>% clearMarkers() %>% clearMarkerClusters()
    if (Cnt == T){
      m <- m %>% addPolygons(data=ILan_town, weight=1.5, smoothFactor = 1, opacity = 0.5,
                  fillOpacity = 0.01,
                  label=~TOWNNAME, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "coral", weight = 5))
    }
    if (Ser == 1){
      Addre1 <- Addre[Addre$ClinType %in% Typ,]
      if (!is.null(Typ)) {
        m <- m %>%
          addMarkers(data = Addre1, ~Response_X, ~Response_Y, icon = icons,
                     layerId=~ClinCode,
                     popup=~paste(sep = "<br/>", ClinName, WebSite),
                     label=~ClinName, labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                     options = markerOptions(riseOnHover =TRUE),
                     clusterOptions = Clu)
      }
    }else{
      if (Ser == 2){
        pal <- colorFactor(palette = rainbow(8), domain = Addre_ILanGroup$HomeDocGroupID, reverse = FALSE)
        m <- m %>%
          addMarkers(data=Addre_ILanGroup, ~Response_X, ~Response_Y, icon = icons, clusterOptions = Clu,
                     #popup = paste0(paste0(Addre_ILanGroup$HomeDocGroupName, MedGroup_),Addre_ILanGroup$ClinName), labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                     label = paste0(paste0(Addre_ILanGroup$HomeDocGroupName, MedGroup_),Addre_ILanGroup$ClinName), labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                     layerId=~ClinCode) %>%
          addCircleMarkers(data=Addre_ILanGroup, ~Response_X, ~Response_Y, radius = 10,
                           stroke = T, color = ~pal(HomeDocGroupID), fillOpacity = 0.6,
                           #popup = paste(sep="<br/>",paste0(Addre_ILanGroup$HomeDocGroupName, MedGroup_),Addre_ILanGroup$ClinName),
                           label = paste0(paste0(Addre_ILanGroup$HomeDocGroupName, MedGroup_),Addre_ILanGroup$ClinName), labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                           layerId=~ClinCode)
        }else{
        m <- m %>%
          addMarkers(data=Addre_ILanHICare, ~Response_X, ~Response_Y, icon = icons, clusterOptions = Clu,
                     popup=~ClinName, layerId=~ClinCode) %>%
          addCircleMarkers(data=Addre_ILanHICare, ~Response_X, ~Response_Y, radius = 11,
                           stroke = T, color="gold", fillOpacity = 0.6,
                           popup = paste(sep="<br/>", Addre_ILanHICare$HomeInteCareGroupName1, Addre_ILanHICare$HomeInteCareGroupName2, Addre_ILanHICare$ClinName),
                           layerId=~ClinCode)
        }
    }

    Hos <- input$HospSelect
    H_Addre1 <- H_Addre[H_Addre$Level %in% Hos,]
    if (length(Hos) > 0){
      m <- m %>%
        addMarkers(data = H_Addre1, ~Response_X, ~Response_Y,
                   icon = ~H_icons[Level],
                   layerId=~HospID,
                   popup=~paste(sep = "<br/>", HospName, WebSite),
                   label=~HospName, labelOptions = labelOptions(textsize = "13px",clickable = TRUE)
        )
    }
    m
  })

  # PopGraph in Tab2
  ILanAgeTown <- read.csv("ILanAgeTown.csv", fileEncoding = "UTF-8")

  output$PopGraph <- renderPlot({
    PG <- as.numeric(input$PopIndexType)
    if (PG == 1){
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
      c <- c + geom_line(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, group=1))
      c <- c + geom_point(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, group=1))
      c <- c + geom_text(data = df, aes(x = df$TownbySum, y = df$cumsum/1000, label=paste0(round(df$cum_freq*100), "%"), group=1), vjust = -0.7, hjust = -0.1)
      c <- c + labs(x = "", y = "Total population (kilopersons)")
      c <- c + geom_segment(x = 1, y = h, xend = 5, yend = h, linetype = 2, size=0.5, color="blue")
      c <- c + geom_text(aes(x=5, y=h, label = paste0(" Yilan County: ", format(round(h,1)), " K persons"), hjust=0, vjust=0.5, cex=5))
      c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
    }
    if (PG == 2){
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
      c <- c + geom_line(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, group=1))
      c <- c + geom_point(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, group=1))
      c <- c + geom_text(data = df, aes(x = df$TownbySum65, y = df$cumsum/1000, label= paste0(round(df$cum_freq*100), "%"), group=1), vjust = -0.7, hjust = -0.1)
      c <- c + labs(x = "", y = "Total elderly population (65 years and above in kilopersons)")
      c <- c + geom_segment(x = 1, y = h, xend = 5, yend = h, linetype = 2, size=0.5, color="blue")
      c <- c + geom_text(aes(x=5, y=h, label = paste0(" Yilan County: ", format(round(h,1)), " K persons"), vjust = 0.5, hjust=0, cex=5))
      c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
    }
    if (PG == 3){
      df <- ILanAgeTown[c(1:12),]
      df$TownbyAgedR <- with(df, reorder(TownName, AgedR))
      h <- ILanAgeTown$AgedR[13]*100
      c <- ggplot(df, aes(x = df$TownbyAgedR , y = df$AgedR*100, fill = TownName))
      c <- c + coord_cartesian(ylim = c(5, 20))
      c <- c + theme(legend.position="none")
      c <- c + geom_bar(stat="identity",width = 0.7)
      c <- c + geom_text(data = df, aes(x = df$TownbyAgedR, y = df$AgedR*100, label= format(round(df$AgedR*100, 1))), vjust=1.5)
      c <- c + labs(x = "", y = "Elderly population (65 years and above)(%)")
      c <- c + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
      c <- c + geom_hline(yintercept = h, linetype="dashed", color = "blue", size=0.5)
      c <- c + geom_text(aes(0, h, label = paste0("Yilan County: ", format(round(h,1)), " %"), hjust=-0.2, vjust=-1, size=12))
    }
    c
  })

  # Age Map in Tab2
  ILanAgeData <- read.csv("ILanAgeData.csv", fileEncoding = "UTF-8")
  LotungAgeData <- subset(ILanAgeData, substr(ILanAgeData$site_id,4,6)==Lotung)
  ILan_villag$EDR <- ILanAgeData$EDR
  ILan_villag$AGR <- ILanAgeData$AgedR
  ILan_villag$AGI <- ILanAgeData$AgIndex
  ILan_villag$summ <- ILanAgeData$summ
  ILan_villag$sum65 <- ILanAgeData$sum65abov
  ILan_villag$labelsEDR <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$EDR,digits = 3)," (",round(1/ILan_villag$EDR,digits = 1), ")")
  ILan_villag$labelsAGR <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$AGR*100,digits = 2), " %")
  ILan_villag$labelsAGI <- paste0(ILan_villag$TOWNNAME,ILan_villag$VILLNAME,": ",round(ILan_villag$AGI*100,digits = 2))

  EDRati <- py("EDRati")
  AGRate <- paste0(py("AGRate"), " (%)")
  AGIndx <- py("AGIndx")

  palAGR <- colorBin("YlOrRd", domain = ILan_villag$AGR*100, bins = c(0,7,14,20,ceiling(max(ILan_villag$AGR)*100)))

  observeEvent(input$show2_2, {
    SM$Show2_2 <- input$show2_2
    leafletProxy("agemap") %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.3))
  })

  output$agemap <- renderLeaflet({
    m0
  })
  observe({
    if (SM$Show2_2 == FALSE) return()
    if (is.null(input$AgeIndexType)){
      return(NULL)
    }else{
      By <- as.numeric(input$AgeIndexType)
      pal <- colorBin("YlOrRd", domain = switch(By,ILan_villag$labelsEDR,ILan_villag$AGR*100,ILan_villag$AGI*100),
                      bins = switch(By,c(0,0.125,0.167,0.2,0.25,0.333,0.5),
                                    c(0,7,14,20,ceiling(max(ILan_villag$AGR)*100)),
                                    c(0,50,100,200,300,ceiling(max(ILan_villag$AGI)*100))))

      leafletProxy("agemap", data = ILan_villag) %>%
        clearShapes() %>%
        addPolygons(data=ILan_villag, fillColor = ~pal(switch(By,EDR,AGR*100,AGI*100)),
                    weight=0.3, color = "white", smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8,
                    label=~switch(By,labelsEDR,labelsAGR,labelsAGI), labelOptions = labelOptions(textsize = "13px", clickable = TRUE),
                    highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
        addLegend(pal = pal, values = ~switch(By,EDR,AGR*100,AGI*100), opacity = 0.7, title = switch(By,EDRati,AGRate,AGIndx), position = "topright", layerId = "l") %>%
        addPolygons(data = ILan_town, fill = FALSE, weight = 1)
    }
  })

  # Population Map in Tab2
  ILan_villag$PopuDens <- as.numeric(ILanAgeData$PopuDens)
  ILan_villag$PopuDens65 <- as.numeric(ILanAgeData$PopuDens65)
  ILan_villag$labelsDens <- paste0(ILan_villag$TOWNNAME, gsub("/","",ILan_villag$VILLNAME),": ",round(ILan_villag$PopuDens,digits = 2), " (",ILan_villag$summ,persons,")")
  ILan_villag$labelsDens65 <- paste0(ILan_villag$TOWNNAME, gsub("/","",ILan_villag$VILLNAME),": ",round(ILan_villag$PopuDens65,digits = 2)," (",ILan_villag$sum65,persons,")")

  PopuDensity <- py("PopuDensity")
  PopuDensity65 <- py("PopuDensity65")

  palDens65 <- colorBin("YlOrRd", domain = ILan_villag$PopuDens65, bins = c(0,85,174,383,1635,ceiling(max(ILan_villag$PopuDens65))))

  observeEvent(input$show2_3, {
    SM$Show2_3 <- input$show2_3
    leafletProxy("densmap") %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.3))
  })

  output$densmap <- renderLeaflet({
    m0
  })
  observe({
    if (SM$Show2_3 == FALSE) return()
    if (is.null(input$DensType)){
      return(NULL)
    }else{
      By <- as.numeric(input$DensType)
      pal <- colorBin("YlOrRd", domain = switch(By,ILan_villag$PopuDens,ILan_villag$PopuDens65),
                      bins = switch(By,c(0,585,1208,3142,10172,ceiling(max(ILan_villag$PopuDens))),
                                    c(0,85,174,383,1635,ceiling(max(ILan_villag$PopuDens65)))))

      leafletProxy("densmap", data = ILan_villag) %>%
        clearShapes() %>%
        addPolygons(data=ILan_villag, fillColor = ~pal(switch(By,PopuDens,PopuDens65)),
                    weight=0.3, color = "white", smoothFactor = 0.5, opacity = 1, fillOpacity = 0.8,
                    label=~switch(By,labelsDens,labelsDens65), labelOptions = labelOptions(textsize = "13px", clickable = TRUE),
                    highlightOptions = highlightOptions(color = "green", weight = 3)) %>%
        addPolygons(data = ILan_town, fill = FALSE, weight = 1) %>%
        addLegend(pal = pal, values = ~switch(By,PopuDens,PopuDens65), opacity = 0.7, title = switch(By,PopuDensity,PopuDensity65), position = "topright", layerId = "l")
    }
  })
  # Aged Distribution in Tab2
  ILanVillGeo <- read.csv("ILanVillGeo.csv", fileEncoding = "UTF-8")
  ILanVillGeo$labpt_X <- as.numeric(ILanVillGeo$labpt_X)
  ILanVillGeo$labpt_Y <- as.numeric(ILanVillGeo$labpt_Y)
  ILanVillGeo$sum65abov <- as.numeric(ILanVillGeo$sum65abov)
  ILanVillGeo$Vill <- paste0(ILanVillGeo$TownName, dsl(villagList$VillagName))

  observeEvent(input$show2_4, {
    SM$Show2_4 <- input$show2_4
    leafletProxy("Aged_distr") %>%
      clearTiles() %>% clearShapes() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.15)) %>%
      addPolygons(data = ILan_town, fillColor = ~ggpal(TOWNNAME), fillOpacity = 0.2, weight = 1, color = "white",
                  label=~TOWNNAME, labelOptions = labelOptions(textsize = "13px",clickable = TRUE), highlightOptions = highlightOptions(color = "coral", weight = 3))
  })

  output$Aged_distr <- renderLeaflet({
    m0
  })
  observe({
    if (SM$Show2_4 == FALSE) return()
    CSize <- as.numeric(input$sizeScale)
    leafletProxy("Aged_distr") %>%
      clearMarkers() %>%
      addCircleMarkers(data=ILanVillGeo, ~labpt_X, ~labpt_Y, color ="coral", fillColor = "coral", radius = ~sqrt(sum65abov)*CSize/4,
                       label=~paste0(Vill, ": ", sum65abov, persons),
                       labelOptions = labelOptions(textsize = "13px",clickable = TRUE))
  })

  # Clinic in Towns in Tab3
  output$ClinGraph <- renderPlot({
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
    p <- p + geom_line(data = df, aes(x = df$TownbySum, y = df$cumsum, group =1))
    p <- p + geom_point(data = df, aes(x = df$TownbySum, y = df$cumsum, group =1))
    p <- p + geom_text(data = df, aes(x = df$TownbySum, y = df$cumsum, label= paste0(round(df$cum_freq*100), "%"), group =1), vjust = -0.7, hjust = -0.1)
    p <- p + labs(x = "", y = "Clinic Count")
    p <- p + geom_text(aes(0, h, label = paste0("Yilan County: ", h), group = "2", hjust=-0.2, vjust=2.5, size=12))
    p <- p + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5))
    p
  })

  # Clinic Aggregation Map in Tab3
  observeEvent(input$show3_1, {
    SM$Show3_1 <- input$show3_1
    leafletProxy("clinaggremap") %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.1))
  })

  output$clinaggremap <- renderLeaflet({
    m0
  })

  observe({
    d <- ILanAgeData
    d$clinNW <- ClinWGeo$ClinNW
    #d$ID <- seq.int(nrow(d))
    d <- arrange(d, desc(PopuDens65)) %>%
      mutate(
        cumsum = cumsum(sum65abov),
        freq = round(sum65abov / sum(sum65abov), 6),
        cum_freq = cumsum(freq),

        cumclinN = cumsum(clinNW),
        cum_clinP = cumclinN/sum(clinNW)
      )

    By <- as.numeric(input$CAlevel)/100
    for (i in 1:nrow(d)){
      id <- i
      if (!(d$cum_freq[i] < By)) break
    }

    if (SM$Show3_1 == FALSE) return()
    AggreA <- subset(ILan_villag, ILan_villag$PopuDens65 > d$PopuDens65[id])

    leafletProxy("clinaggremap", data = ILan_villag) %>%
      clearShapes() %>% clearMarkers() %>%
      addPolygons(data = ILan_town, color = "white", weight = 0.5, fillColor = ~ggpal(TOWNNAME), fillOpacity = 0.1) %>%
      addPolygons(data=AggreA, fillColor = "coral",
                  weight=0.5, smoothFactor = 1, opacity = 1, fillOpacity = 0.9,
                  label=~paste0(TOWNNAME,VILLNAME), labelOptions=labelOptions(clickable=TRUE, textsize = "13px"),
                  highlightOptions=highlightOptions(color="green", weight=2)) %>%
      addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, radius = 2,
                       label=~ClinName, labelOptions = labelOptions(textsize = "12px",clickable = TRUE))

    output$clinaggretext <- renderUI({
      tagList(
        "Highlighted population proportion (in the most densely populated areas): ", format(round(d$cum_freq[id]*100, 2)), "%", tags$br(),
        "Covered clinic proportion: ", format(round(d$cum_clinP[id]*100,2)), "% (", d$cumclinN[id], "/", sum(d$clinNW), ")"
      )
    })
  })

  # Clinic to popDens correlation graph, table and map in Tab3

  Dff <- reactiveValues(Reg1 = NULL, ClinGeo = NULL)

  output$clindensmap <- renderLeaflet({
    m0
  })

  observe({
    bin <- as.numeric(input$CutNumber)
    if (bin > 100) {updateNumericInput(session,"CutNumber", value=100)}
    if (bin < 3) {updateNumericInput(session,"CutNumber", value=3)}

    d <- ILanAgeData
    d$ID <- seq.int(nrow(d))
    d <- arrange(d, desc(PopuDens65)) %>%
      mutate(
        cumsum = cumsum(sum65abov),
        freq = round(sum65abov / sum(sum65abov), 5),
        cum_freq = cumsum(freq)
      )
    d$DensIndex <- c("")

    bins <- bin
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

    ClinWGeo$DensIndex <- as.numeric(d$DensIndex)
    ClinWGeo$sum65abov <- d$sum65abov
    ClinWGeo$cum_freq <- d$cum_freq

    c <- data.frame(id = numeric(0), clinN = numeric(0), pop = numeric(0), area = numeric(0), clind = numeric(0), popd = numeric(0), N_pop = numeric(0))
    for (i in 1:max(as.numeric(d$DensIndex))){
      target <- ClinWGeo[ClinWGeo$DensIndex == i,]
      s <- sum(target$ClinNW)
      p <- sum(target$sum65abov)
      a <- sum(target$vill_area)
      c1 <- c(i, s, p, a, s/a, p/a, s/p)
      c <- rbind(c, c1)
    }
    names(c) <- c("id","ClinN", "pop65", "villarea", "ClinDens", "pop65Dens", "N_pop")
    Dff$Reg1 <- c
    Dff$ClinGeo <- ClinWGeo
  })

  RegP <- eventReactive(input$show3_3, {
    c <- Dff$Reg1
    DF <- c
    DF_reg <- DF
    DF_reg <- DF_reg[DF_reg$ClinN != 0,]
    DF_reg <- data.frame(VAR1=DF_reg$pop65Dens, VAR2=DF_reg$ClinDens, grp=DF_reg$id)
    DF <- DF[DF$ClinN != 0,]
    DF <- data.frame(VAR1=DF$pop65Dens, VAR2=DF$ClinDens, grp=DF$id)
    DF_OL <- c[c$ClinN == 0,]
    DF_OL <- data.frame(VAR1=DF_OL$pop65Dens, grp=DF_OL$id)

    reg <- lm(log(VAR2)~log(VAR1),data=DF_reg)
    reg1 <- reg
    coeff <- coefficients(reg)
    eq = paste0("ln(y) = ", format(round(coeff[2],2)), "*ln(x) ", ifelse(coeff[1]<0, "- ", "+ "), format(abs(round(coeff[1],2)),nsmall=2))
    r_sq = paste0("R^2== " , round(summary(reg)$adj.r.squared, 2))
    xlab <- "Elderly Population Density (persons aged 65 years and above/sqkm)"
    ylab <- "Clinic Density (1/sqkm)"

    p <- ggplot(DF,aes(VAR1, VAR2, label = grp))
    p <- p + scale_x_continuous(trans='log', labels = fmt_dcimals(2))
    p <- p + scale_y_continuous(trans='log', labels = fmt_dcimals(2))
    p <- p + geom_point() + geom_smooth(method=lm, fill='lightblue', color='darkblue', size=1)
    p <- p + geom_label_repel(size=4,aes(label=DF$grp))
    p <- p + labs(x = xlab, y = ylab, title = "Clinic density to Elderly Population density (65 years and above): Yilan County")
    p <- p + theme(title=element_text(size=13),
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=13,face="bold"))
    y <- exp(layer_scales(p)$y$range$range[2])
    x <- exp(layer_scales(p)$x$range$range[1])
    y_OL <- exp(layer_scales(p)$y$range$range[1])
    if (nrow(DF_OL) > 0){
      p <- p + geom_point(data = DF_OL, aes(x= VAR1, y= y_OL))
      p <- p + geom_text_repel(data = DF_OL, size=4, aes(x= VAR1, y= y_OL, label=grp))
    }
    p <- p + geom_text(aes(x = x, y = y, label = eq), parse = FALSE, hjust = 0, vjust = 4.5, cex = 5)
    p <- p + geom_text(aes(x = x, y = y, label = r_sq), parse = TRUE, hjust = 0, vjust = 5, cex = 5)
    p
  })
  RegT <- eventReactive(input$show3_3, {
    t <- Dff$Reg1[,c(1:6)]
    t[,1] <- as.integer(t[,1])
    t[,2] <- as.integer(t[,2])
    t[,3] <- as.integer(t[,3])
    t[,4] <- format(round(t[,4], digits = 3))
    t[,5] <- format(round(t[,5], digits = 3))
    t[,6] <- format(round(t[,6], digits = 3))
    names(t) <- c("Group Id","Clinic Count", "Elderly Population (65 years and above)", "Group Area (sqkm)", "Clinic Density (1/sqkm)", "Elderly Population Density (1/sqkm)")
    t
  })

  output$ClinPopRegression <- renderPlot({
    text <- clickshow
    if (SM$Show3_3 == FALSE){
      ggplot() +
        annotate("text", x = 1, y = 1, size=6, label = text) +
        theme_gray() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }else
    RegP()
  })
  output$Regtable <- renderDataTable(RegT(), iris,
                                     options = list(searching = FALSE, pageLength = 10, dom = 'tp'))

  observeEvent(input$show3_3, {
    SM$Show3_3 <- input$show3_3
    ILan_villag$DensIndex <- Dff$ClinGeo$DensIndex
    df <- Dff$ClinGeo
    df$labpt_X <- ILanVillGeo$labpt_X
    df$labpt_Y <- ILanVillGeo$labpt_Y
    pal <- colorFactor(palette = rainbow(nrow(Dff$Reg1)), domain = Dff$Reg1$id)
    #pal <- colorFactor(palette = gray.colors(nrow(Dff$Reg1),start = 0, end = 1), domain = Dff$Reg1$id)
    leafletProxy("clindensmap") %>%
      clearShapes() %>% clearMarkers() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.15)) %>%
      addPolygons(data=ILan_villag, fillColor = ~pal(DensIndex), fillOpacity = 0.65, color="white", weight = 0.01,
                  label=~paste0("Group Id: ", DensIndex, " (", TOWNNAME, VILLNAME,")"),
                  labelOptions=labelOptions(clickable=TRUE, textsize="13px"),
                  highlightOptions=highlightOptions(color="white", weight=2, opacity=1)) %>%
      addPolygons(data=ILan_town, weight=1, color="white", smoothFactor = 1, opacity = 1, fill = FALSE) %>%
      addCircleMarkers(data = AddreW, ~Response_X, ~Response_Y, radius = 1, opacity = 0.9,
                       label=~ClinName, labelOptions=labelOptions(clickable=TRUE, textsize="12px"),
                       group="Show Clinic") %>%
      addLabelOnlyMarkers(data = df, ~labpt_X, ~labpt_Y,
                          label = ~paste0(DensIndex), labelOptions = labelOptions(noHide = T, direction = 'top', offset = c(0,-10), textOnly = T, textsize = "11.5px"),
                          group="Show Group ID") %>%
      addLayersControl(
        overlayGroups = c("Show Clinic", "Show Group ID"),
        options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Show Clinic")
  })

  # Tab4
  villGeo <- ILanVillGeo
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

  for (i in 1:nrow(AddreW)){
    isINT <- sapply(c(JiaYi,NeiKe,BuFen),grepl,AddreW$Speci[i])
    AddreW$isIN[i] <- as.logical(any(isINT))
  }
  AddreIN <- AddreW[AddreW$isIN == TRUE,]
  AddreGroup <- Addre[is.na(Addre$HomeDocGroupID) == FALSE,]
  AddreHome <- Addre[Addre$HomeInteCareGroupID1 != "",]

  output$coverageMap <- renderLeaflet({
    m0
  })
  observeEvent(input$show4_1, {
    SM$Show4_1 <- input$show4_1
    leafletProxy("coverageMap") %>%
      clearTiles() %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(opacity = 0.1))
  })
  observe({
    if (SM$Show4_1 == FALSE) return()
    team <- as.numeric(input$TeamType)
    radi <- as.numeric(input$CoverRad)
    SR <- (team-1)*4 + radi
    df <- switch(SR, NoClinVill2h, NoClinVill3h, NoClinVill5h, NoClinVill10h,
                 NoClinVill2g, NoClinVill3g, NoClinVill5g, NoClinVill10g,
                 NoClinVill2i, NoClinVill3i, NoClinVill5i, NoClinVill10i)
    team_df <- switch(team, AddreHome, AddreGroup, AddreIN)
    CSize <- as.numeric(input$sizeScale1)

    leafletProxy("coverageMap") %>%
      clearMarkers() %>% clearShapes() %>%
      addPolygons(data = ILan_town, color = "white", weight = 1, fillColor = ~ggpal(TOWNNAME), fillOpacity = 0.1) %>%
      addCircleMarkers(data = df, ~labpt_X, ~labpt_Y, color = "coral",
                       radius = ~sqrt(sum65abov)*CSize/4,
                       label = ~paste0(Vill,": ",sum65abov,persons),
                       labelOptions = labelOptions(textsize = "13px", clickable = TRUE),
                       group = "Unreached Villages") %>%
      addCircles(data = team_df, ~Response_X, ~Response_Y, color = "gold", fillOpacity = 0.2,
                 radius = switch(radi,2000,3000,5000,10000),
                 label = ~ClinName,
                 labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                 group = "Service Coverage") %>%
      addMarkers(data = team_df, ~Response_X, ~Response_Y,
                 label = ~ClinName,
                 labelOptions = labelOptions(textsize = "13px",clickable = TRUE),
                 group = switch(team, "Home Care Teams", "Community Group", "General practitioners")) %>%
      addLayersControl(
        overlayGroups = c("Unreached Villages",switch(team, "Home Care Teams", "Community Group", "General practitioners"), "Service Coverage"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, metric = TRUE, imperial = FALSE))
  })

  cvgData <- read.csv("cvrgData.csv", fileEncoding = "UTF-8")
  cvgData$coverag_dist <- factor(cvgData$coverag_dist, levels = c("2 km radius", "3 km radius", "5 km radius", "10 km radius"))
  cvgData$clinic_type <- factor(cvgData$clinic_type, levels = c("Home Care Team" , "Community Group" , "General Practitioner"))

  output$cvgPlot <- renderPlot({
    xlab <- "Putative coverage distance (km)"
    ylab <- "Covered elderly population (%)"
    ggplot(cvgData, aes(fill=clinic_type, y=ratio*100, x=coverag_dist, label = paste0(format(round(ratio*100,1)),"%"))) +
      geom_bar(position="dodge", stat="identity", width = 0.85) +
      geom_text(size = 4, position = position_dodge(width = 0.85), vjust = -1.1) +
      labs(x = xlab, y = ylab, title = "Elderly Population Coverage by Clinic Type and Service Distance") +
      guides(fill=guide_legend(title="Clinic Type")) +
      coord_cartesian(ylim = c(40, 100)) +
      theme(title=element_text(size=13),
            axis.text=element_text(size=12),
            axis.title=element_text(size=13,face="bold"),
            text = element_text(size=12),
            legend.text=element_text(size=12),
            legend.position="bottom")
  })

}

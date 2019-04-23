
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rgdal)
library(leaflet)

shinyServer(function(input, output, session) {

  #checkboxesRXN
  selAll <- observeEvent(input$checkAll, {
    updateCheckboxGroupInput(session,"ClinicType", selected=as.character(c(1:4)))
  })
  delALL <- observeEvent(input$checkNone, {
    updateCheckboxGroupInput(session,"ClinicType", selected=as.character(c()))
  })

  #readSHP
  ILan_villag <- readOGR(dsn = "ILan_villages", layer = "ILan_villages")
  Lotung_villag <- subset(ILan_villag, ILan_villag$TOWNID == "G06")
  #ReadClinicData
  Addre <- read.csv("ILanClinicFull.csv", fileEncoding = "UTF-8")
  Addre$Response_X <- as.numeric(Addre$Response_X)
  Addre$Response_Y <- as.numeric(Addre$Response_Y)
  Addre$ClinicName <- as.character(Addre$ClinicName)

  output$LotungMAP <- renderLeaflet({
    Type_select <- as.vector(input$ClinicType)
    Addre <- subset(Addre, Type %in% Type_select)

    leaflet(Addre, options = leafletOptions(minZoom = 10)) %>%
      addTiles() %>%
      addPolygons(data=Lotung_villag, weight=1, smoothFactor = 0.5) %>%
      addMarkers(~Response_X, ~Response_Y, popup=~as.character(ClinicName), clusterOptions = markerClusterOptions()) %>%
      fitBounds(121.7475,24.6641,121.8018,24.7069)

  })
})


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rgdal)
library(leaflet)

shinyUI(navbarPage("羅東鎮醫療地圖",
  tabPanel("羅東鎮醫療地圖",

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("ClinicType",
                         label = h2("基層診所"),
                         choices = list("西醫診所"=1,"衛生所"=2,"中醫診所"=3,"牙醫診所"=4),
                         selected = c(1:2)),
      actionButton("checkAll", label = "全選"),
      actionButton("checkNone", label = "清空"),
      #tags$style(type = "text/css", "#map {height: calc(100vh) !important;}")
      tags$head(tags$style("#LotungMAP{height: 90vh !important;}"))
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("LotungMAP")
    )
  )),
  tabPanel("統計"),
  tabPanel("關於")
))

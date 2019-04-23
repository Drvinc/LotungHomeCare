library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "羅東／宜蘭醫療地圖"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("基層診所地圖", tabName = "LotungMap", icon = icon("medkit")),
      menuItem("高齡人口相關數據", tabName = "LotungAgeMap", icon = icon("pie-chart")),
      menuItem("基層診所之分布情形", tabName = "ClinicMap", icon = icon("line-chart")),
      menuItem("居家醫療可涵蓋範圍之估計", tabName = "HomeTeamCoverage", icon = icon("home")),
      menuItem("長照資源", tabName = "OldCare", icon = icon("users")),
      menuItem("關於", tabName = "About", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "LotungMap",
              fluidRow(
                column(width = 9,
                       tags$head(tags$style("#clinicmap{height: 80vh !important; min-height: 500px}")),
                       box(width = NULL,
                           solidHeader = TRUE,
                           leafletOutput("clinicmap"), tags$hr(),
                           tags$h4("醫療院所資訊"),
                           htmlOutput("clininfo"))
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("ClinicTypeSelect"),
                           actionButton("showmap", label = "製圖"),tags$hr(),
                           radioButtons("ClinicService", label = NULL, choices = c("所有基層診所" = 1,
                                                                              "社區醫療群" = 2,
                                                                              "居家醫療照護整合計畫" = 3),
                                        selected = 1),
                           conditionalPanel(
                             condition = "input.ClinicService == '1'",
                             checkboxGroupInput("ClinicType", "基層診所類別", choices = c("西醫診所" = 1,
                                                                                        "衛生所" = 2,
                                                                                        "中醫診所" = 3,
                                                                                        "牙醫診所" = 4),
                                                selected = c(1, 2))
                           ),
                           tags$hr(),
                           checkboxGroupInput("HospSelect", label = NULL,
                                              choices = c("地區醫院" = 1, "區域醫院" = 2),
                                              selected = c()),
                           tags$hr(),
                           tags$img(src="cluster.png", height = 40),
                           checkboxGroupInput("Clust_Contour", label = NULL,
                                              choices = c("合併相鄰的標記" = 1, "顯示鄉鎮市界" = 2),
                                              selected = c(1,2))
                       )
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "LotungAgeMap",
              h3("一、宜蘭縣老年人口：各鄉鎮概況"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotOutput("PopGraph")
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("PopGraphSelect"),
                           radioButtons("PopIndexType", "人口統計類別", choices = c("總人口" = 1, "老年人口總數" = 2, "老年人口比例" = 3),
                                        selected = 2),
                           tags$hr(),
                           "註：老年人口定義為65歲及以上之人口。（OECD）"
                       )
                )
              ),
              h3("二、人口老化地圖"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("agemap")
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("AgeIndexSelect"),
                           actionButton("show2_2", label = "製圖"),tags$hr(),
                           radioButtons("AgeIndexType", "老化指標", choices = c("老年人口扶養比" = 1, "老年人口比例" = 2, "老化指數" = 3),
                                        selected = 2)
                       )
                )
              ),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("densmap")
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("DensSelect"),
                           actionButton("show2_3", label = "製圖"),tags$hr(),
                           radioButtons("DensType", "人口密度", choices = c("人口密度" = 1, "老年人口密度" = 2),
                                        selected = 2),
                           tags$hr(),
                           "註：每個人口密度區間大約涵蓋（老年）人口總數之20%。"
                       )
                )
              ),
              h3("三、老年人口實際分布 (65歲及以上)"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("Aged_distr")
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("popCirSize"),
                           actionButton("show2_4", label = "製圖"),tags$hr(),
                           sliderInput("sizeScale",
                                       "老年人口氣球大小",
                                       min = 1,
                                       max = 5,
                                       value = 2,step=1),
                           tags$hr(),
                           "註：氣球圓心為各村里輪廓之幾何中心。"
                       )
                )
              )
      ),

      # Third tab content
      tabItem(tabName = "ClinicMap",
        h3("一、西醫基層診所數量：各鄉鎮概況"),
        fluidRow(
          column(width = 9,
                 box(width = NULL, solidHeader = TRUE,
                     plotOutput("ClinGraph"))
          ),
          column(width = 3,
                 box(width = NULL, status = "warning",
                     uiOutput("CDIndexSelect"),
                     "宜蘭縣各鄉鎮之診所數。"
                 )
          )
        ),
        h3("二、人口密集區與診所分布之對應"),
        fluidRow(
          column(width = 9,
                 box(width = NULL, solidHeader = TRUE,
                     leafletOutput("clinaggremap"),
                     htmlOutput("clinaggretext"))
          ),
          column(width = 3,
                 box(width = NULL, status = "warning",
                     uiOutput("ClinAggreSelect"),
                     actionButton("show3_1", label = "製圖"),tags$hr(),
                     sliderInput("CAlevel",
                                 "老年人口密度最高之前__%人口區域",
                                 min = 20,
                                 max = 90,
                                 value = 40,step=10)
                 )
          )
        ),
        h3("三、診所密度與人口密度之相關性"),
        fluidRow(
          column(width = 9,
                 box(width = NULL, solidHeader = TRUE,
                     htmlOutput("RegHTML"),
                     conditionalPanel(
                       condition = "input.Map_Show == 1",
                       leafletOutput("clindensmap"),tags$hr()
                     ),
                     plotOutput("ClinPopRegression"),
                     conditionalPanel(
                       condition = "input.Table_Show == 1",
                       tags$hr(),
                       dataTableOutput("Regtable"))
                     )
          ),
          column(width = 3,
                 box(width = NULL, status = "warning",
                     uiOutput("RegSelect"),
                     #radioButtons("PIIndexType", "人口密度根據", choices = c("全部人口" = 1, "老年人口" = 2),
                     #              selected = 2),
                     sliderInput("CutNumber",
                                 "按老年人口密度順序將老年人口__等分",
                                 min = 3, max = 100,
                                 value = 20, step=1),
                     checkboxGroupInput("Map_Show", label = NULL,
                                        choices = c("顯示分組地圖" = 1),
                                        selected = c()),
                     checkboxGroupInput("Table_Show", label = NULL,
                                        choices = c("顯示資料表" = 1),
                                        selected = 1),
                     tags$hr(),
                     actionButton("show3_3", label = "製圖")
                 )
          )
        )
      ),

      # Fourth tab content
      tabItem(tabName = "HomeTeamCoverage",
              h3("一、居家醫療候選提供者可涵蓋之區域"),
              fluidRow(
                column(width = 9,
                       tags$head(tags$style("#coverageMap{height: 80vh !important; min-height: 500px}")),
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("coverageMap"))
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("coverageSelect"),
                           actionButton("show4_1", label = "製圖"), tags$hr(),
                           radioButtons("TeamType", label = "服務提供者", choices = c("居整計畫成員" = 1,
                                                                          "社區醫療群成員" = 2,
                                                                          "所有一般科診所" = 3),
                                        selected = 1),
                           tags$hr(),
                           radioButtons("CoverRad", label = "假定服務範圍半徑為：", choices = c("兩公里" = 1,
                                                                                 "三公里" = 2,
                                                                                 "五公里" = 3,
                                                                                 "十公里" = 4),
                                        selected = 1),
                           tags$hr(),
                           sliderInput("sizeScale1",
                                       "老年人口氣球大小",
                                       min = 1,
                                       max = 5,
                                       value = 2,step=1),
                           tags$hr(),
                           "註：一般科診所係指登記科別包含「內科」、「家醫科」、「不分科」之診所。"
                           )
                )
              ),
              h3("二、可涵蓋老年人口比例之比較"),
              fluidRow(
                box(width = 12, plotOutput("cvgPlot", height = "500px"))
              )
      ),

      # Fifth tab content
      tabItem(tabName = "OldCare",
              h3("長照資源"),
              tags$ul(
                tags$li("宜蘭縣長期照護服務管理所：",
                        tags$a(href="http://ltc.ilshb.gov.tw", "http://ltc.ilshb.gov.tw"),tags$br(),
                        tags$img(src = "pic-logo.png", height = "70px")),
                tags$li("衛服部《長期照顧資源地理地圖》：",
                        tags$a(href="http://ltcgis.mohw.gov.tw", "http://ltcgis.mohw.gov.tw"),tags$br(),
                        tags$img(src = "ltcgis.png", height = "70px"))
              )
      ),

      # Sixth tab content
      tabItem(tabName = "About",
              h3("資料來源"),
              tags$ul(
                tags$li("地理圖資：",
                        tags$a(href="http://data.gov.tw/node/7438", "村里界圖(TWD97經緯度)"),"（106年1月）、",
                        tags$a(href="http://data.gov.tw/node/7441", "鄉鎮市區界線(TWD97經緯度)"),"（106年3月）"),
                tags$li("人口資料：",
                        tags$a(href="http://data.moi.gov.tw/MoiOD/Data/DataDetail.aspx?oid=93B503C4-6799-400D-A3AE-A42464E91B3D", "村里戶數、單一年齡人口"),"（106年3月）"),
                tags$li("醫療院所：",
                        tags$a(href="http://data.nhi.gov.tw/Datasets/DatasetDetail.aspx?id=328", "健保特約醫事機構-診所"),"（105年11月）、",
                        tags$a(href="http://data.nhi.gov.tw/Datasets/DatasetDetail.aspx?id=327", "健保特約醫事機構-地區醫院"),"、",
                        tags$a(href="http://data.nhi.gov.tw/Datasets/DatasetDetail.aspx?id=326", "健保特約醫事機構-區域醫院")),
                tags$li("社區醫療群成員：",
                        tags$a(href="http://data.nhi.gov.tw/Datasets/DatasetDetail.aspx?id=121", "家庭醫師整合性照護計畫院所")),

                tags$li("居整計畫成員：",
                        tags$a(href="http://data.nhi.gov.tw/Datasets/DatasetDetail.aspx?id=319", "居家醫療整合計畫參與院所名單"))
              ),
              h3("使用工具"),
              tags$ul(
                tags$li("R語言（R version 3.4.0）：",
                        tags$a(href="https://www.r-project.org", "The R Project for Statistical Computing")),
                tags$li("地址轉座標：",
                        tags$a(href="https://www.tgos.tw/tgos/Web/Address/TGOS_Address.aspx", "TGOS全國門牌地址定位服務"))
              ),
              h3("作者"),
              tags$ul(
                tags$li("陳柏霖",tags$br(),
                "t24735281@gmail.com"))
      )
    )
  )
)

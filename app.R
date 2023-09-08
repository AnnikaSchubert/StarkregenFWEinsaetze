######################################################
##                 KARE ShinyApp:                   ##
##                                                  ##
##      Fire Brigade Operations due to flooding     ##
##            Caused by Heavy Rainfall              ##
##                                                  ##
##          Annika Schubert & Felix Bauer           ##
#                                                   ##
#                    08.09.2023                     ##
######################################################


# To dos:
# - Pfade mit here package setzen (Erklärung s. https://claudius-graebner.com/teaching/20_10_CosimaR/2_Projektsetup.pdf)
# - 

# Felix
#setwd('C:/Users/flexi/LRZ Sync+Share/Transfer_Hiwi (Anne von Streit)/Felix Bauer/Git/KARE Shiny/Feuerwehreinsaetze')
#data <- read.xlsx('C:/Users/flexi/LRZ Sync+Share/Transfer_Hiwi (Anne von Streit)/Felix Bauer/Git/KARE Shiny/Feuerwehreinsaetze/Firebrigade_Kopie.xlsx', sheet = 1)
#data <- read.xlsx('C:/Users/Felix/LRZ Sync+Share/Transfer_Hiwi (Anne von Streit)/Felix Bauer/Git/KARE Shiny/Feuerwehreinsaetze/Firebrigade_Kopie.xlsx', sheet = 1)

# Annika
setwd('D:/LRZ Sync+Share/Transfer_Hiwi (Anne von Streit)/Felix Bauer/Git/KARE Shiny/Feuerwehreinsaetze')



library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(openxlsx)
library(shiny)
library(shinyBS)



data <- read.xlsx('Firebrigade_Kopie.xlsx', sheet = 1)


ui <- fluidPage(
  theme = shinythemes::shinytheme('simplex'),
  tags$head(
    tags$style(HTML("
      .title-bar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 20px;
        background-color: #f5f5f5;
        border-bottom: 1px solid #ddd;
      }
      .logo-container {
        display: flex;
        align-items: center;
      }
      .button-container {
        display: flex;
        align-items: center;
        gap: 50px;
      }
      .slider-container {
        margin-right: 100px;
      }
      .toggle-container {
        margin-right: 10px;
      }
    "))
  ),
  div(class = "title-bar",
      h1("Feuerwehreinsätze aufgrund von Starkregen"),
      div(class = "logo-container",
          a(href="https://klimaanpassung-oberland.de/",
            img(src="KARE-Logo-quer-mitClaim-RGB.png",
                style="width: 15em; height: 5.7em;")
          )
      )
  ),
  
  fluidRow(
    column(width = 12,
           div(class = "button-container",
               div(class = "slider-container",
                   sliderInput('date_range', 'Select Date Range',
                               min = as.Date("2011-01-01"), max = as.Date("2021-12-31"),
                               value = c(as.Date("2011-01-01"), as.Date("2021-12-31")))
               ),
               div(class = "toggle-container",
                   shinyWidgets::switchInput("toggleButton", "Toggle Clustering", value = TRUE)
               ),
               actionButton('show_about', 'About')
           )
    )
  ),
  
  leaflet::leafletOutput('map', height = 'calc(100vh - 200px)')
)





server <- function(input, output, session) {
  
  observeEvent(input$show_about, {
    showModal(modalDialog("Placeholder"))
  })
  
  clusteringEnabled <- reactiveVal(TRUE)
  
  filtered_data <- reactive({
    data %>%
      filter(datum >= input$date_range[1], datum <= input$date_range[2])
  })
  
  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(11.4, 47.8, zoom = 10) %>%
      addCircleMarkers(
        data = filtered_data(),
        lat = ~lat,
        lng = ~lon,
        fillColor = 'red', color = 'red', weight = 1,
        fillOpacity = 0.5,
        clusterOptions = if (clusteringEnabled()) {
          markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            maxClusterRadius = 40,
            radius = filtered_data()$cases
          )
        } else {
          NULL
        }
      )
  })
  
  observeEvent(input$toggleButton, {
    clusteringEnabled(input$toggleButton)
    leafletProxy("map") %>%
      clearMarkerClusters() %>%
      clearMarkers()
    
    if (clusteringEnabled()) {
      leafletProxy("map") %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~lat,
          lng = ~lon,
          fillColor = 'red', color = 'red', weight = 1,
          fillOpacity = 0.5,
          clusterOptions = markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            maxClusterRadius = 40,
            radius = filtered_data()$cases
          )
        )
    } else {
      leafletProxy("map") %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~lat,
          lng = ~lon,
          fillColor = 'red', color = 'red', weight = 1,
          fillOpacity = 0.5
        )
    }
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (clusteringEnabled()) {
      leafletProxy("map") %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~lat,
          lng = ~lon,
          fillColor = 'red', color = 'red', weight = 1,
          fillOpacity = 0.5,
          clusterOptions = markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            maxClusterRadius = 40,
            radius = filtered_data()$cases
          )
        )
    } else {
      leafletProxy("map") %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~lat,
          lng = ~lon,
          fillColor = 'red', color = 'red', weight = 1,
          fillOpacity = 0.5
        )
    }
  })
  
}

shinyApp(ui, server)








#### Old code - just as backup
# server <- function(input, output, session) {
#   clusteringEnabled <- reactiveVal(TRUE)
#
#
#   output$map <- leaflet::renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       setView(11.4, 47.8, zoom = 10) %>%
#       addCircleMarkers(
#         data = data,
#         lat = ~lat,
#         lng = ~lon,
#         fillColor = 'red', color = 'red', weight = 1,
#         fillOpacity = 0.5,
#         clusterOptions = markerClusterOptions(
#           spiderfyOnMaxZoom = TRUE,
#           maxClusterRadius = 30
#         )
#       )
#   })
#
#
#   observeEvent(input$toggleButton, {
#     clusteringEnabled(input$toggleButton)
#     leafletProxy("map") %>%
#       clearMarkerClusters()
#
#     leafletProxy("map") %>%
#       clearMarkers()
#
#     leafletProxy("map") %>%
#       addCircleMarkers(
#         data = data,
#         lat = ~lat,
#         lng = ~lon,
#         fillColor = 'red', color = 'red', weight = 1,
#         fillOpacity = 0.5,
#         clusterOptions = if (clusteringEnabled()) {
#           markerClusterOptions(
#             spiderfyOnMaxZoom = TRUE,
#             maxClusterRadius = 40,
#             radius = data$cases
#           )
#         } else {
#           NULL
#         }
#       )
#
#   })
#
# }

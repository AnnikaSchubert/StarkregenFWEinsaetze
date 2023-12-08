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



# Irgendwann mal:
# - renv anlegen (Erklärung s. https://claudius-graebner.com/teaching/20_10_CosimaR/2_Projektsetup.pdf)


library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(openxlsx)
library(shiny)
library(shinyBS)
library(viridis)
library(here)

here::here()
data <- read.xlsx(here('data/Anonymised_FirebrigadeData.xlsx'),sheet = 'Sheet 1')


ui <- fluidPage(
  theme = shinythemes::shinytheme('simplex'),
  tags$head(
    tags$style(HTML("
      .title-bar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 20px 20px;
        background-color: #f5f5f5;
        border-bottom: 1px solid #ddd;
      }
      .logo-container {
        display: flex;
        align-items: center;
      }
      .slider-container h4,
      .checkbox-container h4{
      font-weight: bold;
      }
      .slider-container,
      .checkbox-container,
      .button-container {
        padding: 10px;
      }
    "))
  ),
  div(
    class = "title-bar",
    h1("Feuerwehreinsätze im Oberland aufgrund von Starkregen"),
    div(
      class = "logo-container",
      a(
        href = "https://klimaanpassung-oberland.de/",
        img(
          src = "KARE-Logo-quer-mitClaim-RGB.png",
          style = "width: 15em; height: 5.7em;"
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 4,
      align = 'center',
      div(
        class = "slider-container",
        sliderInput(
          'date_range', h4('Zeitraum auswählen'),
          min = as.Date("2011-01-01"), max = as.Date("2021-12-31"),
          value = c(as.Date("2011-01-01"), as.Date("2021-12-31"))
        )
      )
    ),
    column(
      width = 4,
      align = 'center',
      div(
        class = "checkbox-container",
        checkboxGroupInput(
          'overlap_filter', h4('Starkregenereignis als Einsatzgrund'), #Titel ändern?
          choices = list('Option 1' = 1, 'Option 2' = 2, 'Option 3' = 3, 'Option 4' = 4),#Anpassen für korrekte Beschriftung
          selected = 1:4
        )
      )
    ),
    column(
      width = 2,
      align = 'end',
      div(
        class = "button-container",
        actionButton('show_about', 'Mehr Informationen')
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      leaflet::leafletOutput('map', height = 'calc(100vh - 200px)')
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$show_about, {
    showModal(modalDialog(HTML("Die Webanwendung basiert auf Einsatzerhebungen der Feuerwehren im Zeitraum von Januar 2011 bis Dezember 2021. Der Datensatz umfasst alle Einsätze im Oberland, die in Zusammenhang mit Unwettern und/oder Überschwemmungen und/oder Wasserschäden stehen. Diese Daten wurden dem KARE-Team freundlicherweise von den Feuerwehren X, X, X und X zur Verfügung gestellt.<br><br> 
                          Die Daten wurden von der LMU München aufbereitet und vom IMK-IFU KIT georeferenziert. Um sicherzustellen, dass Einsätze mit hoher Wahrscheinlichkeit auf Starkregenereignisse zurückzuführen sind, wurden zwei Ansätze gewählt. Zunächst wurden alle Einsatzdaten mit dem Katalog der Starkregenereignissen (CatRaRE) verglichen. Für Einsätze, die weder zeitlich noch räumlich mit den Starkregenereignissen zusammenhingen, wurde zudem eine Medienanalyse in Zeitungen und auf Websites der Feuerwehr durchgeführt, um Einsätze aufgrund anderer Ursachen als Starkregen (z.B. Wasserrohrbruch) auszuschließen.<br><br> 
                          Um die Anonymität der betroffenen Haushalte zu gewährleisten, wurden die Daten verändert. Die GPS-Koordinate zeigt nicht den genauen Standort des betroffenen Gebäudes an, sondern variiert zufällig um etwa 40 m.<br><br>
                          Sollten Sie weitere Fragen haben, wenden Sie sich bitte an Annika.Schubert@lmu.de."), #Checkbox-Group erklären
                          footer = tagList(
                            modalButton("Schließen")
                          )))
  })
  
  set.seed(123)
  
  filtered_data <- reactive({
    data %>%
      filter(datum >= input$date_range[1], datum <= input$date_range[2]) %>%
      filter(overlap %in% input$overlap_filter) %>%
      mutate(
        lat = lat + (runif(n()) - 0.5) * 0.0004,
        lon = lon + (runif(n()) - 0.5) * 0.0004
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(11.2, 47.7, zoom = 10) %>%
      addCircleMarkers(
        data = filtered_data(),
        lat = ~lat,
        lng = ~lon,
        fillColor = 'grey',
        color = 'grey',
        weight = 1,
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,
          maxClusterRadius = 40,
          radius = filtered_data()$cases
        )
      )
  })
}

shinyApp(ui, server)

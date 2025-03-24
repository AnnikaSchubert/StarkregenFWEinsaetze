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


# load libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(openxlsx)
library(shinyBS)
library(viridis)
library(here)


# load data
here::here()
data <- read.xlsx(here('data/tidy/Anonymised_FirebrigadeData.xlsx'),sheet = 'Sheet 1')


# app
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
          'categ_filter', h4('Einsatzgrund Starkregen'), 
          choices = list('sehr hohe Wahrscheinlichkeit' = 1, 
                         'hohe Wahrscheinlichkeit' = 2),
          selected = 1:2
        )
      )
    ),
    column(
      width = 2,
      align = 'end',
      div(
        class = "button-container",
        actionButton('show_about', 'Informationen zur Anwendung')
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
    showModal(modalDialog(HTML("<b>Mit Hilfe dieser interaktiven Seite können Ortsbereiche identifizieren werden, in denen es gehäuft zu Überflutungen und Schäden durch Starkregen kommt.</b> <br><br>
                          Die Webanwendung basiert auf Einsatzerhebungen der Feuerwehren in den Landkreisen Bad Tölz-Wolfratshausen, Garmisch-Partenkirchen, Miesbach und Weilheim-Schongau im Zeitraum von Januar 2011 bis Dezember 2021. Diese Daten wurden dem KARE-Team freundlicherweise von den Kreisbrandräten des Oberlandes zur Verfügung gestellt.<br><br> 
                          Da das Tool lediglich Feuerwehreinsatzdaten darstellt und in einem Überflutungsfall nicht zwangsläufig die Feuerwehr kontaktiert wird, ist unsere Anwendung nicht erschöpfend. Auch an Orten, die kein gehäuftes Einsatzaufkommen aufweisen, kann es zu Überflutungen und Schäden durch Starkregen kommen. Nutzen Sie deswegen bitte zusätzlich die <a href='https://umweltatlas.bayern.de/mapapps/resources/apps/umweltatlas/index.html?lang=de&layers=lfu_domain-naturgefahren,service_naturgef_32,32;lfu_domain-naturgefahren,service_naturgef_33,33;lfu_domain-naturgefahren,service_naturgef_24,24&scale=18056&bm=webk'>Hinweiskarte Oberflächenabfluss und Sturzflut</a> des Bayerischen Landesamtes für Umwelt, um sich über mögliche Gefährdungen zu informieren.<br><br> 
                          <i>Datenaufbereitung</i> <br><br>
                          Da aus den Feuerwehrdaten nicht direkt ersichtlich wird, ob es sich um einen Einsatz handelt, der tatsächlich durch Starkregen ausgelöst wurde, wurden die Daten zunächst aufbereitet. Gemeinsam mit Einsatzkräften wurden entschieden für die Analyse nur Einsatzdaten mit den Einsatzstichwörtern ‚Unwettern‘ und/oder ‚Überschwemmungen‘ und/oder ‚Wasserschäden‘ zu verwenden. In einem zweiten Schritt wurden die Adressdaten georeferenziert und mit dem Katalog der Starkregenereignisse (<a href='https://www.dwd.de/DE/leistungen/catrare/catrare.html'>CatRaRE</a>) verglichen. Dieser Katalog listet alle Niederschlagsereignisse auf, die vom Deutschen Wetterdienst als heftiger Starkregen klassifiziert wurden (Warnstufe 3, Schwellenwert: 25-40 l/m² in 1 Stunde bzw. 35-60 l/m² in 6 Stunden). Die Einsätze konnte basierend auf den metrologischen Daten in drei Gruppen aufgeteilt werden:<br><br> 
                          <ol>
                          <li>Gruppe (<i>sehr hohe Wahrscheinlichkeit</i>): Einsätze, die sowohl räumlich als auch zeitlich mit den Starkregendaten übereinstimmen, können mit Sicherheit auf Starkregen zurückgeführt werden. Insgesamt sind dies 1952 Einsätze.</li>
                          <li>Gruppe (<i>hohe Wahrscheinlichkeit</i>): Einsätze, die entweder räumlich oder zeitlich mit den Starkregenereignissen überlappen, lassen sich ziemlich wahrscheinlich auf Starkregen zurückführen. Zudem konnten weitere Einsätze, die weder räumlich noch zeitlich mit den Starkregendaten übereinstimmen aber gehäuft in der gleichen Region auftraten, mittels einer Medienanalyse von Zeitungen und Webseiten der Feuerwehr auf Starkregenereignisse zurückgeführt werden. Insgesamt sind dies 854 Einsätze.</li>
                          <li>Gruppe (<i>nicht auf Starkregen zurückzuführen</i>): Einsätze, die mittels der Starkregendaten und/oder der Medienanalyse nicht mit Starkregen in Verbindung gebracht werden konnten, wurden aussortiert (insgesamt 1973 Einsätze).</li>
                          </ol>
                          <br><br>
                          <i>Was zeigt die Webanwendung?</i> <br><br>
                          In der Webanwendung werden grundsätzlich nur Feuerwehreinsätze angezeigt, die in Zusammenhang mit Starkregenereignissen stehen. Sie können über die Auswahlkästchen einstellen, ob lediglich Einsätze, die sicherlich (Gruppe 1) oder auch Einsätze, die wahrscheinlich durch Starkregenereignisse ausgelöst wurden (Gruppe 2), dargestellt werden sollen. Außerdem können Sie mit dem Zeitstrahl den angezeigten Zeitraum verändern.<br><br> 
                          Um die Anonymität der betroffenen Haushalte zu gewährleisten, wurden die Geodaten verändert. Die GPS-Koordinate zeigt nicht den genauen Standort des betroffenen Gebäudes an, sondern variiert zufällig um etwa 40 m.<br><br>
                          Sollten Sie weitere Fragen haben, wenden Sie sich bitte an <a href='https://www.geo.lmu.de/geographie/de/personen/kontaktseite/annika-schubert-b1e9ba80.html'>Annika.Schubert@lmu.de </a>.<br><br>
                          Beteiligte Forschende: Dr. Anne von Streit, Dr. Gamze Ko&#231;, Annika Schubert, Felix Bauer<br><br>
                          Der Softwarecode für diese Anwendung ist hier verfügbar: <a href='https://doi.org/10.17605/osf.io/jvz6m'>https://doi.org/10.17605/osf.io/jvz6m </a> (GNU General Public License v3.0)"), #Checkbox-Group erklären
                          footer = tagList(
                            modalButton("Schließen")
                          )))
  })
  
  filtered_data <- reactive({
    data %>%
      filter(datum >= input$date_range[1], datum <= input$date_range[2]) %>%
      filter(categ %in% input$categ_filter)
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

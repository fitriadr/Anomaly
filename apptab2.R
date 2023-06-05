# load library ====
library(shiny)
library(shinydashboard)

library(tidyverse)
library(echarts4r)
library(leaflet)
library(shinyWidgets)

raw_data <- read_csv(file = "file_ais_ext_aa.csv")

# bagian UI ====
ui <- dashboardPage(
  header = dashboardHeader(
    title = "AIS Data Monitoring V1"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Main Dashboard",
        icon = icon("dashboard"),
        tabName = "main_dashboard"
      ),
      menuItem(
        text = "Ship Monitoring",
        icon = icon("ship"),
        tabName = "ship_monitoring"
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "main_dashboard",
        h1("Main Dashboard"),
        fluidRow(
          ## last datetime value box ====
          valueBoxOutput(outputId = "main_dashboard_lastdatetime_valuebox", width = 6),
          ## total unknown ships value box ====
          valueBoxOutput(outputId = "main_dashboard_nunknownships_valuebox", width = 6)
        ),
        box(
          title = "Peta Posisi Terakhir",
          width = 12,
          ## Peta posisi terakhir ====
          leafletOutput(outputId = "main_dashboard_lastposition_map")
        ),
        fluidRow(
          box(
            title = "Total Jenis Kapal per Jam",
            ## Total Jenis Kapal Terdeteksi per Jam ====
            echarts4rOutput(outputId = "main_dashboard_ntypeshour_linechart")
          ),
          box(
            title = "Total Status Kapal per Jam",
            ## Total Status Kapal per Jam ====
            echarts4rOutput(outputId = "main_dashboard_nstatushour_linechart")
          )
        )
      ),
      tabItem(
        tabName = "ship_monitoring",
        h1("Vessel Analisys"),
        fluidRow(
          ## Pick Date and Time====
          column(
            width = 3,
            airDatepickerInput(
              inputId = "date",
              label = "Pick Date",
              timepicker = TRUE)
          ),
          ##Pick MMSI
          column(
            width = 3,
            selectInput(
              inputId = "MMSI",
              label = "Vessel MMSI",
              choices = raw_data$MMSI %>% unique())
          )
        ),
        actionButton(
          inputId = "Filter", 
          label = "Filter"
        ),
        uiOutput(outputId = "shipprofile_infobox")
      )
    )
  ))

# Bagian Server ====
server <- function(input, output, session) {
  ## last datetime value box ====
  output$main_dashboard_lastdatetime_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        pull(BaseDateTime) %>% 
        max(),
      subtitle = "Last Monitored Data",
      icon = icon("calendar")
    )
  })
  
  ## total unknown ships ====
  output$main_dashboard_nunknownships_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        select(MMSI, VesselName) %>% 
        filter(!is.na(VesselName)) %>% 
        distinct_all() %>% 
        nrow(),
      subtitle = "Total Unknown Ships",
      icon = icon("ship")
    )
  })
  
  ## peta posisi terakhir
  output$main_dashboard_lastposition_map <- renderLeaflet({
    raw_data %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng = ~LON,
        lat = ~LAT,
        clusterOptions = markerClusterOptions()
      )
  })
  
  ## total jenis kapal per jam
  output$main_dashboard_ntypeshour_linechart <- renderEcharts4r({
    raw_data %>% 
      mutate(BaseDateTime = floor_date(BaseDateTime, unit = "hours")) %>% 
      mutate(VesselType = ifelse(is.na(VesselType), "No Data", as.character(VesselType))) %>% 
      group_by(BaseDateTime, VesselType) %>% 
      summarise(Total_Kapal = n_distinct(MMSI)) %>% 
      ungroup() %>% 
      group_by(VesselType) %>% 
      e_charts(BaseDateTime) %>% 
      e_line(Total_Kapal) %>% 
      e_tooltip("axis")
  })
  
  ## total status kapal per jam
  output$main_dashboard_nstatushour_linechart <- renderEcharts4r({
    raw_data %>% 
      mutate(BaseDateTime = floor_date(BaseDateTime, unit = "hours")) %>% 
      mutate(Status = ifelse(is.na(Status), "No Data", as.character(Status))) %>% 
      group_by(BaseDateTime, Status) %>% 
      summarise(Total_Kapal = n_distinct(MMSI)) %>% 
      ungroup() %>% 
      group_by(Status) %>% 
      e_charts(BaseDateTime) %>% 
      e_line(Total_Kapal) %>% 
      e_tooltip("axis")
  })
  
  ev <- eventReactive(input$Filter,{
    raw_data %>% 
      filter(MMSI == input$MMSI) %>% 
      arrange(BaseDateTime) %>% 
      select(LON, LAT, BaseDateTime, VesselName, IMO, CallSign, TransceiverClass, Width)
  })
  
  #EventReactive 
  
  output$trajectory_plot <- renderLeaflet({
    ev() %>% 
      leaflet() %>% 
      addTiles() %>% 
      leaflet.extras2::addArrowhead(
        lng = ~LON,
        lat = ~LAT
      )
  })
  
  output$shipprofile_infobox <- renderUI({
    fluidRow(
      box(
        width = 6,
        title = "Vessel Traffic Map",
        leafletOutput("trajectory_plot")
      ),
      infoBox(
        width = 3,
        title = "Vessel Name",
        icon = icon("ship"),
        value = ev()$VesselName %>% unique()
      ),
      infoBox(
        width = 3,
        title = "IMO",
        icon = icon("hashtag"),
        value = ev()$IMO %>% unique()
      ),
      infoBox(
        width = 3,
        title = "Last Activities",
        icon = icon("clock"),
        value = ev()$BaseDateTime %>% max()
      ),
      infoBox(
        width = 3,
        title = "CallSign",
        icon = icon("phone"),
        value = ev()$CallSign %>% unique()
      ),
      infoBox(
        width = 3,
        title = "Transceiver Class",
        icon = icon("signal"),
        value = ev()$TransceiverClass %>% unique()
      ),
      infoBox(
        width = 3,
        title = "Ship Dimension",
        icon = icon("box"),
        value = ev()$Width %>% unique()
      )
    )
  })
  
  
  
  
  
}

shinyApp(ui, server)
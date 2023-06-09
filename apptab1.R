# load library ====
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(echarts4r)
library(leaflet)

# load dataset ====
raw_data <- read_csv(file = "file_ais_ext_aa.csv")

# Filter data untuk ditampilkan dalam box
vesselName <- c("All", unique(raw_data$VesselName))
vesselName
vesselType <- c("All", unique(raw_data$VesselType))
vesselType

# ui part ====
# bagian UI ====
ui <- dashboardPage(
  header = dashboardHeader(
    title = "AIS Data Monitoring V1"
  ),
  
  sidebar = dashboardSidebar(
    # Sidebar Menu Main Dashboard
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
    # Tombol Bookmark
    bookmarkButton(),
    
    tabItems(
      tabItem(
        tabName = "main_dashboard",
        h1("Main Dashboard"),
        
        # Input Box after date time
        fluidRow(
          # Input Box after date time
          column(
            width = 3,
            airDatepickerInput(
              inputId = "date",
              label = "After Date Time:",
              timepicker = TRUE)
          ),
          
          # Input Box before date time
          column(
            width = 3,
            airDatepickerInput(
              inputId = "date",
              label = "Before Date Time:",
              timepicker = TRUE)
          ),
          
          # Input Box vessel name
          column(
            width = 3,
            selectInput(
              inputId = "VesselName",
              label = "Vessel Name:",
              vesselName)
          ),
          
          # Input Box vessel type
          column(
            width = 3,
            selectInput(
              inputId ="VesselType",
              label = "Vessel Type:",
              vesselType)
          )
        ),
        fluidRow(
          ## Value Box Last Incoming Data ===
          valueBoxOutput(outputId = "main_dashboard_lastincomingdata_valuebox", width = 3),
          ## Value Box Number of Ship Recorded ===
          valueBoxOutput(outputId = "main_dashboard_numberofshiprecorder_valuebox", width = 3),
          ## Value Box Ships Not Under Command ===
          valueBoxOutput(outputId = "main_dashboard_shipsnotundercommand_valuebox", width = 3),
          ## Value Box Ships with Unknown Status ===
          valueBoxOutput(outputId = "main_dashboard_shipswithunknownstatus_valuebox", width = 3)
        ),
        
        box(
          # Map (Last Position)
          title = "Peta Posisi Terakhir",
          width = 12,
          ## Peta posisi terakhir ====
          leafletOutput(outputId = "main_dashboard_lastposition_map")
        ),
        
        fluidRow(
          # Activity Time Chart
          box(width = 3,
              title = "Activity Time Charts",
              ## Total Jenis Kapal Terdeteksi per Jam ====
              echarts4rOutput(outputId = "main_dashboard_activitytimecharts_linechart")
          ),
          
          # Vessel Type Distribution
          box(width = 3,
              title = "Vessel Type Distribution",
              ## Total Status Kapal per Jam ====
              echarts4rOutput(outputId = "main_dashboard_nstatushour_linechart")
          ),
          
          # Vessel Status Distribution
          box(width = 3,
              title = "Vessel Status Distribution",
              ## Total Status Kapal per Jam ====
              echarts4rOutput(outputId = "main_dashboard_statushour_linechart")
          ),
          
          # Transceiver Class Distribution
          box(width = 3,
              title = "Transceiver Class Distribution",
              ## Transceiver Class Distribution ====
              echarts4rOutput(outputId = "main_dashboard_transceiverclass_piechart")
          )
          
        )
      ),
      tabItem(
        tabName = "ship_monitoring",
        h1("Ship Monitoring")
      )
    ),
    
    
  ),
  
)

# Bagian Server ====
server <- function(input, output, session) {
  ## last incoming data value box ====
  output$main_dashboard_lastincomingdata_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        pull(BaseDateTime) %>% 
        max(),
      subtitle = "Last Incoming Data",
      icon = icon("calendar")
    )
  })
  
  ## total number of ships recorder ====
  output$main_dashboard_numberofshiprecorder_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        select(MMSI, VesselName) %>% 
        #filter(is.na(VesselName)) %>% 
        distinct_all() %>% 
        nrow(),
      subtitle = "Number of Ships Recorder",
      icon = icon("ship")
    )
  })
  
  ## total ships not under command ====
  output$main_dashboard_shipsnotundercommand_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        select(MMSI, VesselName) %>% 
        filter(!is.na(VesselName)) %>% 
        distinct_all() %>% 
        nrow(),
      subtitle = "Ships Not Under Command",
      icon = icon("x")
    )
  })
  
  ## total ships with uknown status ====
  output$main_dashboard_shipswithunknownstatus_valuebox <- renderValueBox({
    valueBox(
      value = raw_data %>% 
        select(MMSI, VesselName) %>% 
        filter(is.na(VesselName)) %>% 
        distinct_all() %>% 
        nrow(),
      subtitle = "Ships With Uknown Status",
      icon = icon("question")
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
  
  
  
  ## Activity Time Chart
  output$main_dashboard_activitytimecharts_linechart <- renderEcharts4r({
    raw_data %>%
      # group_by(BaseDateTime = floor_date(BaseDateTime, unit = "hours")) %>%
      # mutate(Status = ifelse(is.na(Status), "No Data", as.character(Status))) %>%
      # group_by(BaseDateTime, Status) %>%
      group_by(BaseDateTime = floor_date(BaseDateTime, "1 hour")) %>%
      summarise(count = n()) %>%
      e_charts(BaseDateTime) %>% 
      e_line(count) %>% 
      e_tooltip()
  })
  
  ## Vessel Type Distribution
  output$main_dashboard_nstatushour_linechart <- renderEcharts4r({
    raw_data %>% 
      group_by(VesselType) %>% 
      summarise(Total_Kapal = n_distinct(MMSI)) %>% 
      ungroup() %>% 
      e_charts(VesselType) %>% 
      e_polar() %>% 
      e_angle_axis() %>% 
      e_radius_axis(VesselType) %>% 
      e_bar(Total_Kapal, coord_system = "polar") %>% 
      e_tooltip("axis")
  })
  
  ## Vessel Status Distribution
  output$main_dashboard_statushour_linechart <- renderEcharts4r({
    raw_data %>% 
      group_by(Status) %>% 
      summarise(Total_Kapal = n_distinct(MMSI)) %>% 
      ungroup() %>% 
      e_charts(Status) %>% 
      e_polar() %>% 
      e_angle_axis() %>% 
      e_radius_axis(Status) %>% 
      e_bar(Total_Kapal, coord_system = "polar") %>% 
      e_tooltip("axis")
  })
  
  ## Transceiver Class Distribution
  output$main_dashboard_transceiverclass_piechart <- renderEcharts4r({
    raw_data %>% 
      # mutate(BaseDateTime = floor_date(BaseDateTime, unit = "hours")) %>%  
      # mutate(Status = ifelse(is.na(Status), "No Data", as.character(Status))) %>% 
      # group_by(BaseDateTime, Status) %>% 
      # summarise(Total_Kapal = n_distinct(MMSI)) %>% 
      # ungroup() %>% 
      # mutate(BaseDateTime = as.character(BaseDateTime)) %>% 
      count(TransceiverClass) %>% 
      e_charts(TransceiverClass) %>% 
      e_pie(n) %>% 
      e_tooltip()
  })
}

shinyApp(ui, server, enableBookmarking = "url")
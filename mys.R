# Library yang digunakan
library(shiny)
library(leaflet)
library(tidyverse)
library(rsconnect)
library(cowplot)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(shinyDatetimePickers)
library(DT)
library(tidyverse)
library(leaflet)
library(secr)
library(sp)
library(leaflet.extras2)
library(shiny)
library(leaflet.extras)
library(shinyWidgets)
library(shinythemes)

# Dashboard 1
ui1 <- dashboardPage(
  dashboardHeader(title = "CHART GENERATOR", titleWidth = 230),
  dashboardSidebar(
    tags$figure(align = "center", 
                br(),
                #tags$img(src="https://i.postimg.cc/dtmsXcSt/antasena.png",
                tags$img(src="https://i.postimg.cc/FHxcFcf3/LOGO-ANTASENA.png",
                         width = 200, 
                         height = 180)),
    numericInput("latitude","LATITUDE:",value=10),
    numericInput("longitude","LONGITUDE:",value=10),
    numericInput("num3", "MMSI:",value=10),
    br(),
    datetimeMaterialPickerInput
    ("dtmpicker",label = "Base Date Time", disablePast = TRUE),
    br(),
    downloadButton('download','DOWNLOAD'),
    actionButton("generate","GENERATE", class = "btn btn-default action-button"),
    actionButton("ukur","MEASURE"),
    actionButton('reset','RESET')
  ),
  
  dashboardBody(
    fluidRow(
      column(
        align = "left",
        height = 2,
        width = 8,
        h1(uiOutput("waktu"))
      )
    ),
    fluidRow(
      column(
        width = 9,
        leafletOutput("map", height = "780px")
      ),
      column(
        width = 3,
        tableOutput("data_table")
      )
    )
  )
)

#===============================================================================

# Dashboard 2
data_koordinat <- read_csv("file_ais_ext_aa.csv")
ui2 <- fluidPage(
  titlePanel(title ="SHIP TRACKER"),# titleWidth = 230),
  leafletOutput("peta_1", height = "780px"))

server2 <- function(input, output, session) {
  # server logic untuk dashboard 2
  output$peta_1 <- renderLeaflet({
    data_koordinat  %>% 
      select(MMSI, LON, LAT, BaseDateTime) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == min(BaseDateTime)) %>% 
      ungroup() %>% 
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~LON, lat = ~LAT )%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "red",
        localization = "id"
      )
  })
}

# ==============================================================================
# Dashboard 3
#baca data
raw_data <- read_csv("file_ais_ext_aa.csv")

y_coord <- c(45,  55, 60, 53)
x_coord <- c(-121, -119, -120, -130)
xym <- cbind(x_coord, y_coord)



p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
data_poly = data.frame(anomaly=99.9)
spanomaly = SpatialPolygonsDataFrame(sps,data_poly)


ui3 <- dashboardPage(
  skin = "black",  
  dashboardHeader(title = "Navy Anomaly Apps"),
  dashboardSidebar(
  ),
  dashboardBody(
    fluidRow(
      box(
        width = 9,
        title = "Peta",
        leafletOutput(outputId = "View_maps")
      ),
      box(
        width = 3,
        title = "Details Trajectory",
        uiOutput("trajectory_plot")
      )  ),
    fluidRow(
      valueBoxOutput("value_jumlah_kapal"),
      valueBoxOutput("value_alert"),
      # valueBoxOutput("tinggi_gelombang")
      valueBox(
        value = "100",
        subtitle = "Tinggi gelombang",
        icon = icon("angle-up"),
        color = "blue",
        width = 4
      ) ) 
  ))

#================================================================================

ais_data <- read_csv("file_ais_ext_aa.csv")
maritime_sec <- read_csv(file = "alki2.csv")


ui4 <- fluidRow(
  column(
    width = 3,
    airDatepickerInput(
      inputId = "nav2_datetimepicker_input_1",
      label = "Input Date (After)",
      value = min(maritime_sec$timestamp),
      timepicker = T
    )
  ),
  column(
    width = 3,
    airDatepickerInput(
      inputId = "nav2_datetimepicker_input_2",
      label = "Input Date (Before)",
      value = min(maritime_sec$timestamp) + 3600*24*30,
      timepicker = T
    )
  ),
fluidRow(
  valueBoxModuleUI(
    id = "nav2_last_date", 
    subtitle = "Last Date",
    icon = icon("calendar"),
    width = 3
  ),
  valueBoxModuleUI(
    id = "nav2_total_ships",
    subtitle = "Total Ships",
    icon = icon("ship"),
    width = 3
  ),
  valueBoxModuleUI(
    id = "nav2_days_to_ffd",
    subtitle = "Days to Next FFD",
    icon = icon("fish"),
    width = 3
  ),
  valueBoxModuleUI(
    id = "nav2_tanggal_pasang",
    subtitle = "Gelompang Pasang Selanjutnya",
    icon = icon("exclamation-triangle"),
    width = 3
  )
),
fluidRow(
  column(
    width = 9,
    leafletOutput(outputId = "nav2_aismap", height = "50vh")
  )
),
fluidRow(
  column(
    width = 9,
    tags$h1("Total Alerts Recorded"),
    echarts4rOutput(outputId = "nav2_total_alert_output")
  )
),
fluidRow(
  column(
    width = 9,
    tags$h1("Total Ship Types Recorded"),
    echarts4rOutput(outputId = "nav2_total_ship_types")
  )
),
fluidRow(
  column(
    width = 9,
    tags$h1("Hourly Activity"),
    echarts4rOutput(outputId = "nav2_hourly_activity")
  )
)
)



# ==============================================================================

## Gabungan 
# Menggabungkan kedua dashboard dengan multi-tab
ui <- navbarPage(
  "ANTASENA",
  tabPanel("CHART GENERATOR", ui1),
  tabPanel("SHIP TRACKER", ui2),
  tabPanel("ANOMALY DETECTION",ui3),
  tabPanel("KONSERVASI",ui4)
)

server <- function(input, output, session) {
  # server logic untuk dashboard gabungan
  # server logic untuk dashboard 1
  # Membuat peta leaflet
  output$map <- renderLeaflet({
    if(is.null(nrow(data$click_data))) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 106.8114, lat = -6.0228, zoom = 12)
    }
    else {data$click_data %>% 
        leaflet() %>%
        addTiles() %>%
        setView(lng = 106.8114, lat = -6.0228, zoom = 12) %>%
        addCircles(lng = ~longitude, lat = ~latitude) 
    }
  }
  )
  
  output$leaflet1<-renderLeaflet({
    leaf <- leaflet() %>%
      addTiles() %>%
      setView(lng = 106.8114, lat = -6.0228, zoom=12)%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "red",
        localization = "id"
      )
  }
  )
  
  leafletOutput("leaflet1")
  
  # Membuat data kosong
  data <- reactiveValues(click_data = 
                           data.frame(MMSI = numeric(),
                                      latitude = numeric(),
                                      longitude = numeric(),
                                      time = numeric()))
  
  # Memfungsikan tombol download
  output$download <- downloadHandler(
    filename = function() {
      paste("Chrt_Gen", '.csv', sep='')
    },
    content = function(file) {
      write.csv(data$click_data, file)
    })
  
  # Memfungsikan tombol measure
  observeEvent(input$ukur,{
    leafletProxy("map")%>%
      addMeasure() 
  })
  
  # Menambahkan fitur klik ke peta dan memperbarui tabel dengan nilai yang diklik
  observeEvent(input$map_click, {
    click_data <- data.frame( MMSI = input$num3,
                              latitude = input$map_click$lat,
                              longitude = input$map_click$lng,
                              time = input$dtmpicker)
    data$click_data <- rbind(data$click_data, click_data)
  })
  
  # Menampilkan tabel data
  output$data_table <- renderTable({
    data$click_data
  })
  
  # Mengaktifkan fungsi bergerak setiap detik  
  output$waktu <- renderText({
    invalidateLater(1000, session) 
    paste0(Sys.time())
  })
  
  # Mengaktifkan fungsi generate  
  observeEvent(input$generate, {
    click_data <- data.frame(MMSI = input$num3,
                             latitude = input$latitude,
                             longitude = input$longitude,
                             time=input$dtmpicker)
    data$click_data <- rbind(data$click_data, click_data)
  })
  
  # Mengaktifkan fungsi download  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("Chart_Gen", '.csv', sep='') },
    content = function(file) {
      write.csv(data, file)
    })
  # Memfungsikan tombol reset
  observeEvent(input$reset,{
    leafletProxy("map", data = NULL) %>%
      clearShapes()
  })
  # Memfungsikan tombol reset2
  observe({
    if (input$reset == 0)
      return()
    data$click_data <- NULL
  })
  # Aksi saat peta di klik
  observeEvent(input$map_click,{
    click <- input$map_click
    leafletProxy("map", data = NULL) %>%
      setView(lng = click$lng, lat = click$lat, zoom = 10) %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  # server logic untuk dashboard 2
  output$peta_1 <- renderLeaflet({
    data_koordinat  %>% 
      select(MMSI, LON, LAT, BaseDateTime) %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == min(BaseDateTime)) %>% 
      ungroup() %>% 
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~LON, lat = ~LAT )%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "red",
        localization = "id"
      )
  })
  
  
  #server logic untuk dashboard 3
  
  
  server3 <- function(input, output, session){
    raw_data_filter <- reactive({
      raw_data %>% 
        group_by(MMSI) %>% 
        count() %>% 
        ungroup() %>% 
        filter(n >= 200) %>% pull(MMSI) -> mmsi_for_filter
      raw_data %>% 
        filter(MMSI %in% mmsi_for_filter) -> ais_data_for_app
      ais_data_for_app %>% 
        select(MMSI, BaseDateTime, LON, LAT) %>% 
        group_by(MMSI) %>% 
        filter(BaseDateTime == max(BaseDateTime)) %>% 
        ungroup() %>% 
        select(MMSI, LON, LAT) -> ais_data_for_app
      ais_data_for_app %>% 
        select(LON, LAT) %>% 
        as.matrix() -> lon_lat_matrix
      rownames(lon_lat_matrix) <- ais_data_for_app$MMSI
      pointsInPolygon(lon_lat_matrix, xym) -> is_anomaly
      ais_data_for_app %>% 
        mutate(is_anomaly = is_anomaly) %>% 
        select(MMSI, LON, LAT, is_anomaly) %>% 
        mutate(is_anomaly = factor(is_anomaly))
    })
    output$View_maps <- renderLeaflet({
      pal <- colorFactor(c("blue","red"), domain = c("TRUE","FALSE"))
      raw_data_filter() %>% 
        leaflet() %>% 
        addTiles() %>% 
        addPolygons(data= spanomaly) %>% 
        addCircleMarkers(lng = ~LON, lat = ~LAT, color = ~pal(is_anomaly),layerId = ~MMSI)
    })
    data_trajectory <- reactive({
      req(input$View_maps_marker_click$id)
      raw_data %>% 
        filter(MMSI == input$View_maps_marker_click$id) %>% 
        # arrange dataset based on BaseDateTime ascendingly
        arrange(BaseDateTime) %>% 
        # get LON, LAT, and BaseDateTime column
        select(MMSI, LON, LAT, BaseDateTime) %>% 
        inner_join(
          raw_data_filter() %>% 
            select(MMSI, is_anomaly), by=c("MMSI"="MMSI")  ) 
    })
    output$trajectory_plot <- renderUI({
      req(data_trajectory)
      if(nrow(data_trajectory() == 0)){
        print(data_trajectory())  
        data_trajectory() %>% pull(is_anomaly) %>% unique() -> status
        if(status == FALSE){
          data_trajectory() %>% 
            leaflet() %>% 
            addTiles() %>% 
            addAntpath(lng = ~LON, lat = ~LAT, color = "blue")
        }else{
          data_trajectory() %>% 
            leaflet() %>% 
            addTiles() %>% 
            addAntpath(lng = ~LON, lat = ~LAT, color = "red")   }
      } else{
        paste0("template")   }   
    })
    output$value_jumlah_kapal <- renderValueBox({
      valueBox(
        value = raw_data %>% pull(MMSI) %>% unique() %>% length(),
        subtitle = "Jumlah Kapal", 
        icon = icon("ship"),
        color = "blue"  )
    })
    output$value_alert <- renderValueBox({
      valueBox(
        value = raw_data_filter() %>%  filter(is_anomaly == TRUE)%>% pull(MMSI) %>% unique() %>% length(),
        subtitle = "Jumlah Alert", 
        icon = icon("triangle-exclamation"),
        color = "maroon"  )
    })  
  }
  
  #Serverfortab4  
  # code for Nav1
  filtered_data <- reactive({
    print(ais_data)
    ais_data %>% 
      arrange(BaseDateTime) %>% 
      filter(
        BaseDateTime >= input$nav1_datetimepicker_input_1,
        BaseDateTime <= input$nav1_datetimepicker_input_2
      )
  })
  
  leaflet_output <- reactive({
    req(filtered_data())
    
    filtered_data() %>%   
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      ungroup() %>% 
      leaflet() %>% 
      addTiles() %>% 
      a_ddFullscreenControl() %>% 
      addCircleMarkers(
        lng = ~LON, 
        lat = ~LAT, 
        layerId = ~MMSI,
        popup = ~paste0(
          cntnt
        )
      ) 
  })
  
  #color pletter for category type in data file
  
  output$nav1_aismap <- renderLeaflet({
    leaflet_output()
  })
  
  observeEvent(input$nav1_model_button, {
    all_data <- model_script(filtered_data() %>% select(-cntnt), input$nav1_epsilon_input, input$nav1_min_point_input)

    all_data %>%
      mutate(
        cntnt=paste0('<strong>Name: </strong>',VesselName,
                     '<br><strong>MMSI:</strong>', MMSI,
                     '<br><strong>Last Observed:</strong>', BaseDateTime,
                     '<br><strong>SOG:</strong>', SOG,
                     '<br><strong>COG:</strong>', COG,
                     '<br><strong>Heading:</strong>', Heading,
                     '<br><strong>IMO:</strong>', IMO,
                     '<br><strong>Call Sign:</strong>', CallSign,
                     '<br><strong>Vessel Type:</strong>', VesselType,
                     '<br><strong>Status:</strong>', Status,
                     '<br><strong>Cargo:</strong>', Cargo,
                     '<br><strong>Transceiver Class:</strong>', TransceiverClass,
                     '<br><strong>Mean Speed:</strong>', mean_speed,
                     '<br><strong>SD Speed:</strong>', sd_speed,
                     '<br><strong>Min Speed:</strong>', min_speed,
                     '<br><strong>Max Speed:</strong>', max_speed,
                     '<br><strong>Mean Acceleration:</strong>', mean_acc,
                     '<br><strong>SD Acceleration:</strong>', sd_acc,
                     '<br><strong>Min Acceleration:</strong>', min_acc,
                     '<br><strong>Max Acceleration:</strong>', max_acc,
                     '<br><strong>Mean Direction:</strong>', mean_dir,
                     '<br><strong>SD Direction:</strong>', sd_dir,
                     '<br><strong>Straightness:</strong>', straightness,
                     '<br><strong>Sinusoity:</strong>', sinusoity,
                     '<br><strong>Expected Displacement:</strong>', ex_disp,
                     '<br><strong>Cluster:</strong>', cluster)
      ) -> all_data



    sort(unique(all_data$cluster)) -> cluster_code

    pal <- colorFactor(palette = "viridis", domain = cluster_code)


    output$nav1_cluster_filter <- renderUI({
      req(all_data)
      selectInput(
        inputId = "nav1_cluster_filter_select",
        choices = c("semua", "hanya outlier"),
        label = "Filter Data",
        selected = "semua"
      )
    })


    output$nav1_aismap <- renderLeaflet({
      req(input$nav1_cluster_filter_select)
      if (input$nav1_cluster_filter_select == "semua") {
        all_data %>%
          leaflet() %>%
          addTiles() %>%
          addFullscreenControl() %>%
          addCircleMarkers(
            lng = ~LON,
            lat = ~LAT,
            color = ~pal(cluster),
            layerId = ~MMSI,
            radius = ~ifelse(cluster ==0, 20, 10),
            popup = ~paste0(
              cntnt
            ),
            clusterOptions = markerClusterOptions()
          )

      } else {
        all_data %>%
          filter(cluster == 0) %>%
          leaflet() %>%
          addTiles() %>%
          addFullscreenControl() %>%
          addCircleMarkers(
            lng = ~LON,
            lat = ~LAT,
            color = ~pal(cluster),
            layerId = ~MMSI,
            radius = ~ifelse(cluster ==0, 20, 10),
            popup = ~paste0(
              cntnt
            ),
            clusterOptions = markerClusterOptions()
          )
      }
    })

  })
  
  
  output$nav1_trajectory_plot <- renderPlot({
    req(input$nav1_aismap_marker_click)
    filtered_data() %>% 
      filter(MMSI == input$nav1_aismap_marker_click$id) %>% 
      # arrange dataset based on BaseDateTime ascendingly
      arrange(BaseDateTime) %>% 
      # get LON, LAT, and BaseDateTime column
      select(LON, LAT, BaseDateTime) -> traj_data
    
    
    print(traj_data)
    
    
    # define new variable: time
    traj_data$time <- traj_data$BaseDateTime - traj_data$BaseDateTime[1]
    
    # from traj_data
    traj_data %>% 
      # get LON, LAT, and time
      select(LON, LAT, time) %>% 
      # change time to integer
      mutate(time = as.integer(time))-> traj_data
    
    # check traj_data
    traj_data
    
    # create trajectory from data
    TrajFromCoords(
      track = traj_data,
      xCol = "LON",
      yCol = "LAT",
      timeCol = "time",
      timeUnits = "s"
    ) -> traj_data
    
    print(traj_data)
    
    traj_data %>% 
      ggplot(aes(x = x, y = y)) + 
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      theme_minimal()
  })
  
  # NAV 2 Server Logic
  
  callModule(
    valueBoxModule,
    "nav2_last_date",
    value = "2022-12-24"
  )
  
  callModule(
    valueBoxModule,
    "nav2_total_ships",
    value = maritime_sec %>% 
      pull(ship_id) %>%
      unique() %>% 
      length()
  )
  
  callModule(
    valueBoxModule,
    "nav2_days_to_ffd",
    value = paste0(15, " days ahead")
  )
  
  callModule(
    valueBoxModule,
    "nav2_tanggal_pasang",
    value = "2023-01-24"
  )
  
  output$nav2_aismap <- renderLeaflet({
    filtered_data() %>% 
      group_by(MMSI) %>% 
      filter(BaseDateTime == max(BaseDateTime)) %>% 
      ungroup() %>%
      leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(
        lng = ~LON, 
        lat = ~LAT, 
        clusterOptions = markerClusterOptions(),
        popup = ~cntnt
      )
  })
  
  output$nav2_total_alert_output <- renderEcharts4r({
    maritime_sec %>%
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      group_by(jenis_alert) %>% 
      count(name = "Total") %>% 
      ungroup() %>% 
      arrange(Total) %>% 
      e_charts(jenis_alert) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      e_tooltip("item")
  })
  
  output$nav2_total_ship_types <- renderEcharts4r({
    maritime_sec %>%
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      group_by(ais_type_summary) %>% 
      count(name = "Total") %>% 
      ungroup() %>% 
      arrange(Total) %>% 
      e_charts(ais_type_summary) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      e_tooltip("item")
  })
  
  output$nav2_hourly_activity <- renderEcharts4r({
    maritime_sec %>% 
      filter(
        timestamp >= input$nav2_datetimepicker_input_1,
        timestamp <= input$nav2_datetimepicker_input_2
      ) %>% 
      mutate(
        hourly = lubridate::hour(timestamp), 
        day = lubridate::day(timestamp),
        month = lubridate::month(timestamp),
        year = lubridate::year(timestamp)
      ) %>% 
      select(ship_id, timestamp, day, month, year, hourly) %>% 
      arrange(timestamp) %>% 
      mutate(hourly_dt = lubridate::make_datetime(year = year, month = month, day = day, hour = hourly)) %>% 
      group_by(hourly_dt) %>% 
      summarise(
        activity_count = n()
      ) %>% 
      ungroup() %>% 
      e_charts(hourly_dt) %>% 
      e_line(activity_count) %>% 
      e_tooltip("axis")
  })
  
}

shinyApp(ui, server)


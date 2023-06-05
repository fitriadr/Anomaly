library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(secr)
library(sp)
library(leaflet.extras2)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(shinythemes)

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


ui <- dashboardPage(
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
server <- function(input, output, session){
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
shinyApp(ui, server)

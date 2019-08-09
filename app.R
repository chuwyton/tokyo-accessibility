####################
# Load libraries and data
####################
# Libraries
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

# Data
hotels_aggr_all = readRDS("data/hotels_aggr_all.rds") %>% 
  map(~.x %>% st_transform(4326))

bin_duration_diff = c(-Inf,    -30,    -15,    -10,   -5,   -1,     0, Inf)
cvl_duration_diff = c(     -45,    -20,    -12,    -7,   -3,   -0.5,  1)
pal_duration_diff = colorBin(palette = "inferno", bins = bin_duration_diff)
lab_duration_diff = c(">30 minutes", ">15", ">10", ">5", ">1 minute", "<1 minute", "Overestimation")

####################
# UI
####################
ui = fluidPage(
  # App title
  titlePanel("Hotel Reviews and Accessibility"),
  
  # Sidebar Layout
  sidebarLayout(
    position = "right",
    
    # Sidebar Panel (Filter)
    sidebarPanel = sidebarPanel(
      # Filter items
      # Number of shortest destinations to take average from
      sliderInput("slider_shortDest",
                  label = "Shortest destinations<br>to take average from",
                  min = 1, max = 17, value = 2)
    ),
  
    # Main Panel (Map)
    mainPanel(
      # Map goes here
      leafletOutput("mapDuration")
    )
  )
)

####################
# Server
####################
server = function(input, output) {
  output$mapDuration = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLines) %>% 
      addPolygons(data = hotels_aggr_all[[input$slider_shortDest]], 
                  stroke = F,
                  fillColor = ~pal_duration_diff(diff.mins),
                  fillOpacity = 0.8) %>% 
      addLegend(position = "topright",
                colors = pal_duration_diff(cvl_duration_diff),
                labels = lab_duration_diff,
                title = "Underestimation <br> in timings for SLDA")
  })
}

####################
# Run App
####################
shinyApp(ui = ui, server = server)


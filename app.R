####################
# Load libraries and data
####################
# Libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(sf)
library(spgwr)
library(leaflet)

# Data
print("Getting data")
hotels_aggr_all = read_rds(here::here("data/hotels_aggr_all.rds")) %>% 
  map(~.x %>% na.omit() %>% st_transform(4326))
hotels_aggr_all_sp = map(hotels_aggr_all, ~na.omit(.) %>% st_transform(6677) %>% as_Spatial())
hotels_aggr_gwr = hotels_aggr_all_sp %>% 
  map(~gwr(reviews.average ~
             price.lead.average * star.average +
             duration.avg +
             fare.avg +
             transfers.avg,
           data=.x,
           bandwidth=1500,
           hatmatrix=T,
           se.fit=T))
hotels_aggr_gwr_edf = map(hotels_aggr_gwr, c("results", "edf"))

get_pval = function(coef, se, edf) {
  # 2-tailed Student t-test
  t = coef/se
  p = 2*pt(-abs(t), edf)
  return(p)
}

hotels_aggr_gwr_est = map2(hotels_aggr_gwr, hotels_aggr_gwr_edf, ~.x$SDF %>%
                             st_as_sf(crs = 6677) %>% 
                             st_transform(4326) %>% 
  mutate(price.lead.pval = get_pval(price.lead.average, price.lead.average_se, .y),
         star.pval = get_pval(star.average, star.average_se, .y),
         price.lead.average.star.pval = get_pval(price.lead.average.star.average, price.lead.average.star.average_se, .y),
         duration.pval = get_pval(duration.avg, duration.avg_se, .y),
         fare.pval = get_pval(fare.avg, fare.avg_se, .y),
         transfers.pval = get_pval(transfers.avg, transfers.avg_se, .y)) %>% 
  mutate_at(vars(contains("pval")),
            list(a = ~case_when(.<0.001 ~ 0,
                                .<0.01  ~ 0.2,
                                .<0.05  ~ 0.4,
                                .<0.1   ~ 0.6,
                                TRUE    ~ 0.8))) %>% 
    rename_at(vars(contains("_a")), ~str_replace_all(., "_a", ".a")))

# lines_sf = read_rds("data/lines_sf.rds") %>% st_transform(4326)
destinations_sf = read_rds("data/destinations_sf.rds") %>% st_transform(4326)
stations_sf = read_rds("data/stations_sf.rds") %>% st_transform(4326)

# Palettes, Legends and Colors
bin_duration = c(0, 5, 10, 15, 20, 30, 45, 60, Inf)
cvl_duration = c(2.5, 7.5, 12.5, 17.5, 25, 37.5, 52.5, 70)
pal_duration = colorBin(palett = "YlOrRd", bins = bin_duration)
lab_duration = c("<5 minutes", "<10", "<15", "<20", "<30", "<45", "<60", ">60 minutes")

bin_duration_diff = c(-Inf,    -30,    -15,    -10,   -5,   -1,     0, Inf)
cvl_duration_diff = c(     -45,    -20,    -12,    -7,   -3,   -0.5,  1)
pal_duration_diff = colorBin(palette = "inferno", bins = bin_duration_diff)
lab_duration_diff = c(">30 minutes", ">15", ">10", ">5", ">1 minute", "<1 minute", "Overestimation")

bin_reviews = seq(3, 5, 0.5)
cvl_reviews = seq(3.25, 4.75, 0.5)
pal_reviews = colorBin(palette = "Blues", bins = bin_reviews)
lab_reviews = map2_chr(bin_reviews[1:4], bin_reviews[2:5], ~str_glue("{.x} \u2013 {.y}"))

# Bounding Box
bbox = hotels_aggr_all[[1]] %>% st_bbox()

####################
# UI
####################
ui = fillPage(
  useShinyjs(),
  title = "Tokyo HotelVis | Visualising Accessibilities of Hotels in Tokyo",
  # Navbar
  # It's hacky but if it works, hey
  navbarPage(
    fluid = F,
    position = "fixed-top",
    title = "Visualizations",
    id = "selectedTab",
    tabPanel(
      title = "Review Scores",
      value = "review",
      div()
    ),
    tabPanel(
      title = "Average trip times",
      value = "duration",
      div()
    ),
    tabPanel(
      title = "Geographically Weighted Regression",
      value = "gwr",
      div()
    )
  ),
  
  # Sidebar Layout
  fillRow(
    flex = c(1, NA),
  
    # Main Panel (Map)
    fillCol(
      leafletOutput(outputId = "map", height = "100%")
    ),
    
    # Sidebar Panel (Filter)
    fillCol(
      flex = 1,
      width = "200px",
      div(
        h4("Map-specific Options"),
        # Filter items
        # Number of shortest destinations to take average from
        sliderInput("slider_shortDest",
                    label = "Shortest destinations to take average from",
                    min = 1, max = 17, value = 2),
        hr(),
        radioButtons("duration_type", 
                     label = "Investigate Accessibility Metric", 
                     choices = c("SLDA", "Compare", "UBA")),
        hr()
      ),
      div(
        h4("Layers"),
        fillRow(
          height = "30px",
          checkboxInput("lines", "Railway", value = T, width = "100%"),
          checkboxInput("stations", "Stations", value = F, width = "100%")
        ),
        fillRow(
          height = "30px",
          checkboxInput("grid", "Grid", value = F, width = "100%"),
          checkboxInput("destinations", "Destinations", value = T, width = "100%")
        )
      )
    )
  ),
  
  # Styling
  tags$style(type="text/css", "body {padding-top: 50px}")
)

####################
# Server
####################
server = function(input, output) {
  output$map = renderLeaflet({
    # Default (init) is the review map
    leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite, options = tileOptions(opacity = 0.5)) %>% 
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
      addPolygons(group = "Reviews",
                  data = hotels_aggr_all[[1]],
                  stroke = F,
                  fillColor = ~pal_reviews(reviews.average),
                  fillOpacity = 0.8) %>% 
      addPolylines(group = "lines",
                   data = lines_sf,
                   stroke = 2,
                   color = ~col) %>% 
      addCircleMarkers(group = "destinations",
                       data = destinations_sf,
                       stroke = F,
                       fillColor = "Red",
                       fillOpacity = 1,
                       radius = 3) %>% 
      addCircleMarkers(group = "stations",
                       data = stations_sf,
                       stroke = F,
                       fillColor = "Black",
                       fillOpacity = 1,
                       radius = 3)
  })
  
  ### Reactors
  
  
  ### Observers
  # Change in selected tab
  observe({
    if(input$selectedTab == "review"){
      leafletProxy("map", data = hotels_aggr_all[[1]]) %>% 
        hideGroup(c("SLDA", "Differences", "UBA", "GWR")) %>% 
        showGroup("Reviews") %>% 
        clearControls() %>%
        addLegend(position = "topright", 
                  title = "Average review scores",
                  colors = pal_reviews(cvl_reviews),
                  labels = lab_reviews,
                  opacity = 1)
    }
    if(input$selectedTab == "duration"){
      leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
        hideGroup(c("Reviews", "GWR")) %>% 
        clearControls()
      
      if(isolate({input$duration_type}) == "SLDA"){
        leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
          showGroup("SLDA") %>% 
          addLegend(group = "SLDA",
                    position = "topright",
                    title = "SLDA estimations",
                    colors = pal_duration(cvl_duration),
                    labels = lab_duration,
                    opacity = 1)
      }
      if(isolate({input$duration_type}) == "Compare"){
        leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
          showGroup("Differences") %>% 
          addLegend(group = "Differences",
                    position = "topright",
                    title = "Underestimation <br> in timings for SLDA",
                    colors = pal_duration_diff(cvl_duration_diff),
                    labels = lab_duration_diff,
                    opacity = 1)
      }
      if(isolate({input$duration_type}) == "UBA"){
        leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
          showGroup("UBA") %>% 
          addLegend(group = "UBA",
                    position = "topright",
                    title = "UBA estimations",
                    colors = pal_duration(cvl_duration),
                    labels = lab_duration,
                    opacity = 1)
      }
    }
    if(input$selectedTab == "gwr"){
      # Data
      data = isolate(hotels_aggr_gwr_est[[input$slider_shortDest]])
      # Palette
      rng = range(data$duration.avg)
      sig = (((rng[1] - rng[2]) %>% abs() %>% log(10)) * -1) %>% ceiling()
      if(rng[1] * rng[2] < 0) {
        # Diverge around 0
        m_rng = range(data$duration.avg) %>% abs() %>% max()
        rng_gwr = c(-m_rng, m_rng)
        pal_gwr = colorBin(palette = "PiYG", domain = rng_gwr, bins = 4)
      } else {
        # Sequential
        pal_gwr = colorBin(palette = "YlGn", domain = data$duration.avg, bins = 4)
      }
      
      leafletProxy("map", data = data) %>% 
        hideGroup(c("Reviews", "SLDA", "Differences", "UBA")) %>% 
        showGroup("GWR") %>% 
        clearControls() %>% 
        addLegend(position = "topright",
                  title = "Coefficient estimate: <br> trip duration",
                  pal = pal_gwr,
                  values = ~duration.avg,
                  labFormat = labelFormat(digits = sig))
    }
  })
  
  # Change in shortest destinations slider value
  observe({
    if(input$slider_shortDest){
      ### Duration
      leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
        clearGroup(c("SLDA", "Differences", "UBA")) %>% 
        addPolygons(group = "SLDA",
                    stroke = F,
                    fillColor = ~pal_duration(time.avg/60),
                    fillOpacity = 0.8) %>% 
        addPolygons(group = "Differences",
                    stroke = F,
                    fillColor = ~pal_duration_diff(diff.mins),
                    fillOpacity = 0.8) %>% 
        addPolygons(group = "UBA",
                    stroke = F,
                    fillColor = ~pal_duration(duration.avg/60),
                    fillOpacity = 0.8) %>% 
        hideGroup(c("SLDA", "Differences", "UBA"))
      
      # If duration is the selected tab, show group and redraw legend
      if(isolate({input$selectedTab}) == "duration"){
        if(isolate({input$duration_type}) == "SLDA"){
          leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
            showGroup("SLDA") %>% 
            clearControls() %>% 
            addLegend(group = "SLDA",
                      position = "topright",
                      title = "SLDA estimations",
                      colors = pal_duration(cvl_duration),
                      labels = lab_duration,
                      opacity = 1)
        }
        if(isolate({input$duration_type}) == "Compare"){
          leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
            showGroup("Differences") %>% 
            clearControls() %>% 
            addLegend(group = "Differences",
                      position = "topright",
                      title = "Underestimation <br> in timings for SLDA",
                      colors = pal_duration_diff(cvl_duration_diff),
                      labels = lab_duration_diff,
                      opacity = 1)
        }
        if(isolate({input$duration_type}) == "UBA"){
          leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
            showGroup("UBA") %>% 
            clearControls() %>% 
            addLegend(group = "UBA",
                      position = "topright",
                      title = "UBA estimations",
                      colors = pal_duration(cvl_duration),
                      labels = lab_duration,
                      opacity = 1)
        }
      }
      
      ### GWR
      # Data
      data = hotels_aggr_gwr_est[[input$slider_shortDest]]
      # Palette
      rng = range(data$duration.avg)
      sig = (((rng[1] - rng[2]) %>% abs() %>% log(10)) * -1) %>% ceiling()
      if(rng[1] * rng[2] < 0) {
        # Diverge around 0
        m_rng = range(data$duration.avg) %>% abs() %>% max()
        rng_gwr = c(-m_rng, m_rng)
        pal_gwr = colorBin(palette = "PiYG", domain = rng_gwr, bins = 4)
      } else {
        # Sequential
        pal_gwr = colorBin(palette = "YlGn", domain = data$duration.avg, bins = 4)
      }
      
      leafletProxy("map", data = data) %>% 
        clearGroup("GWR") %>%  
        addPolygons(group = "GWR",
                    stroke = F,
                    fillColor = ~pal_gwr(duration.avg),
                    fillOpacity = 0.8)
      
      # If gwr is the selected tab, draw legend
      if(isolate({input$selectedTab}) == "gwr"){
        leafletProxy("map", data = data) %>% 
          clearControls() %>% 
          addLegend(position = "topright",
                    title = "Coefficient estimate: <br> trip duration",
                    pal = pal_gwr,
                    values = ~duration.avg,
                    labFormat = labelFormat(digits = sig))
      }else{
        leafletProxy("map") %>% hideGroup("GWR")
      }
    }
  })
  
  # Change in accessibility metric
  observe({
    input$duration_type
    if(isolate({input$selectedTab}) == "duration"){
      data = hotels_aggr_all[[isolate({input$slider_shortDest})]]
      if(input$duration_type == "SLDA"){
        leafletProxy("map", data = data) %>%
          hideGroup(c("Differences", "UBA")) %>% 
          showGroup("SLDA") %>% 
          clearControls() %>% 
          addLegend(group = "SLDA",
                    position = "topright",
                    title = "SLDA estimations",
                    colors = pal_duration(cvl_duration),
                    labels = lab_duration,
                    opacity = 1)
      }
      if(input$duration_type == "Compare"){
        leafletProxy("map", data = data) %>% 
          hideGroup(c("SLDA", "UBA")) %>% 
          showGroup("Differences") %>% 
          clearControls() %>% 
          addLegend(group = "Differences",
                    position = "topright",
                    title = "Underestimation <br> in timings for SLDA",
                    colors = pal_duration_diff(cvl_duration_diff),
                    labels = lab_duration_diff,
                    opacity = 1)
      }
      if(input$duration_type == "UBA"){
        leafletProxy("map", data = data) %>% 
          hideGroup(c("SLDA", "Differences")) %>% 
          showGroup("UBA") %>% 
          clearControls() %>% 
          addLegend(group = "UBA",
                    position = "topright",
                    title = "UBA estimations",
                    colors = pal_duration(cvl_duration),
                    labels = lab_duration,
                    opacity = 1)
      }
    }
  })
  
  # Toggle lines
  observe({
    if(input$lines){
      leafletProxy("map") %>% 
        showGroup("lines")
    } else {
      leafletProxy("map") %>% 
        hideGroup("lines")
    }
  })
  
  # Toggle Destinations
  observe({
    if(input$destinations){
      leafletProxy("map") %>% 
        showGroup("destinations")
    }else{
      leafletProxy("map") %>% 
        hideGroup("destinations")
    }
  })
  
  # Toggle Stations
  # observe({
  #   if(input$stations){
  #     leafletProxy("map") %>% 
  #       showGroup("stations")
  #   }else{
  #     leafletProxy("map") %>% 
  #       hideGroup("stations")
  #   }
  # })
}

####################
# Run App
####################
shinyApp(ui = ui, server = server)


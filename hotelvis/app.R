####################
# Load libraries and data
####################
# Libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(sf)
library(spgwr)
library(leaflet)
# library(lazyeval)

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

lines_sf = read_rds("data/lines_sf.rds") %>% st_transform(4326)
destinations_sf = read_rds("data/destinations_sf.rds") %>% st_transform(4326)
stations_sf = read_rds("data/stations_sf.rds") %>% st_transform(4326)
# labels_sf = read_rds("data/labels_sf.rds") %>% st_transform(4326)

# Palettes, Legends and Colors
bin_duration = c(0, 5, 10, 15, 20, 30, 45, 60, Inf)
cvl_duration = c(2.5, 7.5, 12.5, 17.5, 25, 37.5, 52.5, 70)
pal_duration = colorBin(palett = "YlOrRd", bins = bin_duration)
lab_duration = c("<5 minutes", "<10", "<15", "<20", "<30", "<45", "<60", ">60 minutes")

bin_duration_diff = c(-Inf,    -30,    -15,    -10,   -5,   -1,     0, Inf)
cvl_duration_diff = c(     -45,    -20,    -12,    -7,   -3,   -0.5,  1)
pal_duration_diff = colorBin(palette = "inferno", bins = bin_duration_diff)
lab_duration_diff = c(">30 minutes", ">15", ">10", ">5", ">1 minute", "<1 minute", "Overestimation")

# bin_reviews = seq(3, 5, 0.5)
# cvl_reviews = seq(3.25, 4.75, 0.5)
# pal_reviews = colorBin(palette = "Blues", bins = bin_reviews)
# lab_reviews = map2_chr(bin_reviews[1:4], bin_reviews[2:5], ~str_glue("{.x} \u2013 {.y}"))

# Bounding Box
bbox = hotels_aggr_all[[1]] %>% st_bbox()

# Text
map_details = c("493 hotels from Expedia.com were used in this analysis, binned into 177 grids as shown on the map. In this map, you can look at the different average hotel attributes of each, such as its review score or star rating, by selecting the factor to view above.",
                "This map shows the average time taken from each grid's centre to the 17 different destinations in the analysis. There are different ways to calculate it; SLDA (straight-line distance accessibility) calculates the geographic distance, whereas UBA (utility-based accessibility) calculates the time taken by travelling along the railway network. You can even compare between the two metrics when selecting the type of accessibility, and also choose how many closest destinations to include in the calculation.",
                "This map shows the results of applying a Geographically Weighted Regression to the dataset, by including utility-based accessibility as one of the factors, on top of the factors in the hotel attributes. Each factor attempts to explain the local variations in review ratings; the map shows how significant the factor is in different parts of the map; greyed-out areas correspond to insignificant values. The colour shows how the factor correlates to the review price. For example, for a value of -0.0004 for the factor of trip duration, it means that each second's increase in average trip duration decreases the review rating by 0.0004, on average.")

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
    title = "HotelVis",
    id = "selectedTab",
    tabPanel(
      title = "Hotel Attributes",
      value = "overall",
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
      flex = c(4, 2, 3),
      width = "400px",
      height = "100%",
      class = "sidebar",
      wellPanel(
        h4("Map-specific Options"),
        # Filter items
        
        # Select Factor (Overall)
        selectInput("select_factor_overall",
                    label = "Show Factor",
                    choices = c("Review Scores",
                                "Room Rates",
                                "Star Rating")),
        
        # Select Accessibility Metric
        selectInput("select_duration_type", 
                     label = "Investigate Accessibility Metric", 
                     choices = c("SLDA", "Compare", "UBA")),
        
        # Select Factor (GWR)
        selectInput("select_factor_gwr",
                    label = "Show Factor",
                    choices = c("Room Rates",
                                "Star Rating",
                                "Room Rates * Star Rating",
                                "Trip Duration",
                                "Trip Fare",
                                "Trip Transfers")),
        
        # Number of shortest destinations to take average from
        sliderInput("slider_shortDest",
                    label = "Shortest destinations to take average from",
                    min = 1, max = 17, value = 2),
        
        # Greying out non-significant grids (GWR)
        sliderInput("slider_pval",
                    label = "Grey out p-values above",
                    min = 0, max = 50, value = 5)
      ),
      wellPanel(
        h4("Layers"),
        checkboxInput("lines", "Railway", value = T, width = "100%"),
        checkboxInput("stations", "Stations", value = F, width = "100%"),
        checkboxInput("grid", "Grid", value = F, width = "100%"),
        checkboxInput("destinations", "Destinations", value = T, width = "100%")
      ),
      wellPanel(
        h4("About this map"),
        textOutput("map_detail")
      )
    )
  ),
  
  # Styling
  tags$style(type="text/css",
             "body {padding-top: 50px}",
             ".sidebar .flexfill-item .flexfill-item-inner {padding: 5px}",
             ".well {height:100%; padding:8px; overflow:auto;}")
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
      addMapPane("grid_polys", zIndex = 300) %>% 
      addPolygons(group = "grid",
                  data = hotels_aggr_all[[1]],
                  stroke = T,
                  weight = 2,
                  color = "gray",
                  opacity = 1,
                  fillOpacity = 0,
                  options = pathOptions(pane = "grid_polys")) %>%
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
                       radius = 3) %>% 
      addLabelOnlyMarkers(group = "stations",
                          data = labels_sf,
                          label = ~name_en,
                          labelOptions = labelOptions(noHide = T, direction = "top", textOnly = T)) 
    
  })
  
  output$map_detail = renderText({
    map_details[match(input$selectedTab, c("overall", "duration", "gwr"))]
  })
  
  ### Reactors
  
  
  ### Observers
  # Change in selected tab
  observe({
    if(input$selectedTab == "overall"){
      variab = input$select_factor_overall
      var = case_when(variab == "Review Scores" ~ "reviews.average",
                      variab == "Room Rates" ~ "price.lead.average",
                      variab == "Star Rating" ~ "star.average",
                      variab == "Room Rates * Star Rating" ~ "price.lead.average.star.average",
                      variab == "Trip Duration" ~ "duration.avg",
                      variab == "Trip Fare" ~ "fare.avg",
                      variab == "Trip Transfers" ~ "transfers.avg")
      col = hotels_aggr_all[[1]][[var]]
      pal = colorBin(palette = "Blues", 
                     domain = range(col), 
                     bins = 6,
                     pretty = T)
      
      leafletProxy("map", data = hotels_aggr_all[[1]]) %>% 
        hideGroup(c("SLDA", "Differences", "UBA", "GWR", "GWR_pval")) %>% 
        showGroup("Overall") %>% 
        clearGroup("Overall") %>% 
        clearControls() %>%
        addPolygons(group = "Overall",
                    data = hotels_aggr_all[[1]],
                    stroke = F,
                    fillColor = pal(col),
                    fillOpacity = 0.8,
                    label = format(col, digits = 3),
                    labelOptions = labelOptions()) %>%
        addLegend(position = "topright", 
                  title = str_glue("Average {variab}"),
                  pal = pal,
                  values = col,
                  opacity = 1)
    }
    if(input$selectedTab == "duration"){
      leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
        hideGroup(c("Overall", "GWR", "GWR_pval")) %>% 
        clearControls()
      
      if(isolate({input$select_duration_type}) == "SLDA"){
        leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
          showGroup("SLDA") %>% 
          addLegend(group = "SLDA",
                    position = "topright",
                    title = "SLDA estimations",
                    colors = pal_duration(cvl_duration),
                    labels = lab_duration,
                    opacity = 1)
      }
      if(isolate({input$select_duration_type}) == "Compare"){
        leafletProxy("map", data = hotels_aggr_all[[input$slider_shortDest]]) %>% 
          showGroup("Differences") %>% 
          addLegend(group = "Differences",
                    position = "topright",
                    title = "Underestimation <br> in timings for SLDA",
                    colors = pal_duration_diff(cvl_duration_diff),
                    labels = lab_duration_diff,
                    opacity = 1)
      }
      if(isolate({input$select_duration_type}) == "UBA"){
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
      redrawGWR(isolate({input$slider_shortDest}), 
                isolate({input$select_factor_gwr}),
                isolate({input$selectedTab}),
                isolate({input$slider_pval}))
      leafletProxy("map") %>% 
        hideGroup(c("Overall", "SLDA", "Differences", "UBA")) %>% 
        showGroup(c("GWR", "GWR_pval"))
    }
    
    toggle("slider_shortDest", condition = input$selectedTab %in% c("duration", "gwr"))
    toggle("slider_pval", condition = input$selectedTab == "gwr")
    toggle("select_duration_type", condition = input$selectedTab == "duration")
    toggle("select_factor_gwr", condition = input$selectedTab == "gwr")
    toggle("select_factor_overall", condition = input$selectedTab == "overall")
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
                    fillOpacity = 0.8,
                    label = ~format(time.avg/60, digits = 3),
                    labelOptions = labelOptions()) %>% 
        addPolygons(group = "Differences",
                    stroke = F,
                    fillColor = ~pal_duration_diff(diff.mins),
                    fillOpacity = 0.8,
                    label = ~format(diff.mins, digits = 3),
                    labelOptions = labelOptions()) %>% 
        addPolygons(group = "UBA",
                    stroke = F,
                    fillColor = ~pal_duration(duration.avg/60),
                    fillOpacity = 0.8,
                    label = ~format(duration.avg/60, digits = 3),
                    labelOptions = labelOptions()) %>% 
        hideGroup(c("SLDA", "Differences", "UBA"))
      
      # If duration is the selected tab, show group and redraw legend
      if(isolate({input$selectedTab}) == "duration"){
        if(isolate({input$select_duration_type}) == "SLDA"){
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
        if(isolate({input$select_duration_type}) == "Compare"){
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
        if(isolate({input$select_duration_type}) == "UBA"){
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
      redrawGWR(isolate({input$slider_shortDest}), 
                isolate({input$select_factor_gwr}),
                isolate({input$selectedTab}),
                isolate({input$slider_pval}))
    }
  })
  
  # Change in accessibility metric
  observe({
    input$select_duration_type
    if(isolate({input$selectedTab}) == "duration"){
      data = hotels_aggr_all[[isolate({input$slider_shortDest})]]
      if(input$select_duration_type == "SLDA"){
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
      if(input$select_duration_type == "Compare"){
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
      if(input$select_duration_type == "UBA"){
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
  
  # Change in GWR factor
  observe({
    redrawGWR(isolate({input$slider_shortDest}), 
              input$select_factor_gwr,
              isolate({input$selectedTab}),
              isolate({input$slider_pval}))
  })
  
  # Change in GWR pval cutoff
  observe({
    input$slider_pval
    redrawGWR(isolate({input$slider_shortDest}),
              isolate({input$select_factor_gwr}),
              isolate({input$selectedTab}),
              input$slider_pval)
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
  observe({
    if(input$stations){
      leafletProxy("map") %>%
        showGroup("stations")
    }else{
      leafletProxy("map") %>%
        hideGroup("stations")
    }
  })
  
  # Toggle Grid
  observe({
    if(input$grid){
      leafletProxy("map") %>% 
        showGroup("grid")
    }else{
      leafletProxy("map") %>% 
        hideGroup("grid")
    }
  })
}

### Functions
redrawGWR = function(shortDest, variab, selectedTab, pval) {
  var = case_when(variab == "Room Rates" ~ "price.lead.average",
                  variab == "Star Rating" ~ "star.average",
                  variab == "Room Rates * Star Rating" ~ "price.lead.average.star.average",
                  variab == "Trip Duration" ~ "duration.avg",
                  variab == "Trip Fare" ~ "fare.avg",
                  variab == "Trip Transfers" ~ "transfers.avg")
  # Get Data
  data = hotels_aggr_gwr_est[[shortDest]]
  col = data[[var]]
  # Palette
  rng = range(col)
  sig = (((rng[1] - rng[2]) %>% abs() %>% log(10)) * -1) %>% ceiling() + 1
  if(rng[1] * rng[2] < 0) {
    # Diverge around 0
    m_rng = range(col) %>% abs() %>% max()
    rng_gwr = c(-m_rng, m_rng)
    pal_gwr = colorBin(palette = "PiYG", domain = rng_gwr, bins = 4)
  } else {
    # Sequential
    pal_gwr = colorBin(palette = "YlGn", domain = col, bins = 4)
  }
  
  leafletProxy("map") %>% 
    clearGroup("GWR") %>%  
    addPolygons(group = "GWR", data = data,
                stroke = F,
                fillColor = pal_gwr(col),
                fillOpacity = 0.8,
                label = format(col, digits = 3),
                labelOptions = labelOptions())
  
  # If gwr is the selected tab, draw legend
  if(selectedTab == "gwr"){
    leafletProxy("map") %>% 
      clearControls() %>% 
      addLegend(position = "topright",
                title = str_glue("Coefficient estimate: <br> {variab}"),
                pal = pal_gwr,
                values = col,
                labFormat = labelFormat(digits = sig))
  
    # pval grey out
    pval = pval/100
    var_pval = case_when(variab == "Room Rates" ~ "price.lead.pval",
                         variab == "Star Rating" ~ "star.pval",
                         variab == "Room Rates * Star Rating" ~ "price.lead.average.star.pval",
                         variab == "Trip Duration" ~ "duration.pval",
                         variab == "Trip Fare" ~ "fare.pval",
                         variab == "Trip Transfers" ~ "transfers.pval")
    if(pval > 0){
      print("Drawing pval mask...")
      data_pval = data %>% 
        filter((!!as.name(var_pval)) > pval)
      
      leafletProxy("map", data = data_pval) %>% 
        clearGroup("GWR_pval") %>% 
        addPolygons(group = "GWR_pval",
                    stroke = F,
                    fillColor = "dimgray",
                    fillOpacity = 0.5)
    } else {
      leafletProxy("map") %>% 
        clearGroup("GWR_pval")
    }
  }else{
    leafletProxy("map") %>% hideGroup("GWR") %>% clearGroup("GWR_pval")
  }
}

####################
# Run App
####################
shinyApp(ui = ui, server = server)


####################
# Load libraries and data
####################
# Libraries
library(shiny)
library(tidyverse)
library(sf)
library(spgwr)
library(leaflet)

# Data
hotels_aggr_all = readRDS("data/hotels_aggr_all.rds") %>% 
  map(~.x %>% st_transform(4326))
hotels_aggr_all_sp = map(hotels_aggr_all, as_Spatial)
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

hotels_aggr_gwr_est = map2(hotels_aggr_gwr, hotels_aggr_gwr_edf, .x$SDF %>% 
  st_as_sf(crs = 6677) %>% 
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

# Palettes, Legends and Colors
bin_duration_diff = c(-Inf,    -30,    -15,    -10,   -5,   -1,     0, Inf)
cvl_duration_diff = c(     -45,    -20,    -12,    -7,   -3,   -0.5,  1)
pal_duration_diff = colorBin(palette = "inferno", bins = bin_duration_diff)
lab_duration_diff = c(">30 minutes", ">15", ">10", ">5", ">1 minute", "<1 minute", "Overestimation")

bin_reviews = seq(3, 5, 0.5)
cvl_reviews = seq(3.25, 4.75, 0.5)
pal_reviews = colorBin(palette = "Blues", bins = bin_reviews)
lab_reviews = map2_chr(bin_reviews[1:4], bin_reviews[2:5], ~str_glue("{.x} \u2013 {.y}"))

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
                  label = "Shortest destinations <br> to take average from",
                  min = 1, max = 17, value = 2)
    ),
  
    # Main Panel (Map)
    mainPanel(
      # Navbar
      navbarPage(
        title = "Hotel Review and Accessibility Scores",
        id = "selectedTab",
        tabPanel(
          title = "Review Scores", 
          leafletOutput("mapReview")
        ),
        tabPanel(
          title = "Average trip times",
          # Duration map
          leafletOutput("mapDuration")
        ),
        tabPanel(
          title = "Geographically Weighted Regression",
          "To be implemented"
        )
      )
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
                title = "Underestimation <br> in timings for SLDA",
                colors = pal_duration_diff(cvl_duration_diff),
                labels = lab_duration_diff,
                opacity = 1)
  })
  
  output$mapReview = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLines) %>% 
      addPolygons(data = hotels_aggr_all[[1]],
                  stroke = F,
                  fillColor = ~pal_reviews(reviews.average),
                  fillOpacity = 0.8) %>% 
      addLegend(position = "topright",
                title = "Average review scores",
                colors = pal_reviews(cvl_reviews),
                labels = lab_reviews,
                opacity = 1)
  })
}

####################
# Run App
####################
shinyApp(ui = ui, server = server)


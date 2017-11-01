library(shiny)
library(leaflet)
library(jsonlite)
library(dplyr)
library(devtools)
library(curl)

# Check for required packages, install them if not installed.
pkgs <-c('shiny', 'leaflet', 'jsonlite', 'curl', 'lambda.r', 'dplyr')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p, repos='http://cran.us.r-project.org')}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

# Get data from citibike api.
citibike <- fromJSON("http://citibikenyc.com/stations/json")
stations <- citibike$stationBeanList %>% subset(testStation=="FALSE" & statusKey == 1)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "77%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("range", "Available Bikes", min(stations$availableBikes), max(stations$availableBikes),
                              value = range(stations$availableBikes), step = 1
                  ),
                  checkboxInput("showdocks", "Available Docks", TRUE)
    )
)

server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        if(!isTRUE(input$showdocks)){
            subset(stations, stations$availableBikes >= input$range[1] & stations$availableBikes <= input$range[2] & stations$availableDocks > 0)
        } else subset(stations, stations$availableBikes >= input$range[1] & stations$availableBikes <= input$range[2])
        
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(stations) %>% setView(lat = 40.7829, lng = -73.9654, zoom = 14) %>%
            addTiles()
    })
    
    
    observe({
        obs_dat <- filteredData()
        
        # Define a function to set marker color based on the number of available bikes.
        getColor <- function(obs_dat) {
            sapply(obs_dat$availableBikes, function(availableBikes) {
                if(availableBikes >= 5) {
                    "green"
                } else if(availableBikes <= 5 & availableBikes > 1) {
                    "orange"
                } else if(availableBikes <= 1){
                    "red"
                } })
        }
        
        # Define custom icons.
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(obs_dat)
        )
        
        popup_dat <- paste0("<strong>Station: </strong>", 
                            obs_dat$stationName, 
                            "<br><strong>Available Bikes: </strong>", 
                            obs_dat$availableBikes,
                            "<br><strong>Available Docks: </strong>", 
                            obs_dat$availableDocks,
                            "<br><strong>Last Communication: </strong>",
                            obs_dat$lastCommunicationTime)
        
        leafletProxy("map", data = obs_dat) %>%
            clearMarkers() %>%
            addAwesomeMarkers(popup = popup_dat, icon = icons)
    })
    
    
}

shinyApp(ui, server)

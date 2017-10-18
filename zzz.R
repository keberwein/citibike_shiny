library(httr)

getPayload <- function(network = 'citibikenyc'){
    url <- sprintf('http://api.citybik.es/%s.json', network)
    bike <- fromJSON(content(GET(url)))
    lapply(bike, function(station){within(station, { 
        fillColor = cut(
            as.numeric(bikes)/(as.numeric(bikes) + as.numeric(free)), 
            breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), 
            labels = brewer.pal(5, 'RdYlGn'),
            include.lowest = TRUE
        )
        # Add popup dat for leaflet while we're at it.
        popup = iconv(whisker::whisker.render(
            '<b></b><br>
            <b>Free Docks: </b>  <br>
            <b>Available Bikes:</b> 
            <p>Retreived At: </p>'
        ), from = 'latin1', to = 'UTF-8')
        latitude = as.numeric(lat)/10^6
        longitude = as.numeric(lng)/10^6
        lat <- lng <- NULL})
    })
}
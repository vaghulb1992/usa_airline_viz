library(leaflet)
library(maps)

server <- function(input, output){
  output$airport_map <- renderLeaflet({
    # including the source files for the datasets
    source("airports.R")
    source("cancellations.R")

    # subsetting the datasets based on flight and departure counts
    airports <- subset(airports, airports$NDepartures >= input$dep_count[1] & airports$NDepartures <= input$dep_count[2])
    cancellations <- subset(cancellations, cancellations$Number >= input$route_count[1] & cancellations$Number <= input$route_count[2])

    # ordering by cancellation ratio in order to be able to calculate the quantiles
    airports$propCancelled <- airports$Cancelled/airports$NDepartures
    airports <- airports[order(airports$propCancelled), ]
    cancellations <- cancellations[order(cancellations$propCancelled), ]

    # subsetting again based on the user-inputted quantiles))
    airports <- subset(airports, airports$propCancelled >= quantile(airports$propCancelled, input$airport_rank[1]) &
                         airports$propCancelled <= quantile(airports$propCancelled, input$airport_rank[2]))
    cancellations <- subset(cancellations, cancellations$propCancelled >= quantile(cancellations$propCancelled, input$route_rank[1]) &
                              cancellations$propCancelled <= quantile(cancellations$propCancelled, input$route_rank[2]))

    # preparing a separate dataframe to be used by the addPolylines function later as it works in a specific way
    spatial_info <- data.frame(lat = numeric(), long = numeric(), group = numeric())
    for (i in 1:nrow(cancellations))
    {
      spatial_info <- rbind(spatial_info, c(cancellations[i, ]$latOrigin, cancellations[i, ]$longOrigin, i))
      spatial_info <- rbind(spatial_info, c(cancellations[i, ]$latDest, cancellations[i, ]$longDest, i))
    }
    colnames(spatial_info) <- c("lat", "long", "group")

    # preparing the colour palette for the pseudo-heatmaps
    airport_pal <- colorNumeric(
      palette = c("green3", "blue", "firebrick4"),
      domain = airports$propCancelled
    )

    journey_pal <- colorNumeric(
      palette = c("green3", "blue", "firebrick4"),
      domain = cancellations$propCancelled
    )

    # preparing the map of 'Murica'
    map_states <- map("state", fill = TRUE, plot = FALSE)
    map_leaflet <- leaflet(data = map_states)
    map_leaflet <- addTiles(map_leaflet)
    map_leaflet <- addPolygons(map_leaflet, fillColor = topo.colors(52, alpha = 0.4), stroke = FALSE)

    # adding circles for the airports based on their quality
    map_leaflet <- addCircles(map_leaflet, lng = airports$longitude_deg, lat = airports$latitude_deg,
                              radius = airports$propCancelled*1000000,
                              popup = paste0("Ratio: ", round(airports$propCancelled, 3),
                                             "<br>No. of Departures: ", airports$NDepartures,
                                             "<br>No. of Cancellations: ", airports$Cancelled),
                              fillColor = airport_pal(airports$propCancelled),
                              label = paste0("IATA Code: ", airports$iata_code),
                              stroke = TRUE, fillOpacity = 0.2, color = airport_pal(airports$propCancelled))

    # adding polylines for the flight routes based on their quality
    for (i in unique(spatial_info$group))
    {
      map_leaflet <- addPolylines(map_leaflet, data = spatial_info[spatial_info$group == i, ],
                                  lat = ~lat, lng = ~long, label = paste0("Route: ", rownames(cancellations)[i]),
                                  fillColor = journey_pal(cancellations[i, ]$propCancelled),
                                  color = journey_pal(cancellations[i, ]$propCancelled),
                                  stroke = TRUE, fillOpacity = 0.2,
                                  popup = paste0("Ratio: ", round((cancellations[i, ]$propCancelled), 3),
                                                 "<br>No. of Flights: ", cancellations[i, ]$Number,
                                                 "<br>No. of Cancellations: ", cancellations[i, ]$Cancelled))
    }

    # adding legends to make life easier
    map_leaflet <- addLegend(map_leaflet, "topright", pal = airport_pal, values = airports$propCancelled,
                             title = "Cancellation Ratio per airport", opacity = 0.5)
    map_leaflet <- addLegend(map_leaflet, "bottomright", pal = journey_pal, values = cancellations$propCancelled,
                             title = "Cancellation Ratio per route", opacity = 0.5)

    # now we're done, time to display
    map_leaflet
  })
}

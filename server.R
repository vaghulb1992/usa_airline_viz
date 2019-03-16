library(leaflet)
library(maps)
library(DT)

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

    # preparing the datasets to display on the UI tables
    ##### Best Airports
    best_airports <- head(airports, 5)[, c('iata_code', 'propCancelled')]
    best_airports$propCancelled <- round(best_airports$propCancelled, 4)
    rownames(best_airports) <- best_airports$iata_code
    best_airports$iata_code <- NULL
    colnames(best_airports) <- c("Proportion of flights cancelled")
    output$best_airports <- DT::renderDataTable({DT::datatable(best_airports, style = 'bootstrap', autoHideNavigation = FALSE,
                                                               options = list(searching = FALSE, paging = FALSE,
                                                                              ordering = FALSE, autoWidth = TRUE))})

    ##### Worst Airports
    worst_airports <- tail(airports, 5)[, c('iata_code', 'propCancelled')]
    worst_airports <- worst_airports[order(worst_airports$propCancelled, decreasing = TRUE), ]
    worst_airports$propCancelled <- round(worst_airports$propCancelled, 4)
    rownames(worst_airports) <- worst_airports$iata_code
    worst_airports$iata_code <- NULL
    colnames(worst_airports) <- c("Proportion of flights cancelled")
    output$worst_airports <- DT::renderDataTable({DT::datatable(worst_airports, style = 'bootstrap', autoHideNavigation = FALSE,
                                                               options = list(searching = FALSE, paging = FALSE,
                                                                              ordering = FALSE, autoWidth = TRUE))})

    ##### Best Routes
    best_routes <- head(cancellations, 5)[, 'propCancelled', drop = FALSE]
    best_routes$propCancelled <- round(best_routes$propCancelled, 4)
    colnames(best_routes) <- c("Proportion of flights cancelled")
    output$best_routes <- DT::renderDataTable({DT::datatable(best_routes, style = 'bootstrap', autoHideNavigation = FALSE,
                                                             options = list(searching = FALSE, paging = FALSE,
                                                                            ordering = FALSE, autoWidth = TRUE))})

    ##### Worst Routes
    worst_routes <- tail(cancellations, 5)[, 'propCancelled', drop = FALSE]
    worst_routes$propCancelled <- worst_routes[order(worst_routes$propCancelled, decreasing = TRUE), ]
    worst_routes$propCancelled <- round(worst_routes$propCancelled, 4)
    colnames(worst_routes) <- c("Proportion of flights cancelled")
    output$worst_routes <- DT::renderDataTable({DT::datatable(worst_routes, style = 'bootstrap', autoHideNavigation = FALSE,
                                                             options = list(searching = FALSE, paging = FALSE,
                                                                            ordering = FALSE, autoWidth = TRUE))})

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
                             title = "Proportion of flights cancelled<br>per airport", opacity = 0.5)
    map_leaflet <- addLegend(map_leaflet, "bottomright", pal = journey_pal, values = cancellations$propCancelled,
                             title = "Proportion of flights cancelled<br>per route", opacity = 0.5)

    # now we're done, time to display
    map_leaflet
  })
}

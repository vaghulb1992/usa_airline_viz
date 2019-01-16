library(leaflet)
library(DT)

ui <- fluidPage(
  headerPanel("Which are the best/worst airports and flight routes? (based on cancellations)"),
  leafletOutput("airport_map", height = 700),
  sidebarPanel(
    sliderInput("dep_count", "No. of departures per airport", min=1, max=411964, value=c(3000, 300000)),
    sliderInput("route_count", "No. of flights per route", min=1, max=13787, value=c(5000, 10000))
  ),
  sidebarPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Best Airports", DT::dataTableOutput("best_airports")),
      tabPanel("Worst Airports", DT::dataTableOutput("worst_airports"))
    )
  ),
  sidebarPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Best Flight Routes", DT::dataTableOutput("best_routes")),
      tabPanel("Worst Flight Routes", DT::dataTableOutput("worst_routes"))
    )
  )
)

library(leaflet)

ui <- fluidPage(
  headerPanel("Which are the best/worst airports and flight routes?"),
  leafletOutput("airport_map", height = 700),
  sidebarPanel(
    sliderInput("airport_rank", "Enter desired airport rankings (quantile)", min=0, max=1, value=c(0.1, 0.6), step = 0.001),
    sliderInput("dep_count", "Enter departure count cutoff", min=1, max=411964, value=c(3000, 300000))
  ),
  sidebarPanel(
    sliderInput("route_rank", "Enter desired route rankings (quantile)", min=0, max=1, value=c(0.1, 0.6), step = 0.001),
    sliderInput("route_count", "Enter route count cutoff", min=1, max=13787, value=c(5000, 10000))
  )
)

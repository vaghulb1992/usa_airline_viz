library(DT)
library(shiny)
library(gplots)
library(leaflet)

ui <- fluidPage(
  titlePanel("USA Airports & Airlines Analysis"),
  tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/vaghulb1992/usa_airline_viz/master/www/airline_icon.ico")),
  headerPanel("The best and worst airports and flight routes in USA"),
  leafletOutput("airport_map", height = 450),
  br(),
  sidebarPanel(
    sliderInput("dep_count", "No. of departures per airport", min=1, max=411964, value=c(3000, 300000)),
    sliderInput("route_count", "No. of flights per route", min=3500, max=13787, value=c(5000, 10000))
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
  ),
  div("This dashboard was created by ", tags$a(href = "https://www.linkedin.com/in/vaghulb1992/", "Vaghul Aditya Balaji."),
      "Source: ", tags$a(href = "https://github.com/vaghulb1992/usa_airline_viz", "https://github.com/vaghulb1992/usa_airline_viz")),
  br()
)

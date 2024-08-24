library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dubai Properties Analysis"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select City:", choices = unique(data_df$City), selected = unique(data_df$City)[1]),
      selectInput("rent_bin", "Select Rent Bin:", choices = levels(data_df$Rent_bin), selected = levels(data_df$Rent_bin)[1]),
      dateRangeInput("date_range", "Select Date Range:", start = min(data_df$Posted_date), end = max(data_df$Posted_date)),
      numericInput("beds", "Number of Beds:", value = 1, min = 1, max = max(data_df$Beds)),
      numericInput("baths", "Number of Baths:", value = 1, min = 1, max = max(data_df$Baths)),
      actionButton("update", "Update")
    ),
    
    # Show a plot and a map
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Rent Distribution", plotlyOutput("rentDistPlot")),
        tabPanel("City Analysis", plotlyOutput("cityAnalysisPlot")),
        tabPanel("Trend Analysis", plotlyOutput("trendPlot")),
        tabPanel("Beds and Baths", plotlyOutput("bedsBathsPlot")),
        tabPanel("Property Size", plotlyOutput("propertySizePlot"))
      )
    )
  )
))

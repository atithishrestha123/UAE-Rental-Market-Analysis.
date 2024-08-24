# server.R

# Load necessary libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)  

# Ensure you have the lubridate package for date handling
# Load the dataset
data_df <- read.csv("cleaned_dubai_properties.csv")

# Convert relevant columns to appropriate types
data_df$Posted_date <- as.Date(data_df$Posted_date)
data_df$Rent_bin <- factor(data_df$Rent_bin, ordered = TRUE)

# Define server logic
shinyServer(function(input, output, session) {
  
  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    req(input$city, input$rent_bin, input$date_range, input$beds, input$baths)
    data_df %>%
      filter(
        City == input$city,
        Rent_bin == input$rent_bin,
        Posted_date >= input$date_range[1],
        Posted_date <= input$date_range[2],
        Beds == input$beds,
        Baths == input$baths
      )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    data <- filtered_data()
    leaflet(data = data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color = "magenta",
        radius = 5,
        opacity = 0.8,
        popup = ~paste(
          "<b>Address:</b>", Address, "<br>",
          "<b>Rent:</b>", Rent_price, "<br>",
          "<b>Beds:</b>", Beds, "<br>",
          "<b>Baths:</b>", Baths, "<br>",
          "<b>Area:</b>", Area_in_sqft, "sqft", "<br>",
          "<b>Posted Date:</b>", Posted_date
        )
      ) %>%
      setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 10) %>%
      addControl("<h4>Properties Locations</h4>", position = "topleft", className = "info legend")
  })
  
  # Render the rent distribution plot
  output$rentDistPlot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = Rent_bin, fill = Rent_bin)) +
      geom_bar(stat = "count", color = "black") +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
      labs(title = "Distribution of Rent Prices",
           x = "Annual Rent (AED)",
           y = "Frequency (Yearly)") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render the city analysis plot
  output$cityAnalysisPlot <- renderPlotly({
    city_distribution <- data_df %>%
      count(City) %>%
      rename(Number_Properties = n) %>%
      arrange(desc(Number_Properties))
    
    p <- ggplot(city_distribution, aes(x = City, y = Number_Properties, group = 1)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      geom_text(aes(label = Number_Properties), vjust = -0.5, color = "black", size = 3) +
      labs(title = "Number of Properties by City", x = "City", y = "Number of Properties") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Render the trend analysis plot
  output$trendPlot <- renderPlotly({
    monthly_trend <- data_df %>%
      mutate(YearMonth = format(Posted_date, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarise(Average_Rent = mean(Rent_price, na.rm = TRUE)) %>%
      arrange(YearMonth)
    
    p <- ggplot(monthly_trend, aes(x = YearMonth, y = Average_Rent, group = 1)) +
      geom_line(color = "darkblue") +
      geom_point(color = "red") +
      labs(title = "Monthly Trend of Average Rent Prices", x = "Year-Month", y = "Average Rent (AED)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Render the beds and baths plot
  output$bedsBathsPlot <- renderPlotly({
    bed_count <- data_df %>%
      group_by(Beds) %>%
      summarise(Count = n()) %>%
      arrange(Beds)
    
    p <- ggplot(bed_count, aes(x = Beds, y = Count, group = 1)) +
      geom_line(color = "green") +
      geom_point(color = "red") +
      geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3) +
      labs(title = "Number of Properties by Beds", x = "Beds", y = "Number of Properties") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render the property size plot
  output$propertySizePlot <- renderPlotly({
    p <- ggplot(data_df, aes(x = Area_in_sqft)) +
      geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
      labs(title = "Distribution of Property Sizes", x = "Area (sqft)", y = "Frequency") +
      theme_minimal()
    ggplotly(p)
  })
})

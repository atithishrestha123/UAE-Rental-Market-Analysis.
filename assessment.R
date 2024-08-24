# Exporting Necessary Libraries
library(ggplot2)
library(knitr)
library(dplyr)
library(reshape2)
library(scales)
library(leaflet)
library(plotly)
library(forcats)
library(lubridate)
library(forecast)
library(RColorBrewer)
library(tidyr)

# Loading the Dataset
data <- read.csv("dubai_properties.csv")
View(data)


# Number of rows and columns in the dataset.
dimensions <- dim(data)
number_of_rows <- dimensions[1]
number_of_columns <- dimensions[2]

cat("The total number of rows is ", number_of_rows, " .", "\n")
cat("The total number of columns is ", number_of_columns, " .", "\n")

head(data)
tail(data)


# Exploration and Summarization of the Data.
# Checking the structure of the dataset.
str(data)

# Checking the summary statistics of the dataset.
summary(data)


# DATA CLEANING
# Checking for missing values.
print(colSums(is.na(data)))

# Mean Imputation by Location.
# Location-based Imputation is applied because it leverage the contextual relationship between 'Location' and 'City' with 'Latitude' and 'Longitude'.
# This then provides the good balance of accuracy without being overly complex or computationally intensive.
# Location-Based Mean Imputation: This step attempts to fill missing Latitude and Longitude values based on the mean of their respective locations.
# Fallback to Overall Mean Imputation: This step fills any remaining missing values with the overall mean of the Latitude and Longitude columns, ensuring no missing values remain.

data <- data %>%
  group_by(Location) %>%
  mutate(
    Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm = TRUE), Latitude),
    Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm = TRUE), Longitude)
  ) %>%
  ungroup()

# Check for remaining missing values
remaining_na <- colSums(is.na(data))
remaining_na

# Perform overall mean imputation for remaining missing values
data$Latitude[is.na(data$Latitude)] <- mean(data$Latitude, na.rm = TRUE)
data$Longitude[is.na(data$Longitude)] <- mean(data$Longitude, na.rm = TRUE)

# Checking again for any remaining missing Values
print(colSums(is.na(data)))

# Converting Tibble to Data Frame for Traditional Printing.
data_df <- as.data.frame(data)


# Displaying the Cleaned Dataset.
print(head(data_df[1:5, ]))
print(tail(data_df[(nrow(data) - 4):nrow(data), ]))


# Checking for Duplicate Rows.
duplicate_rows <- data[duplicated(data), ]

# Print the duplicate rows, if any
if (nrow(duplicate_rows) > 0) {
  print("Duplicate Rows:")
  print(duplicate_rows)
} else {
  print("No duplicate rows found.")
}

data_df <- data %>% 
  rename(Rent_price = Rent)

# Saving The Cleaned Dataset To a New CSV File.
write.csv(data_df,"cleaned_dubai_properties.csv", row.names = FALSE)

View(data_df)

# Unique values for each column
unique_counts <- sapply(data_df, function(x) length(unique(x)))
print(unique_counts)

print(colSums(is.na(data_df)))


# EXPLORATORY DATA ANALYSIS 
# 1. Correlation Matrix
# Select only numeric columns for correlation analysis
numeric_data <- select_if(data_df, is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

correlation_df <- melt(correlation_matrix)

heatmap_plot <- ggplot(correlation_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Annotate heatmap cells with correlation values
heatmap_plot + 
  geom_text(aes(label = round(value, 2)), color = "white", size = 3)

print(heatmap_plot)

# Visualizing Data Distributions.
# ANALYZING TRENDS IN RENTAL MARKET.
data_df$Rent_price <- as.numeric(data_df$Rent_price)

# Checking summary to ensure there are no issues
summary(data_df$Rent_price)

data_df <- data_df %>%
  mutate(Rent_bin = cut(Rent_price, 
                        breaks = c(0, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, Inf),
                        labels = c("0-10K", "10K-50K", "50K-100K", "100K-500K", "500K-1M", "1M-5M", "5M-10M", "10M+"),
                        include.lowest = TRUE,
                        ordered_result = TRUE))

# Check the distribution of Rent_bin
table(data_df$Rent_bin)

# Plotting the bar
ggplot(data_df, aes(x = Rent_bin, fill = Rent_bin)) +
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +  
  labs(title = "Distribution of Rent Prices",
       x = "Annual Rent (AED)",
       y = "Frequency (Yearly)") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# 2. Distribution of Rent by City.
ggplot(data_df, aes(x = City, fill = Rent_bin)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Rent Distribution by City", x = "City", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Number of Properties by City.
city_distribution <- data_df %>%
  count(City) %>%
  rename(Number_Properties = n) %>%
  arrange(desc(Number_Properties))

ggplot(city_distribution, aes(x = City, y = Number_Properties, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_text(aes(label = Number_Properties), vjust = -0.5, color = "black", size = 3) + 
  labs(title = "Number of Properties by City", x = "City", y = "Number of Properties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# STUDYING THE GEOGRAPHICAL DISTRIBUTION OF RENTAL PROPERTIES.
# 4. Showing the Map Location of the Mentioned Rental Properties.
leaflet(data = data_df) %>%
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
  setView(lng = mean(data_df$Longitude), lat = mean(data_df$Latitude), zoom = 10) %>%
  addControl("<h4>Properties Locations</h4>", position = "topleft", className = "info legend")

# Calculate the number of rental properties in each city via the map.
city_counts <- data_df %>%
  group_by(City) %>%
  summarise(Count = n(), Longitude = mean(Longitude), Latitude = mean(Latitude)) %>%
  arrange(desc(Count))

# Define color palette based on the density of rental properties
pal <- colorNumeric(
  palette = "viridis",
  domain = city_counts$Count
)

# Plotting the map
leaflet(data = city_counts) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~pal(Count),
    radius = ~sqrt(Count) * 2, 
    opacity = 0.8,
    popup = ~paste(
      "<b>City:</b>", City, "<br>",
      "<b>Number of Properties:</b>", Count
    )
  ) %>%
  setView(lng = mean(city_counts$Longitude), lat = mean(city_counts$Latitude), zoom = 10) %>%
  addControl("<h4>Rental Properties by City</h4>", position = "topleft", className = "info legend") %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Count,
    title = "Number of Properties",
    labFormat = labelFormat(suffix = " Properties")
  )

# COMPARING RENTAL PRICES ACROSS DIFFERENT CITIES AND PROPERTY TYPES.
# 5. Impact of Cities and The Rental Category on Rental Costs.
ggplotly(ggplot(data_df, aes(x = City, y = Rent_bin, color = Rent_category, size = Baths)) +
  geom_point() +
  labs(title = "Impact of Cities and Rent Category on Rental Costs",
       x = "City",
       y = "Rent",
       color = "Rent Category",
       size = "Baths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# 6. Boxplot of Rent and Area in Square Ft.
ggplot(data_df, aes(x = Rent_bin, y = Area_in_sqft)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Rent and Area in Square Feet",
       x = "Rent (AED)",
       y = "Area (Square Feet)") +
  theme_minimal()

# 7. City Vs. Total Area In Square Feet.
city_total_area <- data_df %>%
  group_by(City) %>%
  summarize(Total_Area_in_sqft = sum(Area_in_sqft)) %>%
  arrange(desc(Total_Area_in_sqft)) 

# Create a bar plot for City vs Total Area in Square Feet
options(scipen = 10)
ggplot(city_total_area, aes(x = reorder(City, Total_Area_in_sqft), y = Total_Area_in_sqft, fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Area_in_sqft), vjust = -0.5, color = "black", size = 3) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "City vs Total Area in Square Feet.",
       x = "City",
       y = "Total Area (Square Feet)") +
  scale_fill_brewer(palette = "Set1")


# 8. City Vs. Average Rent.
city_area_rent <- data_df %>%
  group_by(City) %>%
  summarize(Average_Rent = mean(Rent_price, na.rm = TRUE)) %>%
  arrange(desc(Average_Rent))

# Create a bar plot for City vs Average Rent
ggplot(city_area_rent, aes(x = reorder(City, -Average_Rent), y = Average_Rent, fill = City)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "City vs Average Rent",
       x = "City",
       y = "Average Rent") +
  scale_fill_brewer(palette = "Set1")

# 9. Room Type Vs. Rent
data_df <- data_df %>%
  mutate(Type = fct_reorder(Type, Rent_price, .desc = TRUE)) %>%
  arrange(desc(Rent_price))

ggplot(data_df, aes(x = Type, y = Rent_price, group = 1, color = Type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(title = "\nRoom Type vs Rent Price\n",
       x = "\nRoom Type\n",
       y = "Rent Price\n") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# 10. Furnished Vs. Unfurnished
furnishing_counts <- data_df %>%
  count(Furnishing) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(furnishing_counts, aes(x = "", y = percentage, fill = Furnishing)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Furnished vs Unfurnished Rate",
       fill = "Furnishing") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) 

# -> Time Series Analysis
# Convert Posted_date to Date format
data$Posted_date <- as.Date(data$Posted_date, format="%m/%d/%Y")

# Check the converted Posted_date column
head(data$Posted_date)

# Now, let's proceed with creating the Season column
data_df <- data %>%
  mutate(Season = case_when(
    month(Posted_date) %in% c(12, 1, 2) ~ "Winter",
    month(Posted_date) %in% c(3, 4, 5) ~ "Spring",
    month(Posted_date) %in% c(6, 7, 8) ~ "Summer",
    month(Posted_date) %in% c(9, 10, 11) ~ "Fall"
  ))

print(head(data_df[c('Posted_date', 'Season')]))

# Aggregate rental prices by season and city
seasonal_rent <- data_df %>%
  group_by(Season, City) %>%
  summarise(Average_Rent = mean(Rent, na.rm = TRUE))

# Check the aggregated data
print(seasonal_rent)

# Convert Season to a factor with ordered levels
seasonal_rent$Season <- factor(seasonal_rent$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

str(seasonal_rent)

# 11. Average Rental Prices by Season and City.
ggplot(seasonal_rent, aes(x = Season, y = Average_Rent, color = City, group = City)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Rental Prices by Season and City",
       x = "Season",
       y = "Average Rent (AED)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 12. Demand for Rental Properties by Season
season_demand <- data_df %>%
  group_by(Season) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Arrange in descending order

# Convert Season to a factor with levels arranged in descending order of Count
season_demand$Season <- factor(season_demand$Season, levels = season_demand$Season)

# Plotting the demand for rental properties by season
ggplot(season_demand, aes(x = Season, y = Count, fill = Season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3) + 
  labs(title = "Demand for Rental Properties by Season",
       x = "Season",
       y = "Number of Properties") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 13.Rental Categories According to Season
rent_season <- data_df %>%
  mutate(
    Rent_category = case_when(
      Rent < 25000 ~ "Low",
      Rent < 70000 ~ "Medium",
      TRUE ~ "High"
    )
  ) %>%
  group_by(Season, Rent_category) %>%
  summarise(Count = n()) %>%
  ungroup()

# Convert Rent_category to factor
rent_season$Rent_category <- factor(rent_season$Rent_category)

# Convert Season to a factor with specified levels
rent_season$Season <- factor(rent_season$Season, levels = c("Spring", "Winter", "Fall", "Summer"))

# Print the rent_season to check the results
print(rent_season)

# Plotting Rental Categories According to Season
ggplot(rent_season, aes(x = Season, y = Count, fill = Rent_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Rental Categories According to Season",
       x = "Season",
       y = "Number of Properties") +
  scale_fill_viridis_d() +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 14. Furnishing Categories in Each Season
# Furnishing Categories in Each Season
furnishing_season <- data_df %>%
  group_by(Season, Furnishing) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Season, desc(Count))

# Convert Season to a factor with levels in descending order of appearance
furnishing_season$Season <- factor(furnishing_season$Season, levels = rev(unique(furnishing_season$Season)))

# Define a custom palette or use a predefined one
custom_palette <- brewer.pal(n = length(unique(furnishing_season$Furnishing)), name = "Set3")

# Plotting Furnishing Categories in Each Season with stacked bar chart
ggplot(furnishing_season, aes(x = Season, y = Count, fill = Furnishing)) +
  geom_bar(stat = "identity") +
  labs(title = "Furnishing Categories in Each Season",
       x = "Season",
       y = "Number of Properties") +
  scale_fill_manual(values = custom_palette) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 15. Most Demanded Rental Type According to Season.
most_demanded_type <- data_df %>%
  group_by(Season, Type) %>%
  summarise(Count = n()) %>%
  arrange(Season, desc(Count)) 

# Plotting Most Demanded Rental Types by Season
ggplot(most_demanded_type, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Season) +  # Separate plots for each season
  labs(title = "Most Demanded Rental Types by Season",
       x = "Type",
       y = "Number of Properties") +
  scale_fill_viridis_d() +  # Using viridis color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 16. Most Demanded City on the basis of season.
demand_by_city_season <- data_df %>%
  group_by(Season, City) %>%
  summarise(Count = n()) %>%
  arrange(Season, desc(Count))

most_demanded_city <- demand_by_city_season %>%
  group_by(Season) %>%
  slice_max(Count)

least_demanded_city <- demand_by_city_season %>%
  group_by(Season) %>%
  slice_min(Count)

ggplot(demand_by_city_season, aes(x = Season, y = Count, color = City, group = City)) +
  geom_line() +
  geom_point() +
  labs(title = "Demand for Cities Across Seasons",
       x = "Season",
       y = "Number of Properties",
       color = "City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 17. Most Demanded Type on the basis of Numebr of Properties.
most_demanded_type <- data_df %>%
  group_by(Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) 

ggplot(most_demanded_type, aes(x = fct_reorder(Type, Count), y = Count)) +
  geom_segment(aes(xend = fct_reorder(Type, Count), yend = 0), color = "grey") +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Most Demanded Type",
       x = "Type",
       y = "Number of Properties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Furnished Vs Unfurnished in each Rental Types.
# Sort the data by Type and Furnishing
# Summarise the counts and arrange them
furnished_counts <- data_df %>%
  group_by(Type, Furnishing) %>%
  summarise(Count = n()) %>%
  arrange(Type, Furnishing)

# Reorder the levels of Type in ascending order based on the sum of Count
furnished_counts <- furnished_counts %>%
  group_by(Type) %>%
  mutate(TotalCount = sum(Count)) %>%
  ungroup() %>%
  mutate(Type = fct_reorder(Type, TotalCount)) %>%
  select(-TotalCount)

# Grouped column chart with types on the x-axis and count on the y-axis
ggplot(furnished_counts, aes(x = Type, y = Count, fill = Furnishing)) +
  geom_col(position = "dodge") +
  labs(title = "Relationship between Furnished and Unfurnished Properties by Type",
       x = "Type",
       y = "Count",
       fill = "Furnishing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()


# ---------- STATISTICAL ANALYSIS -----------------
# HYPOTHESIS TESTING
# 1. ANOVA: To test if there are significant diffrences in rental prices across different cities.
anova_result <- aov(Rent ~ City, data = data_df)
summary(anova_result)
# The ANOVA results indicate that rental prices significantly differ across different cities, with a very small p-value (< 2e-16) showing strong statistical significance. 
# The high F value (489.8) further suggests a substantial effect of the city on rent prices, confirming that the city is a strong predictor of rent.

# 2. T-TEST: To compares means, such as rental prices between furnished and unfurnished properties.
t_test_result <- t.test(Rent ~ Furnishing, data = data_df)
print(t_test_result)
# The t-test results indicate that there is a statistically significant difference in mean rental prices between furnished and unfurnished properties, with a very small p-value (< 2.2e-16). 
# The mean rent for furnished properties is approximately 205,656.4 AED, while for unfurnished properties, it is around 132,334.3 AED. 
# The 95% confidence interval for the difference in means is between 67,471.91 and 79,172.32 AED, confirming that furnished properties generally have higher rental prices than unfurnished ones.

# 3. LINEAR REGRESSION
lm_model <- lm(Rent ~ City + Area_in_sqft + Beds + Baths + Furnishing, data = data_df)
summary(lm_model)
# The linear regression analysis shows how various factors affect rental prices. Key findings include:
# i. City Effects: Renting in Ajman, Al Ain, Fujairah, Ras Al Khaimah, Sharjah, and Umm Al Quwain generally lowers rent compared to Abu Dhabi (the reference city), with the effect most pronounced in Umm Al Quwain. Dubai increases rent significantly.
# ii. Area: Larger area in square feet increases rent.
# iii. Bedrooms: More bedrooms increase rent.
# iv. Bathrooms: More bathrooms decrease rent.
# v. Furnishing: Unfurnished properties are cheaper than furnished ones.
# Most predictors are highly significant (p < 0.001), except for Fujairah and Umm Al Quwain, which are less significant. 
# The model explains about 26.7% of the variability in rental prices (Adjusted R-squared = 0.2671).

# -------------- TIME SERIES ANALYSIS --------------------
# 1. TIME SERIES DECOMPOSITION
options(scipen = 10)
data_ts <- ts(data_df$Rent, start = c(2010, 1), frequency = 12) # Assuming monthly data
decomp <- decompose(data_ts)
plot(decomp)

# 2. FORECASTING
fit <- auto.arima(data_ts)
forecast_values <- forecast(fit, h=12) # Forecast next 12 months
plot(forecast_values)

# -----------CLUSTERING ANALYSIS ----------------
# K-MEANS CLUSTERING
set.seed(123)
numeric_features <- data_df %>% select(Rent, Area_in_sqft, Beds, Baths)
km <- kmeans(numeric_features, centers = 3)
data_df$Cluster <- as.factor(km$cluster)

ggplot(data_df, aes(x = Area_in_sqft, y = Rent, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering of Properties", x = "Area in Square Feet", y = "Rent Price (AED)")

# ---------------------- FEATURE ENGINEERING AND MACHINE LEARNING ---------------
# MACHINE LEARNING

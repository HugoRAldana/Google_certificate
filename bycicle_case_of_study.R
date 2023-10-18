#title: "Google_data_analysis"
#author: "Hugo Enrique Reyes Aldana"
#date: "2023-10-17"
#output: html_document

### Setting working directory and opening files ###
setwd("directory link")

## Loading packages ## 
#These are packages I load every time. #

pacman::p_load(pacman, plyr, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, tidyverse, lubridate, readr, ggpubr)

# additional packages can be loaded if necessary #

### reading the file ###
bike_data <- read.csv("Divvy_Trips_2020_Q1.csv")

head(bike_data)
glimse(bike_data)

### First tasks ### 
# 1. The data has be downloaded and stored adequately
# 2. The data is organized in a data frame containing the following columns: 
    # ride_id (identificaition of the ride)
    # rideble_type (status of the vehicle)
    # stated_at (starting time point)
    # ended_at (ending time point)
    # start_station_name (Name of the station where the ride started)
    # start_station_id (identification number of the station where the ride started)
    # end_station_id (identification number of the station where the ride ended)
    # start_lat (starting latitude)
    # start_lng (starting longitude)
    # end_lat (ending latitude)
    # end_lng (ending longitude)
    # member_casual (type of member)
# 3. the data is already ordered
# 4. Considering this as an mock dataset, we will assume that the data is reliable. Also, there is no additional 
  # information to determine the credibility

### Second block of tasks ###

# 1. Checking for errors in the data
# Check for missing values
print(paste("Total missing values:", sum(is.na(bike_data))))

# Check data types and structure
str(bike_data)

# Summary statistics to identify potential errors or outliers
summary(bike_data)

# 2. Choosing tools
    # Tidyverse would be the best option for this task, it is already loaded

# 3. Transform data
# Convert 'started_at' and 'ended_at' to datetime format
bike_data$started_at <- as.POSIXct(bike_data$started_at, format="%Y-%m-%d %H:%M:%S")
bike_data$ended_at <- as.POSIXct(bike_data$ended_at, format="%Y-%m-%d %H:%M:%S")

# Convert 'rideable_type' and 'member_casual' to factor
bike_data$rideable_type <- as.factor(bike_data$rideable_type)
bike_data$member_casual <- as.factor(bike_data$member_casual)

# Removing missing values 
bike_data <- bike_data %>% drop_na()

# Check the transformed data
str(bike_data)

# 4. The cleaning process has been documented in each step

### Third block- Analysis of the data ###

# creating a new column for day of the week
# Create a new column 'day_of_the_week' extracting the weekday from 'started_at'
bike_data <- bike_data %>% 
  mutate(day_of_week = weekdays(started_at))

# Verify the new column
head(bike_data[c("started_at", "day_of_week")])

# Calculate ride_length in minutes
bike_data <- bike_data %>% 
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")))

# Calculate mean ride_length
mean_ride_length <- mean(bike_data$ride_length, na.rm = TRUE)

# Calculate max ride_length
max_ride_length <- max(bike_data$ride_length, na.rm = TRUE)

# Calculate mode of day_of_the_week
mode_day_of_week <- as.character(sort(table(bike_data$day_of_the_week), decreasing = TRUE)[1])

# Print results
print(paste("Mean Ride Length:", round(mean_ride_length, 2), "minutes"))
print(paste("Max Ride Length:", round(max_ride_length, 2), "minutes"))
print(paste("Most Common Day of the Week for Rides:", names(mode_day_of_week)))

# Write a summary
cat("Summary:\n",
    "- The average ride length is approximately", round(mean_ride_length, 2), "minutes.\n",
    "- The longest ride in the dataset took", round(max_ride_length, 2), "minutes.\n",
    "- The most common day for rides is", names(mode_day_of_week), ".\n")

### Analyzing tendencies regarding the type of user ###


# Plot for rideable_type
p1 <- ggplot(bike_data, aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Rideable Type by User Type", 
       x = "Rideable Type", 
       y = "Count") +
  theme_minimal()

# Plot for day_of_week
p2 <- ggplot(bike_data, aes(x = day_of_the_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Rides per Day of the Week by User Type", 
       x = "Day of the Week", 
       y = "Count") +
  theme_minimal()

# Plot for top N start_station_name
top_stations <- head(names(sort(table(bike_data$start_station_name), decreasing = TRUE)), 10)
p3 <- ggplot(bike_data %>% filter(start_station_name %in% top_stations), 
             aes(x = start_station_name, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Rides from Top Start Stations by User Type", 
       x = "Start Station", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for ride_length
p4 <- ggplot(bike_data, aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot() +
  labs(title = "Ride Length by User Type", 
       x = "User Type", 
       y = "Ride Length (minutes)") +
  theme_minimal()

# Combine all plots into a single panel
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

### Summarizing the data ### 

# Summarize findings
summary_findings <- bike_data %>%
  group_by(member_casual) %>%
  summarise(
    AvgRideLength = mean(ride_length, na.rm = TRUE),
    MaxRideLength = max(ride_length, na.rm = TRUE),
    MostCommonRideableType = names(sort(table(rideable_type), decreasing = TRUE)[1]),
    MostCommonDayOfWeek = names(sort(table(day_of_the_week), decreasing = TRUE)[1]),
    MostCommonStartStation = names(sort(table(start_station_name), decreasing = TRUE)[1]),
    MostCommonEndStation = names(sort(table(end_station_name), decreasing = TRUE)[1])
  )

# Print the summary
print(summary_findings)

# Convert the findings to a more readable format
cat("Summary of Findings:\n\n")

cat("For Members:\n",
    "- Average Ride Length: ", round(summary_findings$AvgRideLength[1], 2), "minutes\n",
    "- Maximum Ride Length: ", round(summary_findings$MaxRideLength[1], 2), "minutes\n",
    "- Most Common Rideable Type: ", summary_findings$MostCommonRideableType[1], "\n",
    "- Most Common Day of Week for Rides: ", summary_findings$MostCommonDayOfWeek[1], "\n",
    "- Most Common Start Station: ", summary_findings$MostCommonStartStation[1], "\n",
    "- Most Common End Station: ", summary_findings$MostCommonEndStation[1], "\n\n",
    sep = ""
)

cat("For Casuals:\n",
    "- Average Ride Length: ", round(summary_findings$AvgRideLength[2], 2), "minutes\n",
    "- Maximum Ride Length: ", round(summary_findings$MaxRideLength[2], 2), "minutes\n",
    "- Most Common Rideable Type: ", summary_findings$MostCommonRideableType[2], "\n",
    "- Most Common Day of Week for Rides: ", summary_findings$MostCommonDayOfWeek[2], "\n",
    "- Most Common Start Station: ", summary_findings$MostCommonStartStation[2], "\n",
    "- Most Common End Station: ", summary_findings$MostCommonEndStation[2], "\n",
    sep = ""
)

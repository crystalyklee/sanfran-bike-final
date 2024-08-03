# BTC1855 - CODING IN R
# MIDTERM PROJECT
# CRYSTAL LEE

library(tidyverse)

stationdata <- read.csv('station.csv')
tripdata <-read.csv('trip.csv')
weatherdata <- read.csv('weather.csv')

# Clean stationdata before analysis
summary(stationdata)

# Check for duplicates
any(duplicated(stationdata))

# Clean tripdata before analysis
summary(tripdata)
# Check for duplicates

any(duplicated(tripdata))
# Separate date and time from datetime column

tripdata2 <- tripdata %>% 
  separate(start_date, c('start_date', 'start_time'), sep = " ") %>% 
  separate(end_date, c('end_date', 'end_time'), sep = " ") 
  
# Change dates into Date class
tripdata2$start_date <- mdy(tripdata2$start_date)
tripdata2$end_date <- mdy(tripdata2$end_date)

# Change blank entries into NAs
tripdata2[tripdata2 == ""] <- NA

# Clean weatherdata before analysis
summary(weatherdata)

# Check for duplicates
any(duplicated(weatherdata))

#Change "T" to 0.01 to indicate trace amounts of precipitation
weatherdata$precipitation_inches <- 
 as.numeric(ifelse(weatherdata$precipitation_inches == "T", 
                    0.01, weatherdata$precipitation_inches))

# Change date into Date class
weatherdata$date <- mdy(weatherdata$date)
weatherdata[weatherdata == ""] <- NA

# EXPLORATORY DATA ANALYSIS

install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(Hmisc)

# EDA on stationdata
station_eda <- function(stationdata)
{
  glimpse(stationdata)
  print(status(stationdata))
  freq(stationdata) 
  print(profiling_num(stationdata))
  plot_num(stationdata)
  describe(stationdata)
}

station_eda(stationdata)

# EDA on tripdata
trip_eda <- function(tripdata2)
{
  glimpse(tripdata2)
  print(status(tripdata2))
  freq(tripdata2) 
  print(profiling_num(tripdata2))
  plot_num(tripdata2)
  describe(tripdata2)
}

trip_eda(tripdata2)


# EDA on weatherdata
weather_eda <- function(weatherdata)
{
  glimpse(weatherdata)
  print(status(weatherdata))
  freq(weatherdata) 
  print(profiling_num(weatherdata))
  plot_num(weatherdata)
  describe(weatherdata)
}

trip_eda(weatherdata)

# DETERMINE CANCELLED TRIPS
# Create variable that satisfies both conditions of a cancelled trip:
# Duration of the trip is less than 3 minutes and the start and end station are the same
canc_trip <- which(tripdata2$duration / 60 < 3 & tripdata2$start_station_name == tripdata2$end_station_name)
canc_trip_id <- tripdata2$id[canc_trip]

# Count number of trips that are cancelled
length(canc_trip)

# Remove rows of cancelled trips
tripdata3 <- tripdata2[-canc_trip, ]

# Record IDs of cancelled trips
write.csv(canc_trip, "1", row.names = FALSE)

# IDENTIFY OUTLIERS

# Overview of variables in each dataset 
colnames(stationdata)
colnames(tripdata3)
colnames(weatherdata)

# Create function to detect outliers using IQR
det_outl <- function(df, col, multiplier = 1.5) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  
  which(df[[col]] < lower_bound | df[[col]] > upper_bound)
}

# Choosing tripdata3 variables for outlier detection
trip_var <- c("duration")

# Visualize outliers in tripdata3
boxplot(tripdata3$duration, main = "Distribution of Trip Duration Data",
        xlab = "Trip Duration")

# Detect outliers
trip_outl <- lapply(trip_var, function(col) det_outl(tripdata3, col))

# Unlist the list of outliers and select only unique outliers for removal
# to avoid duplicate removals
trip_outl_all <- unique(unlist(trip_outl))

# Record outlier trip IDs
trip_outl_id <- tripdata3$id[trip_outl_all]

# Investigate subscription type of tripdata4 outliers
outlier_data <- tripdata3 %>% 
  filter(id %in% trip_outl_id) %>%
  select(id, duration, subscription_type) # Adjust column names as needed

# Count the number of customers and subscribers
subscription_counts <- outlier_data %>%
  group_by(subscription_type) %>%
  summarise(count = n())

# Create a bar plot of subscription types for the outliers
ggplot(outlier_data, aes(x = subscription_type)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Subscription Types Among Outliers",
       x = "Subscription Type",
       y = "Count") +
  theme_minimal()

# Record outlier trip IDs
trip_outl_id<- tripdata3$id[trip_outl_all]

# Save trip outliers in csv file
write.csv(data.frame(TripID = trip_outl_id), "trip_outliers_ids.csv", row.names = FALSE)

# Remove duration outliers from the trip dataset
tripdata4 <- tripdata3[-trip_outl_all, ]

summary(tripdata4)

# Visualize tripdata4 after removal of outliers
boxplot(tripdata4$duration, main = "Distribution of Trip Duration Data (without outliers)",
        xlab = "Trip Duration")

# Choosing weatherdata variables for outlier detection
weather_var <- c("max_temperature_f", "mean_temperature_f", "min_temperature_f",
                     "max_visibility_miles", "mean_visibility_miles", "min_visibility_miles",
                     "max_wind_speed_mph", "mean_wind_speed_mph", "max_gust_speed_mph",
                     "cloud_cover")

# Detect outliers for each weather variable chosen
weather_outl <- lapply(weather_var, function(col) det_outl(weatherdata, col))

# Unlist the list of outliers and select only unique outliers for removal
# to avoid duplicate removals
weather_outl_all <- unique(unlist(weather_outl))

# Remove duration outliers from the weather dataset
weatherdata2 <- weatherdata[-weather_outl_all, ]

summary(weatherdata2)

# IDENTIFY RUSH HOURS
library(lubridate)
library(dplyr)

# Remove extra spaces in start_time so we can convert it to POSIXct 
tripdata4$start_time <- trimws(tripdata4$start_time)

# Combine start_date and start_time to create a start_datetime column
tripdata4$start_datetime <- as.POSIXct(paste(tripdata4$start_date, tripdata4$start_time), format="%Y-%m-%d %H:%M")

# Extract day of the week and hour
tripdata4 <- tripdata4 %>%
  mutate(
    day = wday(start_datetime, label = TRUE),  
    hour = hour(start_datetime) 
  )

# Number of trips per hour during each weekday
hourly_volume <- tripdata4 %>%
  filter(day %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%  # Exclude weekends
  group_by(day, hour) %>%
  summarise(trip_count = n(), .groups = 'drop')  # Calculate trips per hour

# Find the top peak hour for each weekday 
top_peak_hours <- hourly_volume %>%
  group_by(day) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 2) %>%  
  ungroup()

# Identify rush hours over the weekdays 
weekday_rush <- hourly_volume %>%
  group_by(hour) %>%
  summarise(trip_count = sum(trip_count), .groups = 'drop') 

# Histograms visualizing peak hours over the weekdays
ggplot(weekday_rush, aes(x = hour, y = trip_count)) +
  geom_col(data = weekday_rush, aes(x = hour, y = trip_count / 1000), 
           size = 1.2, show.legend = FALSE, fill = "maroon") +
  labs(title = "Trip Volume by Hour from Monday to Friday",
       x = "Hour of the Day",
       y = "Trip Count in Thousands") +
  theme_minimal()

# Histograms visualizing peak hours per weekday
# Encircled red bars are the top peak hours
ggplot(hourly_volume, aes(x = hour, y = trip_count, fill = day)) +
  geom_col() +
  facet_wrap(~day, scales = 'free_y') +
  geom_point(data = top_peak_hours, aes(x = hour, y = trip_count), 
             shape = 21, color = "red", fill = NA, size = 5, stroke = 1.5) +
  labs(title = "Trip Volume by Hour for Each Weekday",
       x = "Hour of the Day",
       y = "Trip Count") +
  theme_minimal()

# RUSH HOUR STATIONS
# Filter trips for peak hours
filt_rush_trips <- tripdata4 %>%
  semi_join(top_peak_hours, by = c("day", "hour"))

# Identify top 10 starting stations during rush hour per weekday
top_start_stations <- filt_rush_trips %>%
  group_by(day, hour, start_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(day, hour) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# Histograms visualizing peak hours over the weekdays
# Create a plot
ggplot(top_start_stations, aes(x = factor(paste(day, hour, sep = "-")), y = trip_count, fill = start_station_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Starting Stations During Rush Hour per Weekday",
       x = "Day-Hour",
       y = "Trip Count",
       fill = "Start Station Name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Identify top 10 ending stations during rush hour per weekday
top_end_stations <- filt_rush_trips %>%
  group_by(day, hour, end_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(day, hour) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# Create a plot
ggplot(top_end_stations, aes(x = factor(paste(day, hour, sep = "-")), y = trip_count, fill = end_station_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Ending Stations During Rush Hour per Weekday",
       x = "Day-Hour",
       y = "Trip Count",
       fill = "End Station Name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# WEEKDAY STATIONS
# Identify top 10 most frequent starting stations on the Weekend
top_start_weekend <- tripdata4 %>%
  filter(day %in% c("Sun", "Sat")) %>%
  group_by(day, start_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(day) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# Create a plot
ggplot(top_start_weekend, aes(x = factor(day), y = trip_count, fill = start_station_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Starting Stations on Weekends",
       x = "Day",
       y = "Trip Count",
       fill = "Start Station Name") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(legend.position = "right")

# Identify top 10 most frequent ending stations on the Weekend
top_end_weekend <- tripdata4 %>%
  filter(day %in% c("Sun", "Sat")) %>%
  group_by(day, end_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  arrange(day, desc(trip_count)) %>%
  group_by(day) %>%
  slice_head(n = 10)

# Create a plot
ggplot(top_end_weekend, aes(x = factor(day), y = trip_count, fill = end_station_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Ending Stations on Weekends",
       x = "Day",
       y = "Trip Count",
       fill = "End Station Name") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(legend.position = "right")

#Calculate the average utilization of bikes for 
#each month (total time used/total time in month).

# BIKE USAGE
tripdata4 <- tripdata4 %>%
  mutate(
    month = month(start_datetime, label = TRUE),  
  )

# Create a data frame with days per month using short-form month names
days_per_month_df <- tibble(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  # Adjust February if needed
)

# Calculate total time used by bikes for each month
total_time_used <- tripdata4 %>%
  group_by(month) %>%
  summarise(total_time_used = sum(duration) / 60, .groups = 'drop')  # Convert to minutes

# Calculate how many unique bike ids there were per month to figure out number of bikes
unique_bikes_per_month <- tripdata4 %>%
  group_by(month) %>%
  summarise(num_bikes = n_distinct(bike_id), .groups = 'drop')

# Calculate average utilization per month
avg_utilization <- tripdata4 %>%
  group_by(month) %>%
  summarise(total_time_used = sum(duration) / 60, .groups = 'drop') %>%  # Convert to minutes
  left_join(unique_bikes_per_month, by = "month") %>%
  left_join(days_per_month_df, by = "month") %>%  # Join with days_per_month_df
  mutate(
    total_time_available = days * 24 * 60 * num_bikes,  # Total time per month in minutes
    utilization_mins = total_time_used / total_time_available  # Calculate utilization ratio
  ) %>%
  select(month, utilization_mins)  # Select relevant columns

# WEATHER DATA
install.packages("corrplot")
library(corrplot)

# Create new variable that calculates trip frequency per day
trip_frequency <- tripdata4 %>%
  group_by(start_date) %>%
  summarise(trip_count = n())

# Join the trip frequency data with the main dataset (e.g., `tripdata4`)
tripdata5 <- tripdata4 %>%
  left_join(trip_frequency, by = "start_date")

# Select only the necessary columns from stationdata
stationdata_city <- stationdata %>%
  select(name, city)  

# Convert zip_code to integer to join with weatherdata zip_code
tripdata5$zip_code <- as.integer(tripdata5$zip_code)

# Join tripdata4 with the filtered stationdata on start_station_name and name
tripdata_station <- tripdata5 %>%
  left_join(stationdata_city, by = c("start_station_name" = "name"))

# Join the tripdata_station dataset with weatherdata2 to join by city and zip_code
# assuming `weatherdata2` has city and zip_code columns
final_data <- tripdata_station %>%
  left_join(weatherdata2, by = c("city" = "city", "zip_code" = "zip_code"))

# CORRELATION ANALYSIS ON WEATHER AND TRIP VARIABLES
# Inspect the data
str(final_data)

# Select numeric variables from the dataset
trip_weather_num <- final_data %>%
  select(trip_count, hour, duration, max_temperature_f, mean_temperature_f, min_temperature_f, 
         min_visibility_miles, max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph, 
         cloud_cover)

# Remove rows with NA values
trip_weather_numcl <- na.omit(trip_weather_num)

# Create the correlation matrix
cor_matrix <- cor(trip_weather_numcl, use = "complete.obs")

# Plot the correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)
# Plot the correlation matrix using ggcorrplot
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, type = "full", 
           colors = c("blue", "white", "maroon"), 
           ggtheme = ggplot2::theme_gray())

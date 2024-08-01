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

# Change "T" to 0.01 to indicate trace amounts of precipitation
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
write.csv(canc_trip, "cancelled_trips.csv", row.names = FALSE)

# IDENTIFY OUTLIERS

# Overview of variables in each dataset 
colnames(stationdata)
colnames(tripdata3)
colnames(weatherdata)

# Create function to detect outliers using IQR

det_outl <- function(df, col, multiplier = 2) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - multiplier * IQR
  upper_bound <- Q3 + multiplier * IQR
  
  which(df[[col]] < lower_bound | df[[col]] > upper_bound)
}

# Choosing tripdata3 variables for outlier detection
trip_var <- c("duration")

# Detect outliers
trip_outl <- lapply(trip_var, function(col) det_outl(tripdata3, col))

# Unlist the list of outliers and select only unique outliers for removal
# to avoid duplicate removals
trip_outl_all <- unique(unlist(trip_outl))

# Record outlier trip IDs
trip_outl_id<- tripdata3$id[trip_outl_all]
write.csv(data.frame(TripID = trip_outl_id), "trip_outliers_ids.csv", row.names = FALSE)

# Remove duration outliers from the trip dataset
tripdata4 <- tripdata3[-trip_outl_all, ]

summary(tripdata4)

# Choosing weatherdata variables for outlier detection
weather_var <- c("max_temperature_f", "mean_temperature_f", "min_temperature_f",
                     "max_visibility_miles", "mean_visibility_miles", "min_visibility_miles",
                     "max_wind_speed_mph", "mean_wind_speed_mph", "max_gust_speed_mph",
                     "precipitation_inches", "cloud_cover")

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
  slice_head(n = 1) %>%  
  ungroup()

# Histograms visualizing peak hours per weekday
# Encircled red bars are the top peak hours
ggplot(hourly_volume, aes(x = hour, y = trip_count, fill = day)) +
  geom_col() +
  facet_wrap(~day, scales = 'free_y') +
  geom_col(data = top_peak_hours, aes(x = hour, y = trip_count), 
           color = "red", fill = NA, size = 1.2, show.legend = FALSE) +
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

# Identify top 10 ending stations during rush hour per weekday
top_end_stations <- filt_rush_trips %>%
  group_by(day, hour, end_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(day, hour) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# WEEKDAY STATIONS
# Identify top 10 most frequent starting stations on the Weekend
top_start_weekend <- tripdata4 %>%
  filter(day %in% c("Sun", "Sat")) %>%
  group_by(day, start_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(day) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# Identify top 10 most frequent ending stations on the Weekend
top_end_weekend <- tripdata4 %>%
  filter(day %in% c("Sun", "Sat")) %>%
  group_by(day, end_station_name) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  arrange(day, desc(trip_count)) %>%
  group_by(day) %>%
  slice_head(n = 10)

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
  left_join(days_per_month_df, by = "month") %>%  # Join with days_per_month_df
  mutate(
    total_time_available = days * 24 * 60 * unique_bikes_per_month$num_bikes,  # Total time per month in minutes
    utilization_mins = total_time_used / total_time_available  # Calculate utilization ratio
  ) %>%
  select(month, utilization_mins)  # Select relevant columns

# WEATHER DATA
install.packages("corrplot")
library(corrplot)

# Combine tripdata4 and weatherdata2 by matching start_date to date and zip_code
tripdata4$zip_code <- as.integer(tripdata4$zip_code) # Change tripdata4 zip_code to integer for joining

trip_weather_combo <- inner_join(tripdata4, weatherdata2, 
                                 by = c("start_date" = "date", "zip_code" = "zip_code"))

# Left join so we can keep all the info from tripdata4 and include matching weatherdata2 where possible
trip_weather_combo2 <- left_join(tripdata4, weatherdata2, 
                                 by = c("start_date" = "date", "zip_code" = "zip_code"))

# CORRELATION ANALYSIS ON WEATHER AND TRIP VARIABLES
# Inspect the data
str(trip_weather_combo)

# Select numeric variables from the dataset
trip_weather_num <- trip_weather_combo %>%
  select(hour, duration, max_temperature_f, mean_temperature_f, min_temperature_f, 
         max_visibility_miles, mean_visibility_miles, min_visibility_miles, 
         max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph, 
         precipitation_inches, cloud_cover)

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


model <- lm(duration ~ max_temperature_f + mean_temperature_f + 
              min_temperature_f + max_visibility_miles + mean_visibility_miles +
              min_visibility_miles + max_wind_speed_mph + mean_wind_speed_mph +
              max_gust_speed_mph + precipitation_inches + cloud_cover, 
            data = combined_data)

# Create a time series plot
ggplot(trip_weather_combo, aes(x = start_date, y = duration)) +
  geom_line() +
  labs(title = "Bike Rentals Over Time", x = "Date", y = "Duration (minutes)")

# BTC1855 - CODING IN R
# MIDTERM PROJECT
# CRYSTAL LEE

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

# Change date into Date class
weatherdata$date <- mdy(weatherdata$date)
weatherdata[weatherdata == ""] <- NA

# EXPLORATORY DATA ANALYSIS

install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
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
trip_eda2 <- function(tripdata2)
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

# Checking tripdata3 variables
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



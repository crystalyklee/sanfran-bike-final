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


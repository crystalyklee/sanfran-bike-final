# BTC1855 - CODING IN R
# MIDTERM PROJECT
# CRYSTAL LEE

#' Any trip starting and ending at the same station, 
#' with duration less than 3 minutes is likely a 'cancelled trip'.
#' Find out the number of such trips, record the trip ids for 
#' your report and then remove them from the dataset.

stationdata <- read.csv('station.csv')
tripdata <-read.csv('trip.csv')
weatherdata <- read.csv('weather.csv')

# Check for duplicates 
any(duplicated(stationdata))
any(duplicated(tripdata))
any(duplicated(weatherdata))




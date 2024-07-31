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


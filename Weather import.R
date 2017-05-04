#### Holly Self #####
## MSc AMFE Univeristy of Aberdeen ##
## ZO5901 Research Project ##

#### Set Up ####
wd <- "C:\\Users\\Holly\\Google Drive\\Masters\\Thesis\\Data\\weather"
setwd(wd)

#### Import SST Data ####
# Data downloaded in 3 month chunks from http://www.ndbc.noaa.gov/station_history.php?station=51203
# Station; Station 51203 - Kaumalapau, HI (146), Lanai'i

#### Import Data ####
files <- list.files("C:\\Users\\Holly\\Google Drive\\Masters\\Thesis\\Data\\weather")
SST <- read.table("51203_2010.txt", header = TRUE)

for( i in 2:7)
{
  x <- files[i]
  SST<- rbind(SST, read.table(x, header = TRUE))
}

## Format collumns 
# Combine separated year.month,day and minute cols 
SST$datetime <- paste(SST$YY,"-",SST$MM,"-", SST$DD," ", SST$hh, ":", SST$mm, sep = "")

# convert to R datetime format 
SST$datetime <- strptime(SST$datetime, format = "%Y-%m-%d %H:%M")

#### Rename Cols ####

# details from; http://www.ndbc.noaa.gov/measdes.shtml

colnames(SST) <- c("Year", "Month", "Day","Hour", "Minute", "Wind Direction", "Wind Speed", 
                   "Gust","Wave Height", "Dominant Wave Period", "Average Wave Period",
                   "Wave Direction", "Air Pressure", "Air Temperature", "SST", "Dewpoint Temp",
                   "Visibility", "Pressure Tendency", "Tide height")

#### Export data to csv ####

write.csv(SST, file = "weather.csv", row.names = FALSE)


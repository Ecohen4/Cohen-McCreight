# The following libraries will be required for this script

require(ggmap)  
require(FNN) 
require(reshape) 
require(gridExtra) 
require(plyr)


####### Load Weather Stations #######

# First step is to download a CSV that has a list of all weather stations
url.location <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"

# Set the directory in which you would like to save the CSV file:
setwd("/Users/mrp/Documents/Columbia/RA/weather_data/")

# Download the data and load it into R
download.file(url.location, "isd_weather_station_history.csv",quiet = TRUE)
weather.stations <- read.csv("isd_weather_station_history.csv")

# For ease, we'll change the names of a few columns
colnames(weather.stations)[c(3, 9)] <- c("NAME", "ELEV")

# Now take out all weather stations that have NAs for LAT & LON 
# This will eiliminate 1,311 stations
weather.stations <- weather.stations[!is.na(weather.stations$LAT) & !is.na(weather.stations$LON),]



####### Cities #######

# Ready to start getting specific weather stations.
# First create a list of cities of interest:
cities.of.interest <- c("New York City", "Philadelphia")



####### Station Selection & Data Download #######

# Function to find the k-nearest weather stations to cities of interest
# The function has two input parameters
# 1) City -- this can be a single city, or a vector of cities
# 2) k -- this is the number of stations to return

k.nearest.stations <- function(city, k)
{
  coordinates.of.interest <- geocode(city)
  nearest.stations <- get.knnx(as.matrix(weather.stations[,c(8,7)]),
                               as.matrix(coordinates.of.interest), k)
  
  stations.of.interest <- weather.stations[nearest.stations$nn.index[,],]
  
  stations.of.interest$rank <- rep(seq(from=1,to=nrow(coordinates.of.interest) ,by=1),
                                   nrow(stations.of.interest)/nrow(coordinates.of.interest))
  
  stations.of.interest$city <- rep(city,nrow(stations.of.interest)/nrow(coordinates.of.interest))
  
  stations.of.interest <- stations.of.interest[order(stations.of.interest$city), ]
  
  stations.of.interest$BEGIN_Date <- as.numeric(substr(stations.of.interest$BEGIN, 1, 4)) # keep original dates, use new column
  stations.of.interest$END_Date <- as.numeric(substr(stations.of.interest$END, 1, 4))
  
  assign("stations.of.interest", stations.of.interest, envir = .GlobalEnv) 
} # End function
k.nearest.stations(cities.of.interest, 5)


# Function to download data for cities of interest between given date ranges (in years)
weather.data <- function(city, beg.date, end.date)
{
  st.list <- stations.of.interest[stations.of.interest$BEGIN_Date <= beg.date &
                                    stations.of.interest$END_Date >= end.date, ]
  temp.log <- as.data.frame(matrix(NA, nrow(st.list),3))
  colnames(temp.log) <- c("File","DL Status", "City")
  temp.log$City <- st.list$city
  output.log <- data.frame()
  
  for (year in beg.date:end.date)
  {
    for (i in 1:nrow(temp.log))
    {
      temp.log[i, 1] <- paste(sprintf("%06d", st.list[i,1]), "-", 
                              sprintf("%05d", st.list[i,2]), "-", year,
                              ".gz", sep = "")
      
      tryCatch(
      {
          download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", 
                      temp.log[i, 1], sep = ""),paste(temp.log[i,1]))
          temp.log[i,2] <- "Success"
      },

          error=function(cond)
            {
              temp.log[i,2] <- "Failed"
              return(NULL)
              next
              })
    } # end i

      output.log <- rbind(output.log, temp.log) # Combine each year's log
      assign("output.log", output.log, envir = .GlobalEnv) 

  } # end year 

write.csv(output.log, paste("output_log", beg.date, end.date, 
                            paste("(cities:",length(city),")",sep=""),
                            format(Sys.time(), "%s"), sep="_"), row.names=F)
} # End function
weather.data(cities.of.interest,2012, 2013)


# Function to convert the .gz files into csv,
# and deletes the .gz files once processed
file.conversions <- function()
{
  files <- list.files(getwd(), pattern="*.gz")
  column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                     7, 5, 5, 5, 4, 3, 1, 1, 4, 1,
                     5, 1, 1, 1, 6, 1, 1, 1, 5, 1,
                     5, 1, 5, 1) # I need to understand what all of these represent...
  
  stations <- as.data.frame(matrix(NA, length(files),6))
  colnames(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")
  
  for (i in 1:length(files)) 
  {
    tryCatch({
      data <- read.fwf(gzfile(files[i]), column.widths)
      data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33)]
      names(data) <- c("USAFID", "WBAN", "YR", "M",
                       "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                       "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT",
                       "ATM.PRES")
    },
    
    error=function(cond)
    {
      return(NULL)
      next
    })
    
    data$LAT <- data$LAT/1000
    data$LONG <- data$LONG/1000
    data$WIND.SPD <- data$WIND.SPD/10
    data$TEMP <- data$TEMP/10
    data$DEW.POINT <- data$DEW.POINT/10
    data$ATM.PRES <- data$ATM.PRES/10
    write.csv(data, file = paste(sprintf("%s", output.log[i,3]), "-",
                                 substr(output.log[i,1],1,17),
                                 ".csv", sep = ""), row.names = FALSE)
    stations[i, 1:3] <- data[1, 1:3]
    stations[i, 4:6] <- data[1, 8:10]
    
  }
  assign("stations", stations, envir = .GlobalEnv) 
  file.remove(files)
} # End Function
file.conversions()

# It looks like the 'stations' dataframe can be combined with 'output log'
# Will get back to that...


# Combine the csv files from the same weather stations
# Save the combined file, and delete the individual .csvs
files <- list.files(getwd(),pattern="*[0-9].csv")
for (i in 1:nrow(count(substr(files, 1, nchar(files)-9))))
{
  station.list <- as.vector(count(substr(files, 1, nchar(files)-9))[,1])
  counts <- as.vector(count(substr(files, 1, nchar(files)-9))[,2])
  
  beg.count <- sum(counts[1:i])-1
  end.count <- sum(counts[1:i])
  
  if(i == 1) {start <- 1}
  else {start <- beg.count}
  
  station.data <- lapply(files[start:end.count],
                         read.csv, header = TRUE)
  
  combined.df <- do.call(rbind , station.data)
  
  assign(as.character(count(substr(files, 1, nchar(files)-9))[i,1]), combined.df)
  
  write.csv(combined.df, 
            paste(as.character(count(substr(files, 1, nchar(files)-9))[i,1]),".csv",sep=""))
  
  file.remove(files[start:end.count])
}


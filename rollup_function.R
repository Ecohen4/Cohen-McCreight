
############################################
#                 Setup                    #
############################################

# Load/Install required packages

load_install<-function(lib){
  if(! require(lib, character.only=TRUE)) install.packages(lib, character.only=TRUE) 
  library(lib, character.only=TRUE, quietly=TRUE)
}
Thelib<-c("knitr", "rmarkdown", "plyr", "reshape2", "ggplot2", "scales",
          "xlsx", "sm", "ggmap", "FNN", "gridExtra", "McSpatial", "httr", "data.table")
lapply(Thelib, load_install)


############################################
#     Step 1: Get List of Stations         #
############################################

all_stations <- function()
{
  isd <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  response <- suppressWarnings(GET(isd))
  all <- read.csv(text=content(response, "text"), header = TRUE)
  colnames(all)[c(3, 9)] <- c("NAME", "ELEV")
  all <- all[!is.na(all$LAT) & !is.na(all$LON),]
  return(all)
}
stations <- all_stations()


############################################
#  Step 2: Create a list of cities         #
############################################

cities <- c("Nairobi, Kenya")


############################################
#  Step 3: Download Stations of interest   #
############################################

# The next function takes the following arguments:

# city.list: vector of cities
# station.list: list of all weather stations -- see prior function
# k: number of nearest stations to each city's reference point
# beg: beginning 4 digit year to get data, eg, 2011
# end: ending 4 digit year to get data, eg: 2013
# int: minimum interval desired, eg: hourly = 1; every 3 hours = 3, etc..
# tol: tolerance for missing values, eg: .05 for 5% missing values, .10 for 10%, etc..
# dist: maximum distance from each city's reference point (in kilometers)

kn_stations <- function(city.list, station.list, k, beg, end, int, tol, dist)
{
  # Finding the reference location for each city
  coords <- geocode(city.list)  
  # Find k-nearest weather stations to the reference point
  kns <- get.knnx(as.matrix(station.list[,c(8,7)]),as.matrix(coords), k)
  st <- station.list[kns$nn.index[,],]
  # Add additional fields to the dataset
  st$city <- rep(city.list,nrow(st)/nrow(coords)) # Reference City
  st$Ref_Lat <- rep(coords$lat,nrow(st)/nrow(coords)) # Reference Latitude
  st$Ref_Lon <- rep(coords$lon, nrow(st)/nrow(coords)) # Reference Longitude
  kilos.per.mile <- 1.60934 
  st$kilo_distance <- geodistance(st$Ref_Lon,st$Ref_Lat,st$LON,st$LAT,
                                  dcoor = FALSE)$dist*kilos.per.mile # Convert miles to kilos
  st <- st[with(st,order(city, kilo_distance)),]
  st$rank <- rep(1:k,length(city.list)) # Rank is list from 1-k, closest to farthest
  st$BEGIN_Date <- as.numeric(substr(st$BEGIN,1,4))
  st$END_Date <- as.numeric(substr(st$END, 1, 4))
  st <- st[st$BEGIN_Date <= beg & st$END_Date >= end, ] # remove stations without complete data
  
  # Setup to download the data
  base.url <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"
  nstations <- nrow(st)
  yrs <- seq(beg,end,1)
  nyrs <- end-beg+1
  usaf <- as.numeric(st$USAF)
  wban <- as.numeric(st$WBAN)
  # The following dataframe will be printed to show 
  # which files were successfully downloaded
  output <- data.frame()
  temp <- as.data.frame(matrix(NA,nstations,5)) 
  names(temp) <- c("File","Status", "City", "rank", "kilo_distance")
  temp$City <- st$city
  temp$rank <- st$rank
  temp$kilo_distance <- st$kilo_distance
  
  # Setup for the list of data
  temp.list <- df.list <- list()
  col.width <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1,
                 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1) # Fixed width datasets
  col.names <- c("CHARS", "USAFID", "WBAN", "YR", "M", "D", "HR", "MIN",
                 "DATE.FLAG", "LAT", "LONG", "TYPE.CODE", "ELEV", "CALL.LETTER",
                 "QLTY", "WIND.DIR", "WIND.DIR.QLTY", "WIND.CODE",
                 "WIND.SPD", "WIND.SPD.QLTY", "CEILING.HEIGHT", "CEILING.HEIGHT.QLTY",
                 "CEILING.HEIGHT.DETERM", "CEILING.HEIGHT.CAVOK", "VIS.DISTANCE",
                 "VIS.DISTANCE.QLTY", "VIS.CODE", "VIS.CODE.QLTY", 
                 "TEMP", "TEMP.QLTY", "DEW.POINT", "DEW.POINT.QLTY", 
                 "ATM.PRES", "ATM.PRES.QLTY")
  city.names <- unlist(lapply(strsplit(st$city,", "), function(x) x[1])) # City Name
  df.names <- paste(city.names, st$USAF, sep="_") # City Name_USAF#
  
  # Download the desired stations into a list (does not save to disk)
  for (i in 1:nyrs)
  {
    for (j in 1:nstations)
      {
        # Create file name
        temp[j,1] <- paste(usaf[j],"-",wban[j],"-", yrs[i], ".gz", sep = "")
        tryCatch({
          # Create connect to the .gz file
          gz.url <- paste(base.url, yrs[i], "/", temp[j, 1], sep="")
          con <- gzcon(url(gz.url))
          raw <- textConnection(readLines(con))
          # Read the .gz file directly into R without saving to disk
          temp.list[[j]] <- read.fwf(raw, col.width)
          # Some housekeeping:
          names(temp.list)[j] <- df.names[j]
          names(temp.list[[j]]) <- col.names
          temp.list[[j]]$LAT <- temp.list[[j]]$LAT/1000
          temp.list[[j]]$LONG <- temp.list[[j]]$LONG/1000
          temp.list[[j]]$WIND.SPD <- temp.list[[j]]$WIND.SPD/10
          temp.list[[j]]$TEMP <- temp.list[[j]]$TEMP/10
          temp.list[[j]]$DEW.POINT <- temp.list[[j]]$DEW.POINT/10
          temp.list[[j]]$ATM.PRES <- temp.list[[j]]$ATM.PRES/10
          temp.list[[j]]$city <- city.names[j]
          temp.list[[j]]$distance <- st$kilo_distance[j]
          temp.list[[j]]$rank <- st$rank[j]
          
          temp[j,2] <- "Success"
          
        },
        error=function(cond)
        {
          return(NA)
          next
       },
        finally={ # if any of the files didn't download successfully, label as such
          if(is.na(temp[j,2])=="TRUE") temp[j,2] <- "Failed"
        })
      }
    # Combine each year's status and list
    output <- rbind(output, temp)
    df.list <- append(df.list, temp.list)
  }
  # Print the results
  print(output)
  
  # Combine the data from the same stations
  comb.list <- list()
  keys <- unique(names(df.list))
  nkeys <- length(keys)
  for (i in 1:nkeys)
  {
    track <- which(names(df.list)==keys[i])
    comb.list[[i]] <- as.data.frame(rbindlist(df.list[track]))
    names(comb.list)[i] <- keys[i]
  }
  
  ### Filter the data ###
  # 1) remove stations with little to no data at all
  rm.junk <- names(comb.list[which(sapply(comb.list, function(x) dim(x)[1] <= 10))]) 
  comb.list <- comb.list[which(sapply(comb.list, function(x) dim(x)[1] > 10))]
  
  # 2) remove stations that exceed maximum distance
  rm.dist <- names(comb.list[which(sapply(comb.list, function(x) max(x["distance"])) > dist)]) 
  comb.list <- comb.list[which(sapply(comb.list, function(x) max(x["distance"])) < dist)]
  
  # keep track of which stations have been removed
  rm.tmp <- unique(c(rm.junk, rm.dist))
  
  # 3.a) remove stations that exceed threshold of missing data,
  # start with counting the 999s:
  cl <- c("TEMP", "DEW.POINT") # Additional columns can be added
  ix <- NULL
  ix.ct <- as.data.frame(matrix(nrow=length(comb.list), ncol=length(cl)))
  colnames(ix.ct) <- cl
  rownames(ix.ct) <- names(comb.list)
  for (L in 1:length(comb.list))
  {
    for (i in 1:length(cl))
    {
      ix <- which(comb.list[[L]][cl[i]]==999.9 | comb.list[[L]][cl[i]]==999 |
                       comb.list[[L]][cl[i]]==99.99 | is.na(comb.list[[L]][cl[i]]))
      ix.ct[L,i] <- length(ix)
    }
  }
  ix.ct$temp_pct <- ix.ct[,cl[1]]/unlist(lapply(comb.list,nrow))
  ix.ct$dew_pct <- ix.ct[,cl[2]]/unlist(lapply(comb.list,nrow))
  print(ix.ct)
  ms.obs <- (ix.ct$TEMP+ix.ct$DEW.POINT)
  
  #ms.ix <- which(ix.ct$temp_pct > .10 | ix.ct$dew_pct > .10)
  #rm.ms <- names(comb.list[ms.ix])
  #rm.tmp <- unique(c(rm.dist, rm.dist))
  #if(length(rm.ms)==0) comb.list <- comb.list else comb.list <- comb.list[-ms.ix]
  
  # 3.b) set a minimum number of observationsand subtract 999s
  min.obs <- (24/int)*365*nyrs*(1-tol)
  obs.ix <- which(sapply(comb.list, nrow) < (min.obs-ms.obs))
  rm.obs <- names(comb.list[which(sapply(comb.list, nrow) < (min.obs-ms.obs))])
  if(length(rm.obs)==0) comb.list <- comb.list else comb.list <- comb.list[-obs.ix]
  
  # update removed stations
  rm.all <- unique(c(rm.tmp, rm.obs))
  
  # 4) All current stations are assumed to be adequet, 
  #   we therefore will take the closest to each reference point
  kept.names <- substr(names(comb.list),1,nchar(names(comb.list))-7)
  kept.ranks <- unname(unlist(lapply(comb.list, function(x) x["rank"][1,1])))
  f.df <- data.frame(location=kept.names, ranks=kept.ranks)
  kp.ix <- as.numeric(rownames(f.df[which(ave(f.df$ranks,f.df$location,FUN=function(x) x==min(x))==1),]))
  final.list <- comb.list[kp.ix]
  
  # Show what was removed during the filtering process:
  kept <- names(comb.list)  
  st.df <- data.frame(count(city.names))
  rm.df <- count(substr(rm.all, 1, nchar(rm.all)-7))
  kept.df <- count(substr(kept, 1, nchar(kept)-7))
  df.list <- list(st.df, rm.df, kept.df)
  mg.df <- Reduce(function(...) merge(..., by="x", all=T), df.list)
  suppressWarnings(mg.df[is.na(mg.df)] <- 0)
  colnames(mg.df) <- c("city", "stations", "removed", "kept")
  print(mg.df)
  
  # Show the stations that will be in the final output:
  print(names(final.list))
  
  return(final.list) 
}
st.interest <- kn_stations(cities, stations, 5, 2011, 2013, 3, .05, 100)

# The returned data will be a list of dataframes in their original raw format.
# In addition to the list of data, you will see 4 separate items printed out.
# 1) List of station data downloaded, stating either success or failed
# 2) The number of NAs or 999s in each station, along with the % of the dataset
# 3) The total number of stations that were downloaded, removed, and kept
#    during the filtering process for each city, prior to the final step
#    of taking the closest station to each reference point
# 4) The name of the stations that are in the final output of the function


############################################
#  Step 4: Write data to file              #
############################################

setwd("~/Desktop/Temp_Data/")

write.csv("Nairobi_Hourly")

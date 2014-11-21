# Use this R script to find the closest weather station to any location (or set of locations).
# choose from NOAA-ISD (n stations=27825) or Hadley-ISD (n station=6103). Hadley-ISD is a filtered subset of NOAA-ISD
# About Hadley-ISD: Stations were selected on the basis of length of record and reporting frequency. 6103 stations passed a suite of automated quality control tests designed to remove bad data while keeping the extremes. None of the ISD flags were used. The QC tests focussed on the temperature, dewpoint temperature and sea-level pressure variables, although some were applied to the wind speed and direction and cloud data. The data files also contain other variables which were pulled through from the raw ISD record, but have had no QC applied. Some final filtering was performed to select those stations which in our opinion are most useful for climate studies

# initialize R session
setwd("~/github/Cohen-McCreight")
library(plyr) # plyr::ddply
library(fields) # fields::rdist.earth - Given two sets of longitude/latitude locations computes the Great circle (geographic) distance matrix among all pairings.

# choose Hadley-ISD
metdata<-read.fwf(file="http://www.metoffice.gov.uk/hadobs/hadisd/v102_2013f/files/hadisd_station_info_v102.txt",  header=FALSE, widths=c(6,6,31,3,8,9,8), strip.white=TRUE)
names(metdata)<-c("USAFID","WBAN","City","Country","LAT","LON","Elev")
str(metdata)

# # choose NOAA-ISH
# wx<-read.csv(file="isd_weather_station_history.csv", header=TRUE, check.names=TRUE, strip.white=TRUE,) # list of all NOAA-ISD weather stations.
# wx<-wx[complete.cases(wx),] # remove incomplete records

# supply a set of lat-lon coords for which you want the nearest available weather data.
# ... for example, here is a dataframe containing lat-long coords for 26 power plants in Northern India.
# ... CGS stands for Central Generating Stations, that is, power plants owned/operated by the central government of India (as opposed to state or private ownership)
stn<-read.csv("India-power-stns/CGSmeta.csv")

# here is the function matching lat-long coords of places of interest (in this case, power plants) to K nearest-neighbor weather stations.
## llDist returns the distance from each place of interest (row) to each weather station (columns)
findClosestISD <- function(stn, knn) {
  llDist <- rdist.earth(matrix(c(stn$X.DEC, stn$Y.DEC), ncol=2),
                        matrix(c(metdata$LON, metdata$LAT), ncol=2),
                        miles=FALSE, R=6371) ## mean radius in km
  sortDistInds <- sort( llDist, ind=TRUE)$ix
  return( cbind(ID=stn[,1],
                distance.km=llDist[sortDistInds[1:knn]],
                metdata[sortDistInds[1:knn],] ) )
}


# findClosestISH <- function(wx, stn, knn) {
#   ## find station name, id, lat,
#   llDist <- rdist.earth(matrix(c(stn$X.DEC,stn$Y.DEC), ncol=2),
#                         matrix(c(wx$LON, wx$LAT), ncol=2),
#                         miles=FALSE, R=6371) ## mean radius of Earth in km
#   sortDistInds <- sort( llDist, ind=TRUE)$ix
#   return( cbind(stnCode=stn$Stn_code,
#                 distance.km=llDist[sortDistInds[1:knn]],
#                 wx[sortDistInds[1:knn],] ) )
# }
knn=7  ## i kept a few to look at, though I end up throwing them out
ISHTempsNearby <- dlply( stn, 1, findClosestISD, knn=knn)
ISHClosest <- ldply(ISHTempsNearby,
                    function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])
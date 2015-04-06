####################################
## use this function to read ncdf datafiles
####################################

#library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

## Define the ncdf files
# whoami<-system('whoami',int=TRUE)
# filepath<- if(whoami!='james') ## since we have different paths.
#   "/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/Hadley" else 
#     "~/Desktop/Dropbox/Cohen-McCreight/Hadley"
# ##jlm: i specified pattern b/c there was a non-.nc file in the directory i'm using. 

#filepath<-"/Users/elliotcohen/Dropbox/Data/Climate/Hadley/WMO_400000-499999_2012p/"
filepath<-"/Users/elliotcohen/Dropbox/data/Climate/Hadley/WMO_100000-199999_clim"

## jlm: can also return the full path
files <- list.files(filepath, pattern=glob2rx('*nc'), full.names=TRUE)
#files <- list.files(filepath)
length(files)

##This is an essential booking step using plyr --> unique Station ID.
names(files)<-laply( strsplit(files, '[./-]'), '[', 15) 

##jlm: define the variables of interest
## the names serve the same plyr book-keeping purpose as above.
vars2Get <- c(temp='temperatures',precip='precip1_depth')

##jlm: define the dates/times of interest.
firstRecord<-as.POSIXct('2011-03-01', tz='UTC')

## getData, purpose: open an ncdf file of station data and take out the relevant variables, subsetting to the desired times. 
getDataNcdf<-function(theFile){
  ## since we got these from list.files, this should not be necessary.
  ## if you were writing a very general purpose function, you'd test the 
  ## file existence.... 
  #if (!file.exists(theFile)) next    
  nc<-open.ncdf(theFile)
  
  ## check that the time dims are the same for all variables
  allParse<- laply(vars2Get, function(theVar) nc$var[[theVar]][['dim']][[1]]$units )
  if(!(all(allParse==allParse[1])))
    warning('Time is messed up for our system',.immed=TRUE)
  ## if they are the same, we can define a common time variable and subset
  timeDim<-nc$var[[vars2Get[1]]][['dim']][[1]]
  parse<-strsplit(timeDim$units,' since ')
  thePeriod<-parse[[1]][1]
  startTime<-as.POSIXct(parse[[1]][2], tz='UTC')
  timePOSIXct<-startTime+as.period(as.integer(timeDim$vals),thePeriod)
  whTimes <- which(timePOSIXct >= firstRecord)
  POSIXct<-timePOSIXct[whTimes]
  
  getVar <- function(theVar) get.var.ncdf( nc, theVar )[whTimes]
  ## ldply dosent work here, so convert a list to a data fame
  ## also note that a plyr command here means you can simply modify the 
  ## vars2Get and that's it!
  out<-as.data.frame(llply(vars2Get, getVar)) 
  out$POSIXct<-POSIXct
  out
}

stnData<-ldply(files, getDataNcdf)
save(stnData, file="stnData.rsav")
####################################
## End function here
####################################

###################################
## Data visualization, QC and preliminary analysis
##################################
library(plyr)     # ddply, ldply, etc...
library(reshape2) # melt()
library(ggplot2)  # ggplot()
library(scales)   # scale_x_date()
library(locfit)   # local polynomial regression 
library(akima)    # interp function
library(fields)   # surface function
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
load("stnData.rsav")
load("d.rain.rsav")
load("metdata.rsav")

#how many NA's?
dim(stnData)
sum(is.na(stnData$precip))

# How many valid precip obs (not NA)?
sum(! is.na(stnData$precip))  

# fraction of obs that are not NA?
sum(! is.na(stnData$precip))/dim(stnData)[1]  # 27%

# aggregate from hourly to daily to see if there is at least one observation per day per station.
# create date attribute
stnData$Date<-as.Date(stnData$POSIXct)
range(stnData$Date) # 2011-02-01 to 2013-01-01
hist(stnData$precip)
# get rid of -999 values
stnData$precip[stnData$precip==-999]<-NA
stnData$precip[stnData$precip==999]<-NA
stnData$precip[stnData$precip==1000]<-NA

# really high values still there... assume any value greater than 100 is erroneus?
#stnData$precip[stnData$precip>100]<-NA
range(stnData$precip)
hist(stnData$precip, breaks=c(0,5,10,20,40,80,250,500,1000, labels=TRUE))

hist<-hist(stnData$precip)
UL<-hist$counts[1]*1.2
plot(hist, freq=TRUE, labels=TRUE, main="Histogram of Precipitation Depths Recorded Across India\nMar 2011 to Jan 2013", ylim=c(0,UL), xlab="Precipitation Depth")

## There appears to be a continuous, yet very small set of observations at the upper tail... do we think these are real?  Should I keep them?  How could precip be 1000 
summary(stnData$precip)  # why are there still values so high (up to 100 cm)?  Could these be real??

# remove NA values
Ptest<-na.omit(stnData$precip)
length(Ptest)
range(Ptest)
plot(density(Ptest))
hist(Ptest)  #nearly all observations are between 0 and 40 (with by far the most in the 0 and 5 bin)
# Plot ALL the precip data
ggplot(stnData, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point()

# aggregate removes NA's by default
d.rain<-aggregate(precip ~ Date + .id, data=stnData, sum)
sum(is.na(d.rain))  #0 NA's.
dim(d.rain)   #174,729 x 3
d.rain<-droplevels(d.rain)
save(d.rain, file="d.rain.rsav")

load("d.rain.rsav")
ggplot(d.rain, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point()

# # merge d.rain with ISDClosest Station metadata...
# names(d.rain)<-c("Date","USAFID","precip")
# d.rain<-merge(d.rain, metdata, by=c("USAFID"))
# names(d.rain)[1]<-".id"
# save(d.rain, file="d.rain.rsav")

# ###################################
# # QC: remove Stn's with less than 1-measurement per day
# # number of days in record
# days<-unique(d.rain$Date)
# ndays<-length(days); ndays
# range(d.rain$Date)
# # number of days in timespan
# test<-seq(range(d.rain$Date)[1], range(d.rain$Date)[2], by="day")
# ntest<-length(test); ntest
# missing<-which(! test %in% days)
# nmissing<-length(missing); nmissing
# test[missing]  #shows the missing dates: 
# # All dates have observations, but is there one per station?.
# freqtest<-table(d.rain$.id)
# drop<-which(freqtest[]<ntest)
# keep<-which(freqtest[]==ntest)
# length(drop)  #339 stations have less than 1 obs per day
# length(keep)  #109 stations have at least 1 obs per day
# 
# # but are these stations in India??
# load("metdata.rsav")
# INstns<-subset(metdata,Country=="IN")
# # subset d.rain to stations with at least one obs per day
# freqtest[keep]
# keep.id<-names(freqtest[keep])
# keep.id %in% INstns$USAFID
# # None of the Stations with at least one obs per day are in India!  Ugh!
# 
# # let's look at the stations with at least one obs per day, anyway...
# test<-d.rain[as.character(d.rain$.id) %in% keep.id,]
# test<-droplevels(test)
# table(test$.id)
# 
# # subset metdata to stations with at least one obs per day
# grab<-which(metdata$USAFID %in% keep.id)
# metdata[grab,]
# # contains stns in Iran, Pakistan, Japan, but none in India
# # same as above...
# # keep.metdata<-metdata[metdata$USAFID %in% keep.id,]
# # dim(keep.metdata)

###################################################
# Since none of the stations with at least one obs per day are in India...
# how many obs are available in the India ISD stations?
##################################################
# INstns<-subset(metdata, Country=="IN")  #77 ISDstns in India
# INdata<-d.rain[d.rain$.id %in% INstns$USAFID, ]  #grab the accompanying data
# INdata<-droplevels(INdata)
# dim(INdata)  #16,668 x 3
# table(INdata$.id)  # observations per station
# save(INdata, file="INdata.rsav")
load("INdata.rsav")
##################################
# Spatial map of observed precipitation
#################################
par(mfrow=c(1,1))
par(mar=c(4, 3, 3, 2) + 0.1) 
library(akima)
library(fields)

# subset by season....
dummy<-strsplit(as.character(INdata$Date), split="-")
INdata$Yr<-laply(dummy, '[[', 1)
INdata$Mon<-laply(dummy, '[[', 2)
m.INdata<-ddply(INdata, .(Yr, Mon, .id, City, Country, Lat, Long, Elev), summarize, precip.total=sum(precip), precip.mean=mean(precip), precip.max=max(precip), .progress="time")

monthly<-m.INdata

# recreate Date attribute
monthly$Date<-as.Date(paste(monthly$Yr, monthly$Mon, 15, sep="-"))

mons<-seq(range(monthly$Date)[1], range(monthly$Date)[2], by="months")

## SURFACE PLOT
par(mfrow=c(1,1)) 
# par(mar=c(5,4,4,2)+0.1) # default
par(mar=c(2,2,1.5,1)+0.1) 
par(oma=c( 0,0,0,4))     # margin of 3 spaces width at right hand side
set.panel(3,4)           # 3X4 matrix of plots

# now draw plots using image() command
for (i in 11:22){
  data<-subset(monthly, Date==mons[i])
  zz0<-interp(x=data$Long, y=data$Lat, z=data$precip.total)
  image(zz0, col=topo.colors(n=64,alpha=1), main=paste(data$Mon[1], data$Yr[1], "Total Precip (mm)", sep=" "), lab.breaks=NULL)
  contour(zz0, add=T)
  world(add=TRUE, lwd=4)
}

set.panel(1,4) # nXm matrix of plots
par(oma=c(0,0,0,1))# reset margin to be much smaller.
par(mar=c(0,0,0,0)+0.0) 
image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(0,max(monthly$precip.total)), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
# image.plot tricked into  plotting in margin of old setting 
# end plot


###########################
## Surface model estimates, and model error
###########################
# zz0<-interp(x=INdata$Long, y=INdata$Lat, z=INdata$precip, duplicate="mean")
# image.plot(zz0, col=topo.colors(n=20,alpha=0.5), ylab="latitude", xlab="longitude", main="Observed Precipitation (mm)")
# contour(zz0, add=T)
# world(add=TRUE, lwd=4)
# 
# zz1<-interp(data$Long,data$Lat,bestmodel$fitted.values, duplicate="mean")
# image.plot(zz1, col=topo.colors(n=20,alpha=0.5), ylab="latitude", xlab="longitude", main="Logit modeled precipitation (mm)")
# contour(zz1, add=T)
# world(add=TRUE, lwd=4)
# 
# zz2<-interp(data$Long,data$Lat,residuals(bestmodel, type="response"), duplicate="mean")
# image.plot(zz2, col=topo.colors(20,0.5), ylab="latitude", xlab="longitude", main="Logit residuals (mm)")
# contour(zz2, add=T)
# world(add=TRUE, lwd=4)
# 
# surface(zz,xlab="Longitude",ylab ="Latitude", main="DEM Grid Predictions") 
# 
# dev.copy(png,"Observed vs. Modeled Precip and Residuals - Binomial Regression.png")
# dev.off()

######################################
## INdata Quality Control
######################################
# There are 672 days in the dataset, how many obs are "enough" for analysis?  Really need one each day, but if not available, then how many?
freqtest<-table(INdata$.id)
hist(freqtest) # histogram of observations per station
# most INstns have between 100 and 400 observations over the 672 day period of interest
range(INdata$precip)  #0 to 610 #should I keep the very high precip values?
hist(INdata$precip)

ggplot(INdata, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point()

# Is there full coverage during the summer (Apr - Sept)?
range(INdata$Date)
# grab May - September
summer1<-subset(INdata, Date > as.Date("2011-05-01") & Date < as.Date("2011-10-01"))
summer2<-subset(INdata, Date > as.Date("2012-05-01") & Date < as.Date("2012-10-01"))
summerRain<-rbind(summer1, summer2)

# QC: remove Stn's with less than 1-measurement per day
# number of days in record
days<-unique(summerRain$Date)
ndays<-length(days); ndays
# range(summerRain$Date)
# number of days in timespan
summerdays1<-seq(range(summer1$Date)[1], range(summer1$Date)[2], by="day")
summerdays2<-seq(range(summer2$Date)[1], range(summer2$Date)[2], by="day")
summerdays<-c(summerdays1, summerdays2)
ntest<-length(summerdays); ntest
missing<-which(! summerdays %in% days)
nmissing<-length(missing); nmissing
summerdays[missing]  #shows the missing dates: 
# 4 days missing altogether  
# "2011-06-09" "2011-06-10" "2011-06-11" "2011-06-12"

# but what about at each station?
freqtest<-table(summerRain$.id)
freqtest
drop<-which(freqtest[]<ntest)
keep<-which(freqtest[]==ntest)
length(drop)  #75 stations have less than 1 obs per day
length(keep)  #0 stations have at least 1 obs per day

par(mfrow=c(2,1))
par(mar=c(3,4,3,2) + 0.1)
plot(table(summer1$Date), main="All-India Precipitation Record, Summer 2011\nObservations per day", ylab="Observations per day")
plot(table(summer2$Date), main="All-India Precipitation Record, Summer 2012\nObservations per day", ylab="Observations per day")

ggplot(summer1, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point() + labs(title="Precipitation Record, All-India, Apr - Sep 2011") + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y"))

ggplot(summer2, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point() + labs(title="Precipitation Record, All-India, Apr - Sep 2012") + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y"))

# # Use code below if want to remove outliers with precip values above a certain threshold
# drop<-which(freqtest[]==0)
# keep<-which(freqtest[]>0)
# length(drop)  
# length(keep)

load("ISDClosest.rsav")
load("ISDClosest2.rsav")
grab1<-unique(ISDClosest$USAFID)
grab2<-unique(ISDClosest2$USAFID)
grab<-unique(c(grab1, grab2))
length(grab) #number of ISD stations to get

wh<-which(grab %in% INstns$USAFID) #11 of 11 nearest neighbors are contained in INstns
length(wh)
grabData<-INdata[INdata$.id %in% grab,]
grabData<-droplevels(grabData)
length(levels(grabData$.id))  #11 of 11 nearest neighbors are contained in INdata

missing<-which(! grab %in% INstns$USAFID)  #NONE.
grab[missing]  # show which USAFID is missing from the Files
ISDClosest[ISDClosest$USAFID %in% grab[missing],]  # show the corresponding Stn

# retrieve missing data... (DONE Dec. 9 2013)
# grab second closest stations instead....
# ISDClosest[ISDClosest$USAFID==grab[missing][1],2:10]<-ISDTempsNearby$SALAL[2,]
# ISDClosest[ISDClosest$USAFID==grab[missing][2],2:10]<-ISDTempsNearby$'SEWA-II'[2,]
# 
# save(ISDClosest, file="ISDClosest.rsav")
# Now re-run previous code... Success!

ggplot(grabData, aes(x=Date, y=precip, group=.id, colour=.id)) + geom_point() + facet_wrap(~.id) + scale_y_continuous(name="Precipitation Depth (mm)") + labs(title="Precipitation Record at Nearest Neighbor Weather Stations\nMar 2011 - Jan 2013") + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b"))


# now repeat KNN ISDClosest with only these stations for which we have daily observations... None in India!  Lower threshold? For temp that may be fine, but for precip will likely give incomplete (or erroneous) picture.
########### 
###########  up to here on Dec. 8 2013
############
source("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/findClosestISD.R")

load("CGSmeta2.rsav")
names(CGSmeta2)<-c("Stn_code","Long","Lat")
summary(CGSmeta2[,c("Long",'Lat')])  ## positive! nice.
summary(subset(metdata, Country=="IN")[,c("Long", "Lat")])

## Impliment findClosestISD function
knn=7  ## i kept a few to look at, though I end up throwing them out
ISDTempsNearby <- dlply(CGSmeta2, 1, findClosestISD, knn=knn )
ISDClosest <- ldply(ISDTempsNearby, function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])

#save(ISDClosest, file="ISDClosest.rsav")

## put the data into chronological order
d.rain<-d.rain[order(d.rain$Date),]

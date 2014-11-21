###################################################
### Chunk 1: Import libs, files and functions
###################################################
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
#setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

require(plyr)
require(reshape2)
source("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/getData/getData.R")
source("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/findClosestISD.R")

load("metdata.rsav")
load("ISDClosest.rsav")
load("ISDClosest2.rsav")

########################
# Read-in HadISD station metadata
# USAFID, WBAN, City, Country, Lat, Long, Elev
########################
metdata<-read.fwf(file="http://www.metoffice.gov.uk/hadobs/hadisd/hadisd_station_info.txt",  header=FALSE, widths=c(6,6,31,3,8,9,8), strip.white=TRUE)
names(metdata)<-c("USAFID","WBAN","City","Country","Lat","Long","Elev")
head(metdata)
save(metdata, file="metdata.rsav")

#########################
## Find nearest neighbor ISD stn to each CGS stn and State Capital
#########################
## Re-do ISHClosest search with "metdata" instead of "ISHmeta" for "CGSmeta2" and for "states"
## Rename ISHClosest to ISDClosest
## match power plants to closest weather stations
## CGS lat-long cords are in CGSmeta2

load("CGSmeta2.rsav")
names(CGSmeta2)<-c("Stn_code","Long","Lat")
summary(CGSmeta2[,c("Long",'Lat')])  ## positive! nice.
summary(subset(metdata, Country=="IN")[,c("Long", "Lat")])

## Impliment findClosestISD function
knn=7  ## i kept a few to look at, though I end up throwing them out
ISDTempsNearby <- dlply(CGSmeta2, 1, findClosestISD, knn=knn )
ISDClosest <- ldply(ISDTempsNearby, function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])

save(ISDClosest, file="ISDClosest.rsav")
save(ISDTempsNearby, file="ISDTempsNearby.rsav")
## Excellent.
## Repeat for State Capitals...
#####################################
##### Repeat for capital cities of NR states
#####################################
## CGS stations matched to nearest ISH station for impact to production
## State capitals matched to nearest ISH station for impact to consumption
options(stringsAsFactors=FALSE)
#Beneficiary<-levels(supplychain$Beneficiary)
Beneficiary<-c("Chandigarh","Delhi","HP","Haryana","JK","Punjab","Rajasthan","Uttarakhand","UP","NR")
capital<-c("Chandigarh", "Delhi", "Shimla", "Chandigarh","Srinagar", "Chandigarh","Jaipur","Dehradun","Luchnow","Delhi")
pop<-c(856900,10400900,150600,856900,948100,856900,2462500,426674,2186000,10400900)
lat<-c(30.750,28.670,31.110,30.750,34.090,30.750,26.920,30.3157,26.8470,28.670)
long<-c(76.780,77.210,77.160,76.780,74.790,76.780,75.800,78.3586,80.9470, 77.210)
states<-data.frame(Beneficiary=Beneficiary, Capital=capital, Pop=pop, Lat=lat, Long=long)
save(states, file="states.rsav")
###################################
## match state capitals to closest weather stations
###################################
# Grab ISD metdata
load("metdata.rsav")
summary(subset(metdata, Country=="IN")[,c("Long", "Lat")])

## Now grab state metadata.  
load("states.rsav")
summary(states[,c("Long",'Lat')])  ## positive! nice.

# findClosestISD function...
knn=5  ## i kept a few to look at, though I end up throwing them out
ISDTempsNearby2 <- dlply(states, 1, findClosestISD, knn=knn )
ISDClosest2 <- ldply(ISDTempsNearby2,
                    function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])

save(ISDClosest2, file="ISDClosest2.rsav")
save(ISDTempsNearby2, file="ISDTempsNearby2.rsav")

# Now grab USAFID from ISDClosest (CGS) and ISDClosest2 (State Capitals)
grab1<-unique(ISDClosest$USAFID)
grab2<-unique(ISDClosest2$USAFID)
grab<-unique(c(grab1, grab2))
length(grab) #number of ISD stations to get

#############################
# Now grab the observed data for these stations...
############################
load("StnData.rsav")
#install.packages('ncdf')
library(ncdf)

filepath<-"/Users/elliotcohen/Dropbox/Data/Climate/Hadley/WMO_400000-499999_2011f/"
files <- list.files(filepath)
length(files)

# 484 stations... subset to just the stations we want (or bring them all in and subset later...)
dummy<-strsplit(files,"\\.")
ext<-laply(dummy, "[[", 7) # file extension...
USAFID<-laply(dummy, "[[", 6)  #ISD code from filename
dummy<-strsplit(USAFID, split="-")
USAFID<-laply(dummy, "[[", 1)   #grab just the 6-digit USAFID from the filenames

wh<-which(USAFID %in% grab)   #12 of 12 USAFID's are found in the files
missing<-which(! grab %in% USAFID)  # None.
grab[missing]  # show which USAFID is missing from the Files
ISDClosest[ISDClosest$USAFID==grab[missing],]  # show the corresponding Stn

# # Retreive missing data... DONE.
# # closest station to SEWA-II is in China... grab second closest instead....
# ISDClosest[ISDClosest$USAFID==grab[missing],2:10]<-ISDTempsNearby$'SEWA-II'[2,]
# save(ISDClosest, file="ISDClosest.rsav")
# # Now re-try above code... Success!

getFiles<-files[wh]
length(getFiles)

# Impliment getData function...
# getData function can be applied inside a for-loop or ddply() to repeat for a set of files 

## Try for just one file...
getData<-function(i, filepath, ext){
  print(i)
  theFile <- paste(filepath, files[i], sep="/")
  if (!file.exists(theFile)) next
  nc<-open.ncdf(theFile)
  print(paste("The file has",nc$nvars,"variables"))
  # look at all the variables in nc...
  names(nc$vars)
  getVar <- function(var) { 
    #whichTimes <- some way of figuring this out for var
    get.var.ncdf( nc, var )#[whTimes]
  }
  theData<-data.frame( temp=getVar('temperatures'), 
                       precip=getVar('precip1_depth') )
}

getData(1, filepath="/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/Hadley", ext="nc")

head(theData)  #NA's in the Precip Column... what's wrong?
head(nc$dim$time$vals)
dim(nc$dim$time$vals)

#######
######### up to here on Dec. 5 2013 at 6:45pm
###########  see getDataNCDF.R (updated Dec. 8 2013.)
##############



###  Define variables for use in the function
filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/raw/"
files<-list.files(filepath)
dummy<-strsplit(files,"\\.")
ext<-laply(dummy, "[[", 2)  # file extension...
ext[1]
  
sheetName<-"Act_Inj_Gen_Stations"
rowIndex=c(3:345)
colClasses=c("character","integer", rep("numeric",96))

### Use the getData function...
  getData<-function(i){
    print(i)
    theFile <- paste(filepath, files[i], sep="/")
    if (!file.exists(theFile)) next
    if(ext[1]=="nc"){
      nc<-open.ncdf(theFile)
      print(paste("The file has",nc$nvars,"variables"))
      # look at all the variables in nc...
      names(nc$var)
      getVar <- function(var) { 
        #whichTimes <- some way of figuring this out for var
        get.var.ncdf( nc, var )#[whTimes]
      }
      theData<-data.frame( temp=getVar('temperatures'), 
                           precip=getVar('precip1_depth') )
    }
    if(ext[1]=="xls"){
        data<-read.xlsx(file=theFile, sheetName=sheetName, header=FALSE, rowIndex=rowIndex, colClasses=colClasses)
        data$V2<-try(as.Date(data$V2-25569, origin="1970-01-01"), silent=TRUE)
        if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
        DF
      }
    if(ext[1]=="csv"){
        data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=rowIndex[1], header=FALSE, check.names=TRUE, comment="#") 
        data$V1<-as.Date(data$V2, format="%Y-%m-%d")  
        if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
        DF
      }
    }  
  

## beware of global assignment for DF.... ask James about this...
for(i in 1:length(files)){
  getData(i)  
}

### for the tricky data...
for(i in 1:length(files)){
  try(getData(i), silent=TRUE)
}










  
  ##' Read the data
widthsData <- c(11,4,4,rep(c(5,1,1,1),12))
ghcnData <- read.fwf(dataFiles[1], widthsData, header=FALSE, comment='@')
whClosest <- which(ghcnData$V1 %in% ghcnClosest$id)
ghcnData <- ghcnData[whClosest,]

outList <- list( ghcnClosest, ghcnData)
names(outList) <- paste0(variable,c("Closest",'Data'))
outList
}

## these could all be rolled in to one 
tavg <- llply( as.list("tavg"), ingestGhcnVariable, download=FALSE)
tmin <- llply( as.list("tmin"), ingestGhcnVariable, download=FALSE)
tmax <- llply( as.list("tmax"), ingestGhcnVariable, download=FALSE)


tmax[[1]][[1]]$id==tmin[[1]][[1]]$id
tmax[[1]][[1]]$id==tavg[[1]][[1]]$id  ##FALSE!
tmax[[1]][[1]]$distance.km>=tavg[[1]][[1]]$distance.km  ##TRUE

## so it looks like we'll only use tavg?
# April 1, 2011 - March 31, 2013.  
tavgData <- tavg[[1]][[2]][,c(1:2, 4*(1:12))]
names(tavgData) <- c('stnId', 'year', 1:12) #month.abb)
tavgMelt <- melt(tavgData, id=c('stnId','year'),
                 variable.name='month', value.name='tavg')
tavgMelt$month <- as.character(tavgMelt$month)
tavgMelt$POSIXct <- ## these are labeled as MDT, but they dont really have a timezone
  as.POSIXct(paste(tavgMelt$year,tavgMelt$month,'15',sep='-'))
tavgGhcn <- subset(tavgMelt, POSIXct >= as.POSIXct('2011-04-1') &
                     POSIXct < as.POSIXct('2013-04-1') )
tavgGhcn$tavg[which(tavgGhcn$tavg==-9999)] <- NA
tavgGhcn$tavg <- tavgGhcn$tavg/100.
summary(tavgGhcn)
table(subset(tavgGhcn, is.na(tavg))$stnId)
save(tavgGhcn, file='tavgGhcn.rsav')
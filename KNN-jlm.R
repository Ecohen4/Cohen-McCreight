###################################################
### Chunk 1: Import libs, files and functions
###################################################
require(plyr)
require(reshape2)
# setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

metdata<-read.fwf(file="http://www.metoffice.gov.uk/hadobs/hadisd/hadisd_station_info.txt",  header=FALSE, widths=c(6,6,31,3,8,9,8), strip.white=TRUE)

names(metdata)<-c("USAFID","WBAN","City","Country","Lat","Long","Elev")
head(metdata)

#cross-reference data availability between Hadley ISD and NOAA ISH..
# IB Stations (station located in Beneficiary State)
grab<-metdata[metdata$USAFID %in% ISHClosest$USAFID,]
identical(length(unique(ISHClosest$USAFID)), dim(grab)[1])
# all available.
# Now grab the observed data for these stations...
install.packages('ncdf')
require(ncdf)
# example
#nc<-open.ncdf('~/Downloads/hadisd.1.0.0.2011f.010010-99999.nc')
#str(nc)

filepath<-"/Users/elliotcohen/Dropbox/Data/Climate/Hadley/"#WMO_400000-499999"
#filepath<-"~/Desktop/Dropbox/Cohen-McCreight/Hadley/"#WMO_400000-499999"
##jlm: i specified pattern b/c there was a non-.nc file in the directory i'm using. 
##jlm: i also returned the full path, as it saves work below.
files <- list.files(filepath, pattern=glob2rx('*nc'), full.names=TRUE)

# use "getData" function..
##jlm: it's somewhat better to "loop" on the set files than than over an index.
getData<-function(fileName){  ## it's somewhat better to 
  #print(fileName)
  if (!file.exists(theFile)) next
    nc<-open.ncdf(theFile)
    print(paste("The file has",nc$nvars,"variables"))
    # look at all the variables in nc...
    for (i in 1:nc$nvars){
      v1 <- nc$var[[i]]
      data1 <- get.var.ncdf( nc, v1 )  #reads ALL the data
      print(paste("Var",i,"has name",v1$name,"and is of shape", dim(data1)))
    }

  # cbind var data into 1 df ease of manipulation...
    #nvars=nc$nvars #if want all the vars...
    nvars=12  #only keep the first 12...
    for (i in 1:nvars){
      v1 <- nc$var[[i]]
      data1 <- get.var.ncdf( nc, v1 )  #reads all the data in var1
      # cbind the data into a data frame
      if(i==1) df<-data1 else df<-cbind(df, data1)
      print(paste("Var",i,"has name",v1$name,"and is of shape", dim(data1)))
    }
  df
}

head(nc$dim$time$vals)

###  Define variables for use in the function
filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/raw/"
.ext=".xls"
files<-list.files(filepath)
sheetName<-"Act_Inj_Gen_Stations"
rowIndex=c(3:345)
#startRow=3
colClasses=c("character","integer", rep("numeric",96))

### Use the getData function...
  getData<-function(i){
    print(i)
    theFile <- paste(filepath, files[i], sep="/")
    if (!file.exists(theFile)) next
    if(.ext==".xls"){
        data<-read.xlsx(file=theFile, sheetName=sheetName, header=FALSE, rowIndex=rowIndex, colClasses=colClasses)
        data$V2<-try(as.Date(data$V2-25569, origin="1970-01-01"), silent=TRUE)
      }
    if(.ext==".csv"){
        data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=rowIndex[1], header=FALSE, check.names=TRUE, comment="#") 
        data$V1<-as.Date(data$V2, format="%Y-%m-%d")  
      }
      if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
      DF
    }  
  
  
## beware of global assignment for DF.... ask James about this...
for(i in 1:length(files)){
  getData(i)  
}

### for the tricky data...
for(i in 1:length(files)){
  try(getData(i), silent=TRUE)
}







# repeat for Stn ISHClosest
# find closest weather station to power station....

load("Stationwise.rsav")
load("CGSmeta.rsav")
load("CGSmeta2.rsav")
load("mergeDF.rsav")
load("Wx_stn_data.rsav") #ISH station data for 136 locations in Northern India
load("dataset.rsav") #ISH station data for 136 locations in Northern India
length(levels(dataset$USAFID)) #136


## match power plants to closes weather stations
# CGS lat-long cords are in CGSmeta2
summary(CGSmeta2[,c("X.DEC",'Y.DEC')])  ## positive! nice.
## summary(ghcnMeta[,c("latitude",'longitude')])
ISHmeta<-monthly
summary(ISHmeta[,c("LONG", "LAT")])

#####################################
##### UP TO HERE ON OCT 29 2013
#####################################

findClosestISH <- function(stn, knn) {
  ## find station name, id, lat,
  llDist <- rdist.earth(matrix(c(stn$X.DEC,stn$Y.DEC), ncol=2),
                        matrix(c(monthly$LONG, monthly$LAT), ncol=2),
                        miles=FALSE, R=6371) ## mean radius in km
  sortDistInds <- sort( llDist, ind=TRUE)$ix
  return( cbind(stnCode=stn$Stn_code,
                distance.km=llDist[sortDistInds[1:knn]],
                ISHMeta[sortDistInds[1:knn],] ) )
}
knn=7  ## i kept a few to look at, though I end up throwing them out
ISHTempsNearby <- dlply( CGSmeta, 1, findClosestISH, knn=knn )
ISHClosest <- ldply(ISHTempsNearby,
                     function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])
##length(unique(ISHClosest$id))


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
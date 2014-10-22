## Purpose get the precip from the GHCN V2 data.
##ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v2/v2.prcp.readme
## The variable names in the files are
## prcp, prcp_adj
## v2.prcp.Z, v2.prcp_adj.Z, v2.prcp.inv

options(warn=1)  ## fields package calculates the distance on the earth assuming
require(fields)  ## a raidus. I think I set to mean radius.
options(warn=2)  ## It's version is newer than my version of R, so fiddle with warnings.
require(plyr)
require(reshape2)
options(stringsAsFactors=FALSE)
setwd('~/Desktop/Dropbox/Cohen-McCreight/GHCN/')
load("CGSmeta.rsav")

ingestGhcnPrecipVariable <- function(variable, downloadData=FALSE) {

  ##' Get the data and unpack it.
  ## These are unix/osx commands at the sytem level (dont attempt on windows).
  if (downloadData) {
    system(paste0("wget ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v2/v2.",variable,".Z"))
    system(paste0("uncompress v2.",variable,".Z"))
    system("wget ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v2/v2.prcp.inv")
  } ## this is less complicated in metadata than the temp data were.
  
  ##' Read the inv (metadata) file.
  ##From the readme: station number (i11), space (1x), station name (a20), country (a10),
  ##                 latitude (f7.2), longitude (f8.2), and elevation in meters (i5)
  widthsMeta <- c(11,1, 20, 10, 7, 8, 5)
  ghcnMeta <- read.fwf('v2.prcp.inv', widthsMeta, header=FALSE, comment='@')
  all.is.na <- function(cc) all(is.na(cc))
  whColsAllNa <- which(unlist(colwise(all.is.na)(ghcnMeta)))
  ghcnMeta <- ghcnMeta[,-whColsAllNa]
  names(ghcnMeta) <- c('id','name','country','latitude','longitude','elev')
  ##summary(ghcnMeta) ## hey they have precip from the south pole!
  
  ##' now match to closest power plants  
  ##summary(CGSmeta[,c("X.DEC",'Y.DEC')])  ## positive! nice.

  findClosestGHCN <- function(stn, nnn) {
    ## find station name, id, lat,
    llDist <- rdist.earth(matrix(c(stn$X.DEC,stn$Y.DEC), ncol=2),
                          matrix(c(ghcnMeta$longitude,ghcnMeta$latitude), ncol=2),
                          miles=FALSE, R=6371) ## mean radius in km
    sortDistInds <- sort( llDist, ind=TRUE)$ix
    return( cbind(stnCode=stn$Stn_code,
                  distance.km=llDist[sortDistInds[1:nnn]],
                  ghcnMeta[sortDistInds[1:nnn],] ) )
  }
  nnn=7  ## i kept a few to look at, though I end up throwing them out
  ghcnTempsNearby <- dlply( CGSmeta, 1, findClosestGHCN, nnn=nnn )
  ghcnClosest <- ldply(ghcnTempsNearby,
                       function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])
  ##length(unique(ghcnClosest$id))
  

  ##' Read the data
  widthsData <- c(11,1,4, rep(5,12))
  ghcnData <- read.fwf(paste0("v2.",variable), widthsData, header=FALSE, comment='@')
  str(ghcnData)
  stop() #max(ghcnData[,3])
  whClosest <- which(ghcnData$V1 %in% ghcnClosest$id)
  ghcnData <- ghcnData[whClosest,]

  outList <- list( ghcnClosest, ghcnData)
  names(outList) <- paste0(variable,c("Closest",'Data'))
  outList
}

## these could all be rolled in to one 
prcp <- llply( as.list("prcp"), ingestGhcnPrecipVariable, download=FALSE)
prcpAdj <- llply( as.list("prcp_adj"), ingestGhcnPrecipVariable, download=FALSE)



## so it looks like none of our stations are in the adj file! damn. ok.
# April 1, 2011 - March 31, 2013.  
prcpData <- prcp[[1]][[2]]
names(prcpData) <- c('stnId','noDuplicates','year', 1:12) #month.abb)
prcpMelt <- melt(prcpData, id=c('stnId','year','noDuplicates'),
                 variable.name='month', value.name='prcp')
## there are noDuplicates at all
prcpMelt$noDuplicates <- NULL
prcpMelt$month <- as.character(prcpMelt$month)
prcpMelt$POSIXct <- ## these are labeled as MDT, but they dont really have a timezone
  as.POSIXct(paste(prcpMelt$year,prcpMelt$month,'15',sep='-'))
prcpGhcn <- subset(prcpMelt, POSIXct >= as.POSIXct('2011-04-1') &
                             POSIXct < as.POSIXct('2013-04-1') )
prcpGhcn$prcp[which(prcpGhcn$prcp==-9999)] <- NA
prcpGhcn$prcp <- prcpGhcn$prcp/10. ## mm
summary(prcpGhcn)
table(subset(prcpGhcn, is.na(prcp))$stnId)
save(prcpGhcn, file='prcpGhcn.rsav')


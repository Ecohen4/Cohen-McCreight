## Purpose get tadj, tmin, tmax from the GHCN V3 data
## ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README


options(warn=1)  ## fields package calculates the distance on the earth assuming
require(fields)  ## a raidus. I think I set to mean radius.
options(warn=2)  ## It's version is newer than my version of R, so fiddle with warnings.
require(plyr)
require(reshape2)
options(stringsAsFactors=FALSE)
setwd('~/Desktop/Dropbox/Cohen-McCreight/GHCN/')
load("CGSmeta.rsav")

ingestGhcnVariable <- function(variable, downloadData=FALSE) {

  ##' Get the data and unpack it.
  ## These are unix/osx commands at the sytem level (dont attempt on windows).
  if (downloadData) {
    system(paste0("wget ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/ghcnm.",variable,
                  ".latest.qca.tar.gz"))
    ## This line will not work with the download when the version changes.
    dataFiles <- system(paste0('tar xvfz ghcnm.',variable,'.latest.qca.tar.gz'), int=TRUE)
  } else
  dataFiles <- list.files(path='ghcnm.v3.2.2.20130927',
                          pattern=glob2rx(paste0('ghcnm.',variable,'.v3.2.2.20130927.qca.*')),
                          full.names=TRUE)
  
  ##' Read the file.
  ## this is a total shit show. some times there are spaces between fields and sometimes not.
  ## this is so from the 70's when you had to code this shit by hand.
  start <- c(1,12,13,21,22,31,32,38,39,69,70,74,75,80,82,84,86,88,89,91,107)
  end <-  c(11,12,20,21,30,31,37,38,68,69,73,74,79,81,83,85,87,88,90,106,107)
  widthsMeta <- end-start+1
  ## (this took me about 5 mins to figure out why it was dying at line 3384
  ## the ... arguments which default to those in read.table() include # as the comment
  ## character which ends the line begin read.)
  ghcnMeta <- read.fwf(dataFiles[2], widthsMeta, header=FALSE, comment='@')
  all.is.na <- function(cc) all(is.na(cc))
  whColsAllNa <- which(unlist(colwise(all.is.na)(ghcnMeta)))
  ghcnMeta <- ghcnMeta[,-whColsAllNa]
  ## gawd, now I find that their column specifications are wrong. Col 75 is not
  ## a space, it belongs with 76-79.
  names(ghcnMeta) <-
    tolower(c('ID','LATITUDE','LONGITUDE','STNELEV','NAME','GRELEV','POPCLS','POPSIZ',
              'TOPO','STVEG','STLOC','OCNDIS','AIRSTN','TOWNDIS','GRVEG','POPCSS'))
  
  ##' now match to closest power plants  
  ##summary(CGSmeta[,c("X.DEC",'Y.DEC')])  ## positive! nice.
  ##summary(ghcnMeta[,c("latitude",'longitude')])

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





                   

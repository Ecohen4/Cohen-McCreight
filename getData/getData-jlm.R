#library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

##jlm: why combine the reads for various different file types into a single function?
## maybe there are good reasons, but i'm skeptical. if you do want to combine these, it's
## better to write individual functions for each file type and after that, write a
## "wrapper" function who's job is to call these based on the type of input file. See
## how that's similar but different? This second way is the "right" way, it's
## essentially  how "methods" work in R. The method is the function to be called based
## on some input. So I'm going to break these up and focus on NCDF. 
## principle: functions should be as simple as possible. (which means writing lots of 
## functions).
## You also have to learn what belongs inside/outside a function. This is an art
## which is only learned through practice.

##jlm:  define the ncdf files
whoami<-system('whoami',int=TRUE)
filepath<- if(whoami!='james') ## since we have different paths.
    "/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/Hadley" else 
    "~/Desktop/Dropbox/Cohen-McCreight/Hadley"
##jlm: i specified pattern b/c there was a non-.nc file in the directory i'm using. 
##jlm: i also returned the full path, as it saves work below.
files <- list.files(filepath, pattern=glob2rx('*nc'), full.names=TRUE)
##This is an essential step for using plyr. This is you book keeping.
names(files)<-laply( strsplit(files, '[./-]'), '[', 14) 

##jlm: define the variables of interest
## the names serve the same plyr booking purpose as above.
vars2Get <- c(temp='temperatures',precip='precip1_depth')

##jlm: define the dates/times of interest.
firstRecord<-as.POSIXct('2011-03-01', tz='UTC')

##jlm: best to write out in english what you're trying to do.
## getData, purpose: open a ncdf file of station data and take out the relevant
## variables, subsetting to the desired times. 
getDataNcdf<-function(theFile){
  ## since we got these from list.files, this should not be necessary.
  ## if you were writing a very general purpose function, you'd test the 
  ## file existence.... 
  ##if (!file.exists(theFile)) next    
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
  #sadly an ldply dosent work here, as described in ldply. so convert a list 
  #to a data fame
  ## also note that a plyr command here means you can simply modify  the 
  ## vars2Get and that is it!
  out<-as.data.frame(llply(vars2Get, getVar)) 
  out$POSIXct<-POSIXct
  out
}

stnData<-ldply(files, getDataNcdf)
##booyah! if you had an abscissa, you could easily plot the individual stations
## as different colors/panels in ggplot!



#########################
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

## Test.
getData(1, filepath="/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/Hadley", ext="nc")



################################################
### Use code below to impliment getData function
#################################################
###  Define variables for use in the function
# filepath<-"/Users/elliotcohen/Dropbox/Data/Climate/Hadley"
# files<-list.files(filepath)
# ## Use the getData function...
# ## beware of global assignment for DF.. ask James about this
# for(i in 1:length(files)){
#   getData(i, filepath=filepath, ext="nc")  
# }
# 
# ### for the tricky data...
# for(i in 1:length(files)){
#   try(getData(i), silent=TRUE)
# }
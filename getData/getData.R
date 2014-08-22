library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

getData<-function(i, filepath, ext){
  print(i)
  files<-list.files(filepath)
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

## Test.
# getData(1, filepath="/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/Hadley", ext="nc")


################################################
### Use code below to impliment getData function
#################################################
# ## plyr version....
# getData(files, filepath=filepath, ext=ext)
# 
# # Loop version....
# ## Define variables for use in the function
# filepath<-"/Users/elliotcohen/Dropbox/Data/Climate/Hadley"
# files<-list.files(filepath)
# ## Use getData function...
# ## beware of global assignment for DF...
# for(i in 1:length(files)){
#   getData(i, filepath=filepath, ext="nc")  
# }
# 
# ### for the tricky data...
# for(i in 1:length(files)){
#   try(getData(i), silent=TRUE)
# }
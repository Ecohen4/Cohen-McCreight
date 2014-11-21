###################################################
### Coal Supply to Thermal Power Stations
###################################################
#setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight")
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
library(plyr)
library(reshape2)
library(ggplot2)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

###################################################
### Create names and structures for incoming data
###################################################
files<-list.files("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/"); files

dummy<-unlist(strsplit(files, "[.]"))
dummy1<-matrix(dummy, nrow=65, byrow=TRUE)
uniqueMons<-dummy1[,1]

### Use this...
ChronMons<-uniqueMons[order(as.Date(paste(15,uniqueMons, sep=""), format="%d%b%y"))]

### Other handy date manipulations...
# Date<-rep(NA, length(uniqueMons))
# for(i in 1:length(uniqueMons)){
#   Date[i]<-as.Date(paste(15,uniqueMons[i], sep=""), format="%d%b%y")
# }
# ChronDate<-Date[order(as.Date(Date, format="%d/%m/%Y"))]
# #dates<-seq(as.Date("2008-03-15"), as.Date("2013-07-15"), by="months")

## Define variables for getData
nastrings<-c("CENTRAL ELECTRICITY AUTHORITY","OPERATION MONITORING DIVISION","DAILY COAL REPORT","Page 1 of3","Page 2 of3", "Page 3 of3", "REGION/", "STATE","POWER","STATION","DAYS","NORTHERN","WESTERN","SOUTHERN","EASTERN","ALL INDIA TOTAL")

theColNames<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
theColNames2<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Import_Stock_MT","Indig_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")
theColNames3<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Import_Stock_MT","Indig_Stock_MT","Total_Stock_MT","Act_Stock_Days","SevenDayStock","FourDayStock","Reason")

length(theColNames)
length(theColNames2)
length(theColNames3)

###################################################
### Import the data files in THREE groups (Mar08-Feb12);  (Mar12-Feb13), and (Mar13-July13) --> ChronMons[1:48], ChronMons[49:60], ChronMons[61:65]
###################################################
## Import and compile first set of .csv's (Mar08-Feb12)
for(i in 1:length(ChronMons[1:48])){ 
  #getData <- function(i) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",ChronMons[i],".csv", sep="")
  if (!file.exists(theFile)) next
  data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=14, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames)
  #if (dim(data)[2]>11) data[,12] <- NULL
  data <- subset(data, ! is.na(as.numeric(data$Stn_num)) ) #remove all rows that are not associated with a station number
  data<-droplevels(data)
  data$ChronMons<-ChronMons[i]
  data$Date<-as.Date(paste(15,ChronMons[i], sep=""), format="%d%b%y")
  
  if(i==1) {DF <- data} else {DF <- rbind(DF,data)}
  str(data)
}
str(DF)

### Visually inspect data
DF[1:100,]
DF<-subset(DF, select=c(1:8,13))
dim(DF)  #40,894
DF$Transport<-as.factor(DF$Transport)
DF$Stn_num<-as.integer(DF$Stn_num)
DF$Stn_name<-as.factor(DF$Stn_name)

table(DF$Stn_num) # all nums, but some have few observations...

levels(DF$Stn_name)
length(levels(DF$Stn_name)) #186 --> mostly Stn_names, but a few erroneous...
table(DF$Stn_name) ## get rid of erroneous stations....
DF$Stn_name<-as.character(DF$Stn_name)

# If Stn_name can be coerced to a numeric, then is.na=FALSE.
# If Stn_name cannot be coerced to numeric (e.g. a valid Stn_name), then is.na=TRUE
test <- subset(DF, is.na(as.numeric(DF$Stn_name)) ) #remove all rows that are not associated with a station number
table(test$Stn_name)  # this works well but doesn't get the "" blank Stn_names..
test2<-subset(test, Stn_name != "")
table(test2$Stn_name)  # Excellent!
test2<-droplevels(test2)
DF<-test2

levels(DF$Transport) # looks good!

# check for NAs --> NONE
sum(is.na(DF)) 
look<-which(is.na(DF[,]), arr.ind=TRUE)
DF[look[,1],]

# Check for complete.cases..
test<-DF[complete.cases(DF[,]),]
if(identical(test,DF)) {print("No Missing data")} else
  {print("Missing data--Check")}
dim(DF)[1]-dim(test)[1]
DF<-test # re-assign DF to the complete cases only

# Double check for NAs...
sum(is.na(DF)) #0

## GOOD UP TO HERE ON NOV 11, 2013...

## quality control: check number of observations at each station.
## Discard stations with less than 50 observations (on avg., ~1 per month)
nstns<-length(levels(DF$Stn_name))
nmons<-length(seq(range(DF$Date)[1],range(DF$Date)[2], by="months")); nmons
max(table(DF$Stn_name)) # max number of observations for one station=845

DF$Stn_name<-as.factor(DF$Stn_name)  # convert Stn_name back to factor...
stns<-levels(DF$Stn_name)
nstns<-length(stns)
nstns #178

stns<-as.data.frame(stns)
stns$keep<-rep(0,nstns)  # empty vector to be populated in for loop...
for (i in 1:nstns){
  if (table(DF$Stn_name)[i]<50) {stns$keep[i]<-"No"} else {stns$keep[i]<-"Yes"}
}

keep<-subset(stns, keep=="Yes") #142 of 178 stations

#now subset "DF" to keep only the observations associated with a station that we want to keep
keep.data<-DF[DF$Stn_name %in% keep$stns,] 
test<-subset(DF, Stn_name %in% keep$stns)
identical(keep.data, test) #true
DF<-keep.data
DF<-droplevels(DF)
table(DF$Stn_name)

# data looks good... 
# Now do manipulations/computations with compiled data... 
# first need to convert characters to numeric....
DF$Capacity<-as.integer(DF$Capacity)
DF$Norm_Stock_Days<-as.integer(DF$Norm_Stock_Days)
DF$Daily_Req_MT<-as.integer(DF$Daily_Req_MT)
DF$Act_Stock_MT<-as.integer(DF$Act_Stock_MT)
DF$Act_Stock_Days<-as.integer(DF$Act_Stock_Days)
sum(is.na(DF$Capacity))        #0
sum(is.na(DF$Norm_Stock_Days)) #0 
sum(is.na(DF$Daily_Req_MT))    #0
sum(is.na(DF$Act_Stock_MT))    #0
sum(is.na(DF$Act_Stock_Days))  #34

look<-which(is.na(DF[,]), arr.ind=TRUE)
look
DF[look[,1],]
# only 34 of 39,958 observational recrods have NA's --> omit them!
DF<-na.omit(DF)
save(DF, file="DF.rsav")

n<-dim(DF)[1]
for (j in 1:n){
  if(DF$Act_Stock_Days[j]<7) {DF$Critical[j]=1} else 
    (DF$Critical[j]=0)
}
save(DF, file="DF.rsav")

## check for NAs
which(is.na(DF), arr.ind=TRUE)
look<-which(is.na(DF), arr.ind=TRUE)
DF[look[,1],]

str(DF)  #39,924 observations of 10 variables
Coal1<-DF
save(Coal1, file="CoalSupply_Mar08-Feb12.rsav")
save(Coal1, file="Coal1.rsav")

# look at trend over time
# are there more coal supply shortages now than in previous years?
###################################################
#### Aggregate from daily to monthly...
##################################################
# number of days per month with critical supply
# one value for each TPS for each month
m.sum<-aggregate(Critical ~ Stn_name + Date, data=DF, FUN=sum)
stns<-levels(m.sum$Stn_name)

# # now subset to CGS in supply chain...
# subset(m.sum, m.sum$Stn_name %in% CGSstns)
# #doesn't well b/c not exact matches...
# 
# # use fuzzy logic instead with agrep
# m.sum$CGS_name<-NA  #create an attribute column for matching CGS name (if applicable)
# n<-dim(m.sum)[1]
# for(i in 1:n){
#   index<-agrep(pattern=m.sum$Stn_name[i], x=CGSstns, ignore.case=TRUE)[1]
#   m.sum$CGS_name[i]<-CGSstns[index]
# }
# test<-na.omit(m.sum)
# dim(m.sum)
# dim(test)
# test<-subset(test, Stn_name!="OBRA")
# test<-subset(test, Stn_name!="KOTA")
# test<-subset(test, Stn_name!="TANDA")
# test<-subset(test, Stn_name!="UKAI")
# test<-droplevels(test)
# table(test$Stn_name)
# table(test$CGS_name)
# 
# ## cbind this to IEX df to get monthly fuel supply shortages at stations
# ## will want to use merge such that zeros are added for other stations....
# test$CGS_name<-as.factor(test$CGS_name)
# length(levels(test$CGS_name)) # 6

# Total number of TPS days with critical supply
total.m.sum<-aggregate(Critical ~ Date, data=DF, FUN=sum)
plot(total.m.sum$Date, total.m.sum$Critical)
ggplot(total.m.sum, aes(x=Date, y=Critical)) + geom_line() + 
  scale_y_continuous(name='TPS days at Critical Supply') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y")) + labs(title="Coal Shortages") 

#add a year attribute just for fun
dummy<-strsplit(as.character(DF$Date), split="-")
DF$Yr<-laply(dummy, '[[', 1)

# aggregate multiple records in a given month.  
DF$Stn_num<-as.factor(DF$Stn_num)
MonSum<-ddply(DF, .(Stn_name, Date, Transport, Capacity, Yr), numcolwise(mean))
MonSum[,6:9]<-round(MonSum[,6:9], digits=2)
head(MonSum)

############## KEY FIGURE ###############
p<-ggplot(MonSum, aes(x=Date, y=Act_Stock_Days, group=Date)) 
p + geom_boxplot() + scale_y_continuous(name='Coal Stock [days]') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y")) + labs(title="Coal Stock for all-India TPS") + geom_abline(intercept=7, slope=0, colour="red")

# geom_smooth(MonSum, aes(group=Yr), method="lm")
# abline(a=7,b=0, col="red") #doesn't work w. ggplot2()
############## KEY FIGURE ###############

ggplot(DF, aes(x=Date, y=Act_Stock_Days)) + geom_point()
ggplot(DF, aes(x=Date, y=Act_Stock_MT)) + geom_point()
ggplot(DF, aes(x=Date, y=Act_Stock_Days, group=Stn_name, colour=Stn_name)) + geom_point()

##################################################
####### Repeat for DF2 (Apr2012 - Apr2013)
##################################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
library(plyr)
library(reshape2)
library(ggplot2)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

for (i in 50:61) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",ChronMons[i],".csv", sep="")
  if (!file.exists(theFile)) next
  data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=14, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames2)
  data <- subset(data, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
  data<-droplevels(data)
  data$Date<-as.Date(paste(15,ChronMons[i], sep=""), format="%d%b%y")
  
  if(i==50) {DF2 <- data} else {DF2 <- rbind(DF2,data)}
  str(data)
}

str(DF2) 
save(DF2, file="DF2raw.rsav")
load("DF2raw.rsav")


#############################
## after days of troubleshooting (see code below...) figured out that the format changed again on March 2013!  
###############################
### Check for complete.cases..
test<-DF2[complete.cases(DF2[,]),]
if(identical(dim(test),dim(DF2))) {print("No Missing data")} else
{print("Missing data--Check")}

### convert columns to a useful class
DF2$Stn_num<-as.integer(DF2$Stn_num)
DF2$Transport<-as.factor(DF2$Transport)
DF2$Stn_name<-as.factor(DF2$Stn_name)

### Drop un-needed data
DF3<-DF2[order(DF2$Stn_num),]  #order by stn_num (more reliable than Stn_name)
DF3<-subset(DF3, select=c(1:9,13)) #drop redundant columns
DF2<-DF3 # assign back to DF2

### Check for complete.cases..
test<-DF2[complete.cases(DF2[,]),]
if(identical(dim(test),dim(DF2))) {print("No Missing data")} else
{print("Missing data--Check")}

## check for NAs
which(is.na(DF2), arr.ind=TRUE)  #none
look<-which(is.na(DF2), arr.ind=TRUE)
head(DF2[look[,1],])

### Visually inspect data
DF2$Stn_num<-as.factor(DF2$Stn_num)
table(DF2$Stn_num) #all Stn_num's, but some have few obs
table(DF2$Stn_name) # Capacity values mixed in with Stn_names
table(DF2$Transport) # Stn_name mixed in with Transport

length(levels(DF2$Stn_num)) #nStns=100
length(levels(DF2$Stn_name)) #nStns=197-->Capacity values mixed in the Stn_name column
levels(DF2$Transport) # Stn_name values mixed in the Transport column.
## everything in those rows would be shifted over one column to the left...tried to un-muddle the "drop" data (see commented-out chunk below) but taking too long for just 2% of the observations... use Stn_num to re-assign proper Stn_name..

## find rows with invalid Stn_name (e.g. Stn_name can be coerced to as.numeric())
## Create a vector of Logical values
## as.numeric() for a character string Stn_name will return NA.  is.na() will return TRUE
## as.numeric() for a character string Stn_name that can be coerced to numeric will return a numeric (non-NA) value.  is.na() will return FALSE
test<-DF2
test$Stn_name<-as.character(test$Stn_name) # MUST be as.character for the is.na(as.numeric()) trick to work....
test$keep<-0

for(i in 1:length(test$Stn_name)){
  test$keep[i]<-is.na(as.numeric(test$Stn_name[i]))
}

keep<-subset(test, keep==1)
dim(keep) #13,284
dim(DF2) #13,579

# Below I try to reconcile rows with data in the wrong columns, but it's taking too long!! 
# drop muddled rows instead....
############ SKIP  ###############
# drop<-subset(test, keep==0)
# dim(drop)
# drop
# 
# # shift numeric values over to correct column
# drop[,4:6]<-drop[,3:5]
# 
# # # # can try to strpslit and re-align data... but taking too long... use na.omit!
# # dummy<-strsplit(as.character(drop$Transport), split=" ")
# # drop$Stn_name<-laply(dummy, '[[', 2)
# # 
# # dummy<-strsplit(as.character(drop$Transport), split=" ")
# # drop$Stn_name<-laply(dummy, '[[', 2)
# 
# for (j in 1:dim(drop)[1]){
#   if(drop$Stn_num==14){drop$Stn_name="ANPARA TPS"} else
#     if(drop$Stn_num==20){drop$Stn_name="RIHAND STPS"} else
#       if(drop$Stn_num==21){drop$Stn_name="SINGRAULI STPS"} else
#         if(drop$Stn_num==25){drop$Stn_name="ANPARA C TPS"} else
#           if(drop$Stn_num==27){drop$Stn_name="KORBA-II"} else
#             if(drop$Stn_num==28){drop$Stn_name="KORBA-WEST TPS"} else
#               if(drop$Stn_num==29){drop$Stn_name="KORBA STPS"} else
#                 if(drop$Stn_num==30){drop$Stn_name="SIPAT STPS"} else
#                   if(drop$Stn_num==38){drop$Stn_name="AMARKANTAK EXT TPS"} else
#                     if(drop$Stn_num==41){drop$Stn_name="VINDHYACHAL STPS"} else
#                       if(drop$Stn_num==43){drop$Stn_name="BHUSAWAL TPS"} else
#                         if(drop$Stn_num==44){drop$Stn_name="CHANDRAPUR(MAHARASHT"} else
#                           if(drop$Stn_num==45){drop$Stn_name="KHAPARKHEDA TPS"} else
#                             if(drop$Stn_num==47){drop$Stn_name="NASIK TPS"} else
#                               if(drop$Stn_num==48){drop$Stn_name="PARLI TPS"} else
#                                 if(drop$Stn_num==50){drop$Stn_name="DAHANU TPS"} else 
#                                   .....
#         
#   
# }

# index<-is.na(as.numeric(DF2$Stn_name)) # Vector of Logical values: as.numeric() for a character string Stn_name will return NA.  is.na() will return TRUE
# sum(index) # 0
# keep<-DF2[index,]
# ndrop<-dim(DF2)[1]-dim(keep)[1]; ndrop
# sum(is.na(keep$Act_Stock_Days)) # 0
############ END SKIP ###############
keep<-droplevels(keep)
DF2<-keep
DF2<-droplevels(DF2)

###################################################
#### QAQC....
##################################################
## check for NAs
which(is.na(DF2), arr.ind=TRUE)  #none
look<-which(is.na(DF2), arr.ind=TRUE)
head(DF2[look[,1],])

# Check for complete.cases..
test<-DF2[complete.cases(DF2[,]),]
if(identical(dim(test),dim(DF2))) {print("No Missing data")} else
{print("Missing data--Check")}

table(DF2$Stn_name)

# combine stations with similar names... can also wait to do this until after we compute critical value.... 
dummy<-strsplit(as.character(DF2$Stn_name), split=" ")
DF2$firstname<-laply(dummy, '[[', 1)

# before using ddply, need to convert columns to as.numeric()
# convert class to numeric()
DF2$Capacity<-as.integer(DF2$Capacity)               #NAs introduced by coercion
DF2$Norm_Stock_Days<-as.numeric(DF2$Norm_Stock_Days) #NAs introduced by coercion
DF2$Daily_Req_MT<-as.numeric(DF2$Daily_Req_MT)       #NAs introduced by coercion
DF2$Import_Stock_MT<-as.numeric(DF2$Import_Stock_MT) #NAs introduced by coercion
DF2$Indig_Stock_MT<-as.numeric(DF2$Indig_Stock_MT)  #NAs introduced by coercion
DF2$Act_Stock_Days<-as.numeric(DF2$Act_Stock_Days)  #NAs introduced by coercion

## where are the NAs?
sum(is.na(DF2$Capacity)) #49
sum(is.na(DF2$Norm_Stock_Days)) #49
sum(is.na(DF2$Daily_Req_MT)) #64
sum(is.na(DF2$Import_Stock_MT)) #1125
sum(is.na(DF2$Indig_Stock_MT)) #357
sum(is.na(DF2$Act_Stock_Days)) # 282
sum(is.na(DF2[,4:9]))  # 1,926 total NA's out of 13,310 observations...
dim(DF2)[1]-sum(is.na(DF2[,4:9])) #11,284 non-NA entries (includes some double counting)

which(is.na(DF2), arr.ind=TRUE)
look<-which(is.na(DF2), arr.ind=TRUE)
head(DF2[look[,1],1:9])

test<-na.omit(DF2) # actually retain 11,847 b.c some NA's in same row....
which(is.na(test), arr.ind=TRUE)  #position of NA's (in any) --> None.
DF2<-test
dim(DF2) # 11,698  x  12

# combine stations with similar names and aggregate from quasi-daily to monthly-mean
test<-ddply(DF2, .(firstname, Stn_num, Transport, Date), numcolwise(mean)) #1710 obs of 11 vars
test2<-na.omit(test) # 1710 obs of 10 vars
test2[,9:10]<-round(test2[,9:10], digits=2)
test2<-test2[,-11]  # drop the "keep" variable

# check values....
test2$Total_Stock_MT = test2$Import_Stock_MT + test2$Indig_Stock_MT
test2$Total_Stock_MT<-round(test2$Total_Stock_MT, digits=2)
test2$test<-round(test2$Total_Stock_MT/test2$Daily_Req_MT, digits=2)

# visually inspect..
cbind(test2$Act_Stock_Days, test2$test) # lots of problem data....

# For which rows does calculated and reported Act_Stock_Days match to the nearest tens place (e.g. 24-->20 and 26-->30)
ok<-which(round(test2$Act_Stock_Days, digits=-1)==round(test2$test, digits=-1))
keep<-test2[ok,]
head(keep)
dim(keep) #833 x 12

drop<-test2[-ok,]
head(drop)
dim(drop)  #877 x 12
drop$diff<- drop$Act_Stock_Days - drop$test
drop2<-drop[order(drop$diff),]
drop2[1:100,]

# check<-0
# for(i in 1:dim(test2)[1]){
#   check[i]<-identical(round(drop2$test[i], digits=-1), round(drop2$Act_Stock_Days[i], digits=-1))  
# }
drop<-test2[check,]
dim(drop) #877 x 12
head(drop)

dim(drop)[1]+dim(keep)[1]  #1710
dim(test2)[1]              #1710

# what should we beleive?  calculated or reported Act_Stock_Days ??
# cross-reference with original PDFs...
drop2<-drop[order(drop$Date),]

# estimated and reported values are very close... 




# ## alternatively...
# ## use set of names contained in DF (less messy than DF2...)
# stns<-levels(DF$Stn_name)
# length(stns)
# dummy<-strsplit(as.character(stns), split=" ")
# firstnames<-laply(dummy, '[[', 1)
# 
# # use fuzzy logic with agrep
# DF2<-subset(DF2, Stn_name !="")
# DF2$sharedname<-NA  #create an attribute column for matching CGS name (if applicable)
# n<-dim(DF2)[1]
# for(i in 1:n){
#   index<-agrep(pattern=DF2$Stn_name[i], x=stns, ignore.case=TRUE)[1]
#   DF2$firstname[i]<-firstnames[index]
# }

# ## quality control: check number of observations at each station.
# ## Discard stations with less than 50 observations...
# DF2$Stn_name<-as.factor(DF2$Stn_name)
# nstns<-length(levels(DF2$Stn_name))
# stns<-levels(DF2$Stn_name)
# stns<-as.data.frame(stns)
# stns$keep<-rep(0,nstns)  # empty vector to be populated in for loop...
# for (i in 1:nstns){
#   if (table(DF2$Stn_name)[i]<23) {stns$keep[i]<-"No"} else {stns$keep[i]<-"Yes"}
# }
# 
# keep<-subset(stns, keep=="Yes")
# 
# #now subset "DF2" to keep only the observations associated with a station that we want to keep
# keep.data<-DF2[DF2$Stn_name %in% keep$stns,] 
# test<-subset(DF2, Stn_name %in% keep$stns)
# identical(keep.data, test) #true
# DF2<-keep.data
# DF2<-droplevels(DF2)
# table(DF2$Stn_name)
# 
# 
# 
# 
# str(DF2)
# save(DF2, file="Coal2.rsav")
# load("Coal2.rsav")


########################################
## check if numeric columns are okay...
########################################
# range(DF2$Norm_Stock_Days)
# #remove rows that are *not* associated with a numeric value
# index<-! is.na(as.numeric(DF2$Norm_Stock_Days))
# keep<-DF2[index,]
# keep<-droplevels(keep)
#       ndrop<-dim(DF2)[1]-dim(keep)[1]
#       sum(is.na(keep$Norm_Stock_Days)) # 0
#       DF2<-keep
#       
# # repeat for next numeric column
# range(DF2$Daily_Req_MT)
# #remove rows that are *not* associated with a numeric value
# index<-! is.na(as.numeric(DF2$Daily_Req_MT))
# keep<-DF2[index,]
# keep<-droplevels(keep)
# ndrop<-dim(DF2)[1]-dim(keep)[1]; ndrop
# sum(is.na(keep$Daily_Req_MT)) # 0
# DF2<-keep
#       
# # repeat for next numeric column
# range(DF2$Import_Stock_MT)
# #remove rows that are *not* associated with a numeric value
# index<-! is.na(as.numeric(DF2$Import_Stock_MT))
# keep<-DF2[index,]
# ndrop<-dim(DF2)[1]-dim(keep)[1]; ndrop 
# 
# # lots of NAs... let's take a closer look
# which(is.na(as.numeric(DF2$Import_Stock_MT)))  #
# sum(is.na(as.numeric(DF2$Import_Stock_MT))) # 1074
# 
# look<-which(is.na(as.numeric(DF2$Import_Stock_MT)))
# DF2[look,] # two entries stuffed into Import_Stock_MT column... but not necessarily a problem b/c other columns in those rows still look good...
# # Don't drop for now....
# 
# # repeat for next numeric column
# range(DF2$Indig_Stock_MT)
# DF2$Indig_Stock_MT # from visual inspection we see that some columns are doubled up....
# 
# #remove rows that are *not* associated with a numeric value
# index<-! is.na(as.numeric(DF2$Indig_Stock_MT))
# keep<-DF2[index,]
# ndrop<-dim(DF2)[1]-dim(keep)[1]; ndrop
# sum(is.na(keep$Indig_Stock_MT)) # 0
# DF2<-keep
# 
# 
# #repeat for next numeric column
# range(DF2$Act_Stock_Days)
# #remove rows that are *not* associated with a numeric value
# index<-! is.na(as.numeric(DF2$Act_Stock_Days))
# keep<-DF2[index,]
# ndrop<-dim(DF2)[1]-dim(keep)[1]; ndrop
# sum(is.na(keep$Act_Stock_Days)) # 0
# DF2<-keep
# 
# ### check for NAs
# which(is.na(DF2), arr.ind=TRUE)  #none
# look<-which(is.na(DF2), arr.ind=TRUE)
# head(DF2[look[,1],])
# dim(DF2) # 12,769 obs of 10 variables.
# 
# save(DF2, file="DF2clean.rsav")

########## SKIP ##############
# # There are Stn_name's in the Transport column... need to string split Transport column into three $Transport, $ Stn_name_TEMP, $Stn_name_TEMP2
# #if(DF$Transport != "INTER" | "INTER MODAL" | "MODAL" | "RAIL" | "PITHEAD" | "ROAD" | "PITHEA")
# 
# modes<-c("INTER", "INTER MODAL", "MODAL", "RAIL", "PITHEAD", "ROAD", "PITHEA")
# 
# # grab valid transport values
# keep<-DF2[DF2$Transport %in% modes,] 
# try1<-subset(DF2, Transport %in% modes)
# identical(keep, try1) #true
# dim(DF2)[1]-dim(keep)[1] #334 of 14142 observations have muddled Transport column
# 
# # grab invalid transport values
# dropIndex<-! DF2$Transport %in% modes
# drop<-DF2[! DF2$Transport %in% modes, ] #the 334 obs with muddled columns

# drop$Capacity<-as.numeric(drop$Capacity)
# drop$Norm_Stock_Days<-as.numeric(drop$Norm_Stock_Days)
# drop$Daily_Req_MT<-as.numeric(drop$Daily_Req_MT)
# drop$Import_Stock_MT<-as.numeric(drop$Import_Stock_MT)
# drop$Indig_Stock_MT<-as.numeric(drop$Indig_Stock_MT)  #NAs introduced by coercion
# drop$Act_Stock_Days<-as.numeric(drop$Act_Stock_Days)  #NAs introduced by coercion
# 
# ## Now stringsplit those columns and re-organize....
# test<-drop
# test$Transport<-as.character(test$Transport)
# dummy<-strsplit(test$Transport, split=" ")
# # if(dummy[i] %in% modes){ test$Transport[i]<-dummy{i}} else
# #   if(! dummy[i] %in% modes) {test$Transport[i]<-dummy}
# 
# # remove problem entries...
# dummy<-dummy[-12]
# dummy<-dummy[-36]
# 
# # check what mode of transport is contained in the muddled Transport column...
# INTER<-grep(modes[1], dummy)
# INTERMODAL<-grep(modes[2], dummy)
# MODAL<-grep(modes[3], dummy)
# RAIL<-grep(modes[4], dummy)
# PITHEAD<-grep(modes[5], dummy)
# ROAD<-grep(modes[6], dummy)
# PITHEA<-grep(modes[7], dummy)
# keep<-c(INTER, INTERMODAL, MODAL, RAIL, PITHEAD, ROAD, PITHEA)
# keep<-unique(keep)
# length(keep)  # 308 entries containing a meaninful mode value
# length(dummy) # 332 total entries 
# ndrop<-length(dummy)-length(keep); ndrop  # ndrop=24
# drop2<-dummy[-keep]; length(drop2)       #length drop2=24
# # ndrop and length(drop2) match, therefore can discard those entries.
# dummy<-dummy[keep]
# length(dummy)  #308
# 
# dummy1<-laply(dummy, '[[', 1)  #contains mode of Transport
# # assign dummy1 back to the original DF2$Transport...
# dropIndex2<-dropIndex[]
# DF2$Transport[dropIndex]<-dummy1  # did this work??????
# 
# ## not quite complete... don't need anyway.... 
# # dummy2<-laply(dummy, '[[', 2) #contains both modes of Transport and Stn_names
# # # seperate out the ones containing a Stn_name
# # drop3<-dummy2[dummy2 %in% modes] # entries containing a mode of transport
# # drop4<-! dummy2 %in% modes  # entries NOT containg a mode of transport
# # dummy3<-dummy2[drop4]  # pared down dummy2 w.out Transport modes
# # 
# # keep2<-dummy2 %in% levels(DF2$Stn_name) #logical. If contains a full Stn_name
# # keep3<-setdiff(dummy3,keep2)  #rows containing partial Stn_name
# # 
# # dummy4<-dummy3[keep3]  # rows containing partial Stn_name... now check if the remaining part of the name is in the third part of strsplit
# # dummy<-dummy[dummy4]
# # dummy5<-laply(dummy4, '[[', 3)
######## End SKIP  #############



###################################################
#### Computations
##################################################
# IF data looks good... 
# do manipulations/computations with compiled data... 
# # but first, need to convert class to numeric()
# DF2$Capacity<-as.numeric(DF2$Capacity)               #NAs introduced by coercion
# DF2$Norm_Stock_Days<-as.numeric(DF2$Norm_Stock_Days) #NAs introduced by coercion
# DF2$Daily_Req_MT<-as.numeric(DF2$Daily_Req_MT)       #NAs introduced by coercion
# DF2$Import_Stock_MT<-as.numeric(DF2$Import_Stock_MT) #NAs introduced by coercion
# DF2$Indig_Stock_MT<-as.numeric(DF2$Indig_Stock_MT)  #NAs introduced by coercion
# DF2$Act_Stock_Days<-as.numeric(DF2$Act_Stock_Days)  #NAs introduced by coercion

## where are the NAs
sum(is.na(DF2$Capacity)) #123
sum(is.na(DF2$Norm_Stock_Days)) #124
sum(is.na(DF2$Daily_Req_MT)) #257
sum(is.na(DF2$Import_Stock_MT)) #1200
sum(is.na(DF2$Indig_Stock_MT)) #444
sum(is.na(DF2$Act_Stock_Days)) # 502
sum(is.na(DF2[,4:9]))  # 2,650 total NA's out of 13,579 observations...
dim(DF2)[1]-sum(is.na(DF2[,4:9])) #10929 non-NA entries (includes some double counting)

test<-na.omit(DF2) # actually retain 11,847 b.c some NA's in same row....
which(is.na(test), arr.ind=TRUE)  #position of NA's (in any) --> None.
DF2<-test

# now onto the computations!
DF2$Act_Stock_Days<-as.integer(DF2$Act_Stock_Days)
n<-dim(DF2)[1]
for (j in 1:n){
  if(DF2$Act_Stock_Days[j]<7) {DF2$Critical[j]=1} else 
    (DF2$Critical[j]=0)
}

## check for NAs
which(is.na(DF2), arr.ind=TRUE)  #position of NA's (in any) --> None.
look<-which(is.na(DF2), arr.ind=TRUE)
DF2[look[,1],]
DF2$Stn_name<-droplevels(DF2$Stn_name)


## subset to CGS only
stations<-levels(CGSmeta$Stn_name)
stations<-stations[-1]
# coal4cgs<-DF2[DF2$Stn_name %in% stations]  #doesn't work b/c not exact matches...
# use fuzzy logic instead with agrep
DF2$CGS_name<-NA  #create an attribute column for matching CGS name (if applicable)
n<-dim(DF2)[1]
for(i in 1:n){
  index<-agrep(pattern=DF2$Stn_name[i], x=stations, ignore.case=TRUE)[1]
  DF2$CGS_name[i]<-stations[index]
}

# Now drop the non-CGS entries... This is a huge waste of data! omits 96.5% of the data!!!
keep<-na.omit(DF2)
keep<-droplevels(keep)
table(keep$Stn_name)

# ... MAYBE SHOULD ONLY DO THE ANALYSIS FOR DELHI????? WE KNOW BOTH THE CGS ALLOCATIONS AND THE STATE AND PRIVATE GEN STATIONS....

###################################################
#### Aggregate from daily to monthly...
##################################################
# number of days per month with critical supply
monthly<-aggregate(DF2$Critical ~ DF2$Stn_name + DF2$Date, data=DF2, FUN=sum)
levels(monthly$Stn_name)



###################################################
#### Natural Gas fuel supply
##################################################
setwd("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply")
gas<-read.csv("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/gas_all_in_one.csv", header=TRUE)

## Notes: 
## 1. gas in units of MMSCMD (million standard cubic feet per day) (as reported)
## 2. Gen in units of MU (GWh).                   (as reported)
## 3. Gas requirement is at 90% PLF.              (as reported)
## 4. Naptha and HSD use in KL (thousand liters)  (as reported)
## 5. gas.supplied.consumed / gas.requiremnet = effective PAF due to fuel constraints (elliot)

## Visually inspect data...
table(gas$name)  # lots of misspellings and imprecise names --> clean up!

# gas$Stn_name<-NA
# for(i in 1:dim(gas)[1]){
#   #index<-agrep(pattern=gas$name[i], x=gas$name[-i], ignore.case=TRUE)
#   gas$Stn_name[i]<-agrep(pattern=gas$name[i], x=gas$name[-i], ignore.case=TRUE, value=TRUE)[1]
# }  ## this works but it just finds itself at another timeslice...

## use laply instead...
gas$name<-as.character(gas$name)
dummy<-strsplit(gas$name, split=" ")
gas$Stn_name<-laply(dummy, '[[', 1)
gas$Stn_name<-as.factor(gas$Stn_name)

## Aggregate mulitple units at same plant...
stationwise<-ddply(gas, .(year, month, Stn_name, located.in.state), summarize, capacity=sum(capacity), gen=sum(gen), gas.requirement=sum(gas.requirement), gas.allotted=sum(gas.allotted), gas.consumed=sum(gas.supplied.consumed), naptha=sum(naptha), HSD=sum(HSD))

## Create Date attribute
stationwise$DATE<-as.Date(paste(stationwise$year,stationwise$month,"15", sep="-"), format="%Y-%b-%d")
range(stationwise$DATE)

## quality control: check number of observations at each weather station.
## Discard stations with less than 90% of the monthly data
stationwise$Stn_name<-as.factor(stationwise$Stn_name)
nstns<-length(levels(stationwise$Stn_name))
stns<-levels(stationwise$Stn_name)
stns<-as.data.frame(stns)
stns$keep<-rep(0,nstns)  # empty vector to be populated in for loop...
for (i in 1:nstns){
  if (table(stationwise$Stn_name)[i]<23) {stns$keep[i]<-"No"} else {stns$keep[i]<-"Yes"}
}

keep<-subset(stns, keep=="Yes")

#now subset "stationwise" to keep only the observations associated with a station that we want to keep
keep.data<-stationwise[stationwise$Stn_name %in% keep$stns,] 
test<-subset(stationwise, Stn_name %in% keep$stns)
identical(keep.data, test) #true
stationwise<-keep.data
stationwise<-droplevels(stationwise)
table(stationwise$Stn_name)

stationwise$effPAF<-round(stationwise$gas.consumed/stationwise$gas.requirement, digits=3)


###################################################
### Repeat for hydro...
###################################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
#options(error=utils::recover)
options(stringsAsFactors=FALSE)

library(plyr)
library(reshape2)
library(ggplot2)

files<-list.files("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/hydro/"); files

dummy<-unlist(strsplit(files, "[.]"))
dummy1<-matrix(dummy, nrow=65, byrow=TRUE)
uniqueMons<-dummy1[,1]

#dates<-seq(as.Date("2008-03-15"), as.Date("2013-07-15"), by="months")

nastrings<-c("DAILY REPORT OF HYDRO RESERVOIRS FOR","FULL","RESERVOIR","LEVEL","DAY LAST",'YEAR','PRESENT',"STATE","TIAL","LEVEL ON",'THE SAME','F.R.L. PRESENT', 'THE MONTH','MIN.DRAW','DOWN','LEVEL','F.R.L','ANNUAL','DESIGN','POTEN-','ENERGY ENERGY','CONTANT CONTANT','AT',"1ST.OF", "EFF.CAP",'CAP','AT','CUMULATIVE','ENERGY',"PRESENT","GEN.FROM")
colnames<-c("Stn_name","State","FRL.m","FRL.ft","PRL.m","PRL.ft","LYRL.m","LYRL.ft","MinDDL.m","MinDDL.ft","AnnDesign.MU","FRL.MU","PRL.MU","FRL.MCM","FRL.MAFT","PRL.MCM","PRL.MAFT","MonGen.MU")

#### getData 
for (i in 1:length(files)) {
  #getData <- function(i) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/hydro/",files[i],sep="")
  if (!file.exists(theFile)) next
  data<-read.table(file=theFile, sep=",", strip.white=TRUE, col.names=colnames, blank.lines.skip=TRUE, fill=TRUE, skip=10, header=FALSE, check.names=TRUE, comment="#", na.strings=nastrings)
  
#   data<-read.table(file=theFile, sep=",", strip.white=TRUE, col.names=colnames, colClasses=c(rep("factor",2), rep("numeric",16)), blank.lines.skip=TRUE, fill=TRUE, skip=10, header=FALSE, check.names=TRUE, comment="#", na.strings=nastrings)
  
  # QAQC
  dim(data)          #1736 x 18
  test<-na.omit(data)
  dim(test)          #331 x 18
  cc<-complete.cases(test)
  data<-test[cc,]    #331 x 18
  
  ## check for NAs
  print(which(is.na(data), arr.ind=TRUE))  #position of NA's (in any) --> None.
  look<-which(is.na(data), arr.ind=TRUE)
  print(data[look[,1],])
  
  # Continue with data import...
  # data <- subset(data, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
  
  data<-droplevels(data)
  data$uniqueMon<-uniqueMons[i]
  if(i==1) {hydro <- data} else {hydro <- rbind(hydro,data)}
  str(data)
}
str(hydro)


###############################
#save(hydro, file="hydro.rsav")

## "hydro" contains mulitiple entries per month, but not necessarily one for each day... thus take monthly mean of each attribute.

## first convert "factor" to "numeric" to compute mean()
test<-hydro
# test[,3:18]<-as.numeric(test[,3:18])
test$FRL.m<-as.numeric(test$FRL.m)
test$FRL.ft<-as.numeric(test$FRL.ft)
test$PRL.m<-as.numeric(test$PRL.m)
test$PRL.ft<-as.numeric(test$PRL.ft)
# test$LYRL.m<-as.numeric(test$LYRL.m)
# test$LYRL.ft<-as.numeric(test$LYRL.ft)  # Warning: NAs introduced by coercion 
test$MinDDL.m<-as.numeric(test$MinDDL.m)
test$MinDDL.ft<-as.numeric(test$MinDDL.ft)
test$AnnDesign.MU<-as.numeric(test$AnnDesign.MU)
test$FRL.MU<-as.numeric(test$FRL.MU)
test$FRL.MCM<-as.numeric(test$FRL.MCM)
test$FRL.MAFT<-as.numeric(test$FRL.MAFT)
test$PRL.MU<-as.numeric(test$PRL.MU)
test$PRL.MCM<-as.numeric(test$PRL.MCM)
test$PRL.MAFT<-as.numeric(test$PRL.MAFT)
test$MonGen.MU<-as.numeric(test$MonGen.MU)

which(is.na(test), arr.ind=TRUE)  #position of NA's (in any) --> None.
look<-which(is.na(test), arr.ind=TRUE)
test[look[,1],]

#re-assign test back to hydro after converting colClasses
hydro<-test

# now compute monthly mean
# test<-ddply(hydro, .(Stn_name, State, Date), summarise, FRL.m=mean(FRL.m), FRL.ft=mean(FRL.ft),PRL.m=mean(PRL.m),PRL.ft=mean(PRL.ft), ...  )
#is.numeric(hydro[,3:18]) # FALSE
#which(! is.numeric(hydro[,3:18]), arr.ind=TRUE)

##################################
##### up to here on Nov 1, 2013...
##################################
test<-subset(hydro, select=c(19, 1, 2, 3:18))
test2<-ddply(test, .(uniqueMon, Stn_name, State), numcolwise(mean))  #monthly mean status of all hydropower stns in India
test2[,4:17]<-round(test2[,4:17], digits=2)

hydro2<-test2
hydro2$Date<-as.Date(paste(15,hydro2$uniqueMon, sep=""), format="%d%b%y")
save(hydro2, file="hydro2.rsav")

## plot potential energy as a fn of reservoir height
p<-ggplot(test2, aes(x=PRL.m, y=PRL.MU, group=Stn_name, colour=Stn_name)) 
p + geom_point() + facet_wrap(~Stn_name, scale="free")

## plot potential energy as a fn of reservoir volumne
p<-ggplot(test2, aes(x=PRL.MCM, y=PRL.MU, group=Stn_name, colour=Stn_name)) 
p + geom_point() + facet_wrap(~Stn_name, scale="free")

## now compute "critical" value.  e.g. if PRL.PE < (0.2*FRL.PE)
n<-dim(hydro2)[1]
for (j in 1:n){
  if(hydro2$PRL.MU[j]<hydro2$FRL.MU[j]*0.20) {hydro2$Critical[j]=1} else 
    (hydro2$Critical[j]=0)
}

p<-ggplot(hydro2, aes(x=Date, y=PRL.MU, group=Stn_name, colour=Stn_name))
p + geom_line()

# Fit a distribtution ...
# try for one loc first
test<-subset(test2, Stn_name=="ALMATTI")
p1<-ggplot(test, aes(x=PRL.m, y=PRL.MU)) 
p1 + geom_point()


library(fitdistrplus)
par(mfrow=c(1,1))
Y<-hydro2$Critical
plotdist(Y) # interpretation: 
descdist(Y) # interpretation: 


# ###################################################
# ### Repeat for gas... data is too scrambled.... use gas_all_in_one...
# ###################################################
# setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
# #options(error=utils::recover)
# options(stringsAsFactors=FALSE)
# 
# library(plyr)
# library(reshape2)
# library(ggplot2)
# 
# # split data into two groups: before Mar12 and after Mar12....
# files<-list.files("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/gas/"); files
# 
# dummy<-unlist(strsplit(files, "[.]"))
# dummy1<-matrix(dummy, nrow=24, byrow=TRUE)
# uniqueMons<-dummy1[,1]
# 
# dates<-seq(as.Date("2010-04-15"), as.Date("2012-05-15"), by="months")
# 
# nastrings<-c("CENTRAL SECTOR","Sub Total (NR)","Sub Total (WR)", "Subt Total (NER)", "Total (CS)","STATE SECTOR","Sub Total","Name","of","Power Station","(MW)","(MMSCMD)","Sub Total (NER)", "Sub Total (WR)", "Sub Total (SR)", "S. No", "Located","in the","(MUs)","HSD","Naptha","@","*","+","$","#","**","***")
# 
# colnames<-c("S.No","Stn_name","Capacity","State","MonGen.MU","Gas.Req","Gas.Alloted","Gas.Consumed","Alt.Fuel.Naptha","Alt.Fuel.HSD", "GenLoss","Remarks")
# 
# #commentchars<-c(@,*,+,$,#,**,***)
# #### getData 
# for (i in 1:length(files)) {
#   #getData <- function(i) {
#   print(i)
#   theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/gas/",files[i],sep="")
#   if (!file.exists(theFile)) next
#   data<-read.csv(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=8, header=FALSE, check.names=TRUE)
#   
#   #data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=8, header=FALSE, check.names=TRUE, na.strings=nastrings, col.names=colnames)
#   
#   #   data<-read.table(file=theFile, sep=",", strip.white=TRUE, col.names=colnames, colClasses=c(rep("factor",2), rep("numeric",16)), blank.lines.skip=TRUE, fill=TRUE, skip=10, header=FALSE, check.names=TRUE, comment="#", na.strings=nastrings)
#   
#   # QAQC
#   print(dim(data))          
#   #test<-na.omit(data)
#   #print(dim(test))     
#   #test<-data
#   cc<-complete.cases(data)
#   data<-data[cc,]   
#   #data<-droplevels(data)
#   print(dim(data))
#   
#   ## check for NAs
#   print(which(is.na(data), arr.ind=TRUE))  #position of NA's (in any) --> None.
#   look<-which(is.na(data), arr.ind=TRUE)
#   print(data[look[,1],])
#   
#   # Continue with data import...
#   # data <- subset(data, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
#   
#   data<-droplevels(data)
#   data$uniqueMon<-uniqueMons[i]
#   if(i==1) {gas <- data} else {gas <- rbind(gas,data)}
#   str(data)
# }
# str(gas)
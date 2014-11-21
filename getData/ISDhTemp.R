###################################
## ISD Temperature data 
## visualization, QC and preliminary analysis
##################################
library(plyr)     # ddply, ldply, etc...
library(reshape2) # melt()
library(ggplot2)  # ggplot()
library(scales)   # scale_x_date()
library(locfit)   # local polynomial regression 
library(akima)    # interp function
library(fields)   # surface function
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
load("stnData.rsav")  # raw ISD hourly Temp and precip data
load("metdata.rsav")  # ISD station metadata

load("stnTemps.rsav") # hourly temp data for CGS
load("d.stnTemps.rsav") # daily temp data for CGS
load("StateTemps.rsav")  # hourly temp data for Beneficiary States
load("d.StateTemps.rsav") # daily temp data for Beneficiary States

# #how many NA's?
# sum(is.na(stnData$temp))  #8771
# 
# # How many valid temp obs (non-NAs)?
# sum(! is.na(stnData$temp))  #3,801,598
# 
# # fraction of obs that are *not* NA?
# sum(! is.na(stnData$temp))/dim(stnData)[1]  # 99.8% --> Excellent!
# 
# # grab the hourly temp data
# h.temp<-stnData[,-3]
# h.temp<-na.omit(h.temp)
# dim(h.temp)  # 3,801,589 x 4
# hist(h.temp$temp)
# range(h.temp$temp)  # -2.0e+30 to 51
# 
# # get rid of erroneous (impossible) values
# h.temp<-subset(h.temp, temp>-100)
# range(h.temp$temp)
# hist(h.temp$temp)
# hist(h.temp$temp, probability=TRUE, xlim=c(-40,60))
# 
# hist<-hist(h.temp$temp)
# plot(hist, freq=FALSE, labels=FALSE, main="Histogram of Temperatures Recorded Across India\nMar 2011 to Jan 2013", xlim=c(range(h.temp$temp)[1],range(h.temp$temp)[2])*1.1, xlab="Temperature (deg. C)")
# 
# summary(h.temp$temp)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -49.10   14.60   24.00   20.61   28.30   51.00 
# 
# # grab h.temp data for Beneficiary States and CGS 
# # first, merge ISDClosest with h.temp 
# load("ISDClosest.rsav")
# load("ISDClosest2.rsav")
# 
# names(h.temp)[1]<-"USAFID"
# ISDClosest<-ISDClosest[,-5]  #drob WBAN
# stnTemps<-merge(ISDClosest, h.temp, by="USAFID")
# dim(stnTemps)  # 152,023 x 11
# save(stnTemps, file="stnTemps.rsav")
# 
# ## aggregate from hourly to daily
# # create date attribute
# stnTemps$Date<-as.Date(stnTemps$POSIXct)
# range(stnTemps$Date) # 2011-02-01 to 2012-12-31
# 
# # d.stnTemps<-aggregate(temp ~ Date + USAFID, data=stnTemps, sum)
# d.stnTemps<-ddply(stnTemps, .(Stn_code, ID, distance.km, USAFID, City, Country, Lat, Long, Elev, Date), summarize, avgT=mean(temp), maxT=max(temp), minT=min(temp), .progress="time")
# d.stnTemps$avgT<-round(d.stnTemps$avgT, digits=2)
# sum(is.na(d.stnTemps))  #0 NA's.
# dim(d.stnTemps)  # 15,378 x 13
# d.stnTemps<-droplevels(d.stnTemps)
# save(d.stnTemps, file="d.stnTemps.rsav")
#
# ## repeat for Beneficiary States
# ISDClosest2<-ISDClosest2[,-5]  #drob WBAN
# StateTemps<-merge(ISDClosest2, h.temp, by="USAFID")
# dim(StateTemps)  # 71,728 x 11
# sum(is.na(StateTemps))  # 0
# StateTemps$Date<-as.Date(StateTemps$POSIXct)
# save(StateTemps, file="StateTemps.rsav")
# 
# ## aggregate from hourly to daily
# # create date attribute
# StateTemps$Date<-as.Date(StateTemps$POSIXct)
# range(StateTemps$Date) # 2011-02-01 to 2012-12-31
# 
# # d.StateTemps<-aggregate(temp ~ Date + USAFID, data=StateTemps, sum)
# d.StateTemps<-ddply(StateTemps, .(Beneficiary, ID, distance.km, USAFID, Country, Lat, Long, Elev, Date), summarize, avgT=mean(temp), maxT=max(temp), minT=min(temp), .progress="time")
# d.StateTemps$avgT<-round(d.StateTemps$avgT, digits=2)
# sum(is.na(d.StateTemps))  #0 NA's.
# dim(d.StateTemps)  # 6,688 x 12
# d.StateTemps<-droplevels(d.StateTemps)
# save(d.StateTemps, file="d.StateTemps.rsav")

#########################
## plot daily temp data
#########################
load("d.stnTemps.rsav")
levels(d.stnTemps$Stn_code) # look at all the CGS
length(levels(d.stnTemps$Stn_code)) # number of CGS
d.stnTemps$USAFID<-as.factor(d.stnTemps$USAFID)
length(levels(d.stnTemps$USAFID)) # unique ISD stations reporting data (KNN to CGS)
ggplot(d.stnTemps, aes(x=Date, y=maxT, group=Stn_code, colour=USAFID)) + geom_line() + facet_wrap(~Stn_code)# show which states share same data

###################################
## Explore correlation between Temp and stnUI
###################################
## get stnUI data
## 15-minute UI given in Rupee (Rs). UI amount to be paid (+), to be received (-)
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

## Stationwise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
load("StnUI.rsav")  #15-min cost of UI in *Rupee* for CGS
load("d.StnUI.rsav") #daily cost of UI in *Lakh Rupee* (10^5 RS)
range(d.StnUI$Date)   #"2011-04-18" "2013-10-27"
range(d.stnTemps$Date) #"2011-03-01" "2012-12-31"
# Temp data only goes up to Dec. 31 2012.
# StnUI data goes all the way up to Oct. 27 2013

head(d.StnUI)
head(d.stnTemps)
levels(d.StnUI$firstname)
levels(d.stnTemps$Stn_code)

# conform names... create d.stnTemps$firstname
dummy<-strsplit(as.character(d.stnTemps$Stn_code), split=" ") 
d.stnTemps$firstname<-laply(dummy, "[[", 1)
dummy<-strsplit(as.character(d.stnTemps$firstname), split="-") 
d.stnTemps$firstname<-laply(dummy, "[[", 1)
dummy<-strsplit(as.character(d.stnTemps$firstname), split="_") 
d.stnTemps$firstname<-laply(dummy, "[[", 1)
d.stnTemps$firstname<-as.factor(d.stnTemps$firstname)

length(levels(d.stnTemps$firstname))  #20
length(levels(d.StnUI$firstname))  #20
levels(d.stnTemps$firstname)
levels(d.StnUI$firstname)
identical(levels(d.stnTemps$firstname),levels(d.StnUI$firstname))  # TRUE

# merge Temp and UI data for CGS stns...
grabData<-merge(d.stnTemps, d.StnUI, by=c("firstname","Date"))

# add Stn Fueltype
load("CGSmeta.rsav")
CGSmeta$Stn_code<-as.factor(CGSmeta$Stn_code)
levels(grabData$Stn_code)
levels(CGSmeta$Stn_code)
identical(levels(grabData$Stn_code),levels(CGSmeta$Stn_code)) #FALSE

get<-which(grabData$Stn_code=="NATHPA JHAKRI")
grabData$Stn_code<-as.character(grabData$Stn_code)
grabData$Stn_code[get]<-"NATHPA"
grabData$Stn_code<-as.factor(grabData$Stn_code)
identical(levels(grabData$Stn_code),levels(CGSmeta$Stn_code))  # TRUE


grabData<-merge(grabData, CGSmeta[,3:4], by="Stn_code")
ggplot(grabData, aes(x=maxT, y=sum.LRS, group=Fuel, colour=Fuel)) + geom_point() + facet_wrap(~firstname) + scale_y_continuous(name='Daily cost of UI (Lakh Rupee)') + scale_x_continuous(name='Daily maximum temperature (deg. C)') + labs(title="Cost of Unscheduled Interchanges as a funciton of Maximum Ambient Temperature for CGS\nReceivable(-) / Payable (+)")

#############################
## Repeat for States
#############################
# load("d.StateTemps.rsav")
d.StateTemps$Beneficiary<-as.factor(d.StateTemps$Beneficiary)
levels(d.StateTemps$Beneficiary)
length(levels(d.StateTemps$Beneficiary))
# drop NR
d.StateTemps<-subset(d.StateTemps, Beneficiary != "NR")
d.StateTemps<-droplevels(d.StateTemps)

d.StateTemps$USAFID<-as.factor(d.StateTemps$USAFID)
length(levels(d.StateTemps$USAFID))

ggplot(d.StateTemps, aes(x=Date, y=maxT, group=Beneficiary, colour=USAFID)) + geom_line() + facet_wrap(~Beneficiary)

## Statewise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
load("StateUI.rsav")  #15-min cost of UI in *Ruppee* for NR States
load("d.StateUI.rsav")  #daily cost of UI in *Lakh Rupee* (10^5 RS)
load("m.StateUI.rsav")  #monthly cost of UI in *Lakh Rupee* (10^5 RS)
range(d.StateUI$Date) "2011-04-18" "2013-10-27"
range(d.StateTemps$Date) "2011-03-01" "2012-12-31"
# Temp data only goes up to Dec. 31 2012.
# StnUI data goes all the way up to Oct. 27 2013

#######################################
## Explore corelation between Temp and StateUI
###################################
# Temp data only goes up to Dec. 31 2012.
# UI data goes all the way up to Oct. 27 2013

setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
load("d.StateUI.rsav")
names(d.StateTemps)[1]<-"State"

length(levels(d.StateTemps$State))  #9
length(levels(d.StateUI$State))  #9
levels(d.StateTemps$State)
levels(d.StateUI$State)
# conform names
levels(d.StateUI$State)<-levels(d.StateTemps$State)

identical(levels(d.StateTemps$State),levels(d.StateUI$State))# TRUE

# combine State temp and UI data
grabData<-merge(d.StateTemps, d.StateUI, by=c("State","Date"))

ggplot(grabData, aes(x=maxT, y=sum.LRS)) + geom_point() + facet_wrap(~State) + scale_y_continuous(name='Daily cost of UI (Lakh Rupee)') + scale_x_continuous(name='Daily maximum temperature (deg. C)') + labs(title="Cost of Unscheduled Interchanges as a funciton of Max Temperature for NR States\nLakh Rupee Receivable (-) / Payable (+)") + theme_bw() + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.4)), plot.title = element_text(size = rel(1.4))) + stat_smooth(method="lm")

## Now combine d.StateTemps with d.UI [Sch-Act Drl]
## 15-minute and daily Sch-Drl given in LU (10^5 Wh = 0.1*MWh)
## get stnUI data
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
load("d.UI.rsav")
head(d.UI)

#setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
#load("d.StateTemps.rsav")
head(d.StateTemps)
names(d.StateTemps)[1]<-"State"
d.StateTemps<-subset(d.StateTemps, State!="NR")
d.StateTemps<-droplevels(d.StateTemps)
d.StateTemps$State<-as.factor(d.StateTemps$State)

length(levels(d.StateTemps$State))  #9
length(levels(d.UI$State))  #9
levels(d.StateTemps$State)
levels(d.UI$State)
# conform names
levels(d.UI$State)<-levels(d.StateTemps$State)

identical(levels(d.StateTemps$State),levels(d.UI$State))# TRUE

grabData<-merge(d.StateTemps, d.UI, by=c("State","Date"))
grabData<-droplevels(grabData)
levels(grabData$State)
dim(grabData)

ggplot(grabData, aes(x=maxT, y=Act.drl)) + geom_point() + facet_wrap(~State) + scale_y_continuous(name='Energy Drawal From Grid [GWh]') + scale_x_continuous(name='Max Temperature (deg. C)') + labs(title="Daily Energy Drawal from Grid to NR States as a funciton of Daily Maximum Temperature") + stat_smooth(method="lm") + theme_bw() + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.4)), plot.title = element_text(size = rel(1.4)))


# ## setwd() and load data....
# setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
# # Energy data (1-yr April 2012-March 2013)
# load("c.rsav"); str(c) #15-min act-sch-UI [energy] for NR States
# # "c" combined with UI rate schedule and instantaneous grid frequency to compute UIprice: UIrate ~ fn(Hz).  
# # c$UIprice is calculated cost of 15-min UI in *Lakh Rupee*
# # c$UI.Rupee is reported cost of 15-min UI in *Rupee*
# 
# load("c_Final.rsav"); str(c) # 15-min act-sch-UI [energy] combined with UIrate, Hz, UIprice and UI.LRS from "UI.rsav"
# range(c$Date)   #1-yr (April 2012-March 2013)
# 
# # "c" aggregated from 15-min to daily and monthly...
# # #1-yr (April 1 2012 - March 31 2013)
# load("d.UI.rsav") #daily act-sch-UI-UIrate-Hz-UI.LRS for NR States
# load("m.UI.rsav") #monthly aggregate act-sch-UI-UIrate-Hz-UI.LRS for NR States
# 
# # Original Normal_UI data in "UI.rsav".
# load("UI.rsav"); str(UI)
# range(UI$Date)
# levels(UI$Name)
# # UI split into StateUI and StnUI....
# # *State*wise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
# load("StateUI.rsav")  #15-min cost of UI in *Ruppee* for NR States
# load("d.StateUI.rsav")#daily cost of UI in *Lakh Rupee* (10^5 RS)
# load("m.StateUI.rsav")#monthly cost of UI in *Lakh Rupee* (10^5 RS)
# # UI.LRS added to "c" from State.UI
# 
# # *Station*wise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
# load("StnUI.rsav")  #15-min cost of UI in *Ruppee* for CGS
# load("d.StnUI.rsav") #daily cost of UI in *Lakh Rupee* (10^5 RS)
# range(StnUI$Date)
# ####
# #### 


# ###################################
# # QC: remove Stn's with less than 1-measurement per day
# # number of days in record
# days<-unique(d.temp$Date)
# ndays<-length(days); ndays
# range(d.temp$Date)
# # number of days in timespan
# test<-seq(range(d.temp$Date)[1], range(d.temp$Date)[2], by="day")
# ntest<-length(test); ntest
# missing<-which(! test %in% days)
# nmissing<-length(missing); nmissing
# test[missing]  #shows the missing dates: 
# # All dates have observations, but is there one per station?.
# freqtest<-table(d.temp$.id)
# drop<-which(freqtest[]<ntest)
# keep<-which(freqtest[]==ntest)
# length(drop)  #339 stations have less than 1 obs per day
# length(keep)  #109 stations have at least 1 obs per day
# 
# # but are these stations in India??
# load("metdata.rsav")
# INstns<-subset(metdata,Country=="IN")
# # subset d.temp to stations with at least one obs per day
# freqtest[keep]
# keep.id<-names(freqtest[keep])
# keep.id %in% INstns$USAFID
# # None of the Stations with at least one obs per day are in India!  Ugh!
# 
# # let's look at the stations with at least one obs per day, anyway...
# test<-d.temp[as.character(d.temp$.id) %in% keep.id,]
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
# INdata<-d.temp[d.temp$.id %in% INstns$USAFID, ]  #grab the accompanying data
# INdata<-droplevels(INdata)
# dim(INdata)  #16,668 x 3
# table(INdata$.id)  # observations per station
# save(INdata, file="INdata.rsav")
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
load("INdata.rsav")
##################################
# Spatial map of observed precip
#################################
par(mfrow=c(1,1))
par(mar=c(4, 3, 3, 2) + 0.1) 
library(akima)
library(fields)

# subset by season....
dummy<-strsplit(as.character(INdata$Date), split="-")
INdata$Yr<-laply(dummy, '[[', 1)
INdata$Mon<-laply(dummy, '[[', 2)

# aggregate from daily to monthly precip
m.INdata<-ddply(INdata, .(Yr, Mon, .id, City, Country, Lat, Long, Elev), summarize, accumulation=sum(precip), avgP=mean(precip), maxP=max(precip), minP=min(precip), .progress="time")

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
for (i in 1:12){
  data<-subset(monthly, Date==mons[i])
  zz0<-interp(x=data$Long, y=data$Lat, z=data$accumulation)
  image(zz0, col=topo.colors(n=64,alpha=1), main=paste(data$Mon[1], data$Yr[1], "Total precip (mm)", sep=" "), lab.breaks=NULL)
  contour(zz0, add=T)
  world(add=TRUE, lwd=4)
}

set.panel(1,4) # nXm matrix of plots
par(oma=c(0,0,0,1))# reset margin to be much smaller.
par(mar=c(0,0,0,0)+0.0) 
image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(0,max(monthly$accumulation)), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
# image.plot tricked into  plotting in margin of old setting 
# end plot

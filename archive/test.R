## Monthly rainfall data for India
## Subdivision-wise
## ftp://www.tropmet.res.in/pub/data/rain/iitm-imr-readme.txt
## ftp://www.tropmet.res.in/pub/data/rain/iitm-subdivrf.txt
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")

# # Monthly Data upto 1 decimal in mm.
# names<-c("ID","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# class<-c("character",rep("numeric",12))
# 
# precip<-read.table(file="IndiaPrecip.txt", header=FALSE, sep="", col.names=names, colClasses=class, fill=TRUE, strip.white=TRUE)
# dim(precip)  # 4260 x 12
# dim(precip)[1]/30  # 30 meteoroligcal subdivisions-->142 records each.
# head(precip)
# save(precip, file="precip.rsav")
# 
# load("precip.rsav")
# # look for NA's
# sum(is.na(precip))  #207 NA's
# which(is.na(precip), arr.ind=TRUE)  # in last three columns...
# look<-which(is.na(precip), arr.ind=TRUE)
# precip[look[],] 
# 
# # how many records contains NA's?
# sum(is.na(precip))/dim(precip)[1]  # less than 5% are NA's...
# # omit records with NA's... (very conservative... could salvage most of the record less the one or two NA's contained in each)
# precip<-na.omit(precip)
# 
# # seperate State ID from Year in "ID" attribute
# dummy<-strsplit(as.character(precip$ID), split=character(0))
# s1<-laply(dummy, "[[", 1)  
# s2<-laply(dummy, "[[", 2)  
# s3<-laply(dummy, "[[", 3)  
# s4<-laply(dummy, "[[", 4)  
# s5<-laply(dummy, "[[", 5)
# div<-paste(s1,s2,s3,s4,s5, sep="")
# precip$id<-div
# 
# # grab the year from the ID...
# dummy<-strsplit(as.character(precip$ID), split=character(0))
# yr1<-laply(dummy, "[[", 6)
# yr2<-laply(dummy, "[[", 7)
# yr3<-laply(dummy, "[[", 8)
# yr4<-laply(dummy, "[[", 9)
# yr<-paste(yr1,yr2,yr3,yr4, sep="")
# precip$yr<-yr
# 
# precip<-subset(precip, select=c("id","yr","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# ##look for NA's
# sum(is.na(precip))
# which(is.na(precip), arr.ind=TRUE)
# look<-which(is.na(precip), arr.ind=TRUE)
# precip[look[1],] 
# 
# save(precip, file="precip.rsav")
load("precip.rsav")

# long-term monthly means (climatology)
climate<-ddply(precip, .(id), numcolwise(mean), na.rm=TRUE)
climate[,2:13]<-round(climate[,2:13], digits=0)
head(climate)

# create precip anamoly matrix
# take each row (year) in precip df and substract corresponding monthly climatology for that meteorological subdivision.
precip$id<-as.factor(precip$id)
id<-levels(precip$id)
P_anomaly<-precip  # start with monthly precip data..
P_anomaly<-merge(precip, climate, by="id")  # create augmented df with annual precip data and climatology in each row
P_anomaly[,3:14]<-P_anomaly[,3:14]-P_anomaly[,15:26]
P_anomaly<-P_anomaly[,1:14]
names(P_anomaly)<-names(precip)
names<-names(precip)

# mean of the anomalies over the entire period of record should be zero.
check<-ddply(P_anomaly, .(id), numcolwise(mean)) 
round(check[,2:13], digits=0)

idnames<-c("ASSAM & MEGHALAYA","BIHAR","CHATTISGARH","COASTAL ANDHRA PRA.","COASTAL KARNATAKA","EAST MADHYA PRADESH","EAST RAJASTHAN","EAST UTTAR PRADESH","GANGETIC W. BENGAL", "GUJARAT","HARYANA","JHARKHAND","KERALA","KONKAN AND GOA","MADHYA MAHARASHTRA ","MARATHWADA","NORTH  INT. KARNATAKA","NAGA.MANI.MIZO.&TRIP.","ORISSA","PUNJAB","RAYALASEEMA","SAURASHTRA & KUTCH","SUB-HIMA. W. BENGAL","SOUTH INT. KARNATAKA","TELANGANA","TAMIL NADU","VIDARBHA","WEST MADHYA PRADESH","WEST RAJASTHAN","WEST U.P. PLAINS")

names<-cbind(id,idnames)
table(P_anomaly$id)

P_anomaly<-merge(P_anomaly, names, by="id")

# melt precip data, create timeseries
P_anomaly<-melt(P_anomaly, id.var=c("id", "idnames","yr"))
P_anomaly$Date<-as.Date(paste(P_anomaly$yr, P_anomaly$variable, "15", sep="-"), format="%Y-%b-%d")
P_anomaly<-melt(P_anomaly, id.var=c("id", "idnames","Date"), measure.var="value")
P_anomaly<-P_anomaly[,-4]
names(P_anomaly)<-c("id", "Area","Date","P_Anomaly_mm")

# plot data
ggplot(P_anomaly, aes(x=Date, y=P_Anomaly_mm, group=Area, colour=Area)) + geom_line() + scale_y_continuous(name="Precipitation Anomly (mm)") + labs(title="Monthly Precipitation Anomaly for N. India Meteorological Subdivisions")

dim(P_anomaly)  # 49068 x 4
save(P_anomaly, file="P_anomaly.rsav")

##############################
# Repeat for actual precip...
############################
P_act<-precip
idnames<-c("ASSAM & MEGHALAYA","BIHAR","CHATTISGARH","COASTAL ANDHRA PRA.","COASTAL KARNATAKA","EAST MADHYA PRADESH","EAST RAJASTHAN","EAST UTTAR PRADESH","GANGETIC W. BENGAL", "GUJARAT","HARYANA","JHARKHAND","KERALA","KONKAN AND GOA","MADHYA MAHARASHTRA ","MARATHWADA","NORTH  INT. KARNATAKA","NAGA.MANI.MIZO.&TRIP.","ORISSA","PUNJAB","RAYALASEEMA","SAURASHTRA & KUTCH","SUB-HIMA. W. BENGAL","SOUTH INT. KARNATAKA","TELANGANA","TAMIL NADU","VIDARBHA","WEST MADHYA PRADESH","WEST RAJASTHAN","WEST U.P. PLAINS")

names<-cbind(id,idnames)
table(P_act$id)

P_act<-merge(P_act, names, by="id")

# melt precip data, create timeseries
P_act<-melt(P_act, id.var=c("id", "idnames","yr"))
P_act$Date<-as.Date(paste(P_act$yr, P_act$variable, "15", sep="-"), format="%Y-%b-%d")
P_act<-melt(P_act, id.var=c("id", "idnames","Date"), measure.var="value")
P_act<-P_act[,-4]
names(P_act)<-c("id", "Area","Date","P_Act_mm")

# plot data
ggplot(P_act, aes(x=Date, y=P_Act_mm, group=Area, colour=Area)) + geom_line() + scale_y_continuous(name="Precipitation (mm)") + labs(title="Monthly Precipitation for N. India Meteorological Subdivisions")

dim(P_act)  # 49068 x 4
save(P_act, file="P_act.rsav")


###########################
# All-India, past two years.
###########################
surf<-P_anomaly[P_anomaly$Date > "2011-03-15", ]
mons<-unique(surf$Date)
mons<-mons[order(mons)]

################################
# grab NR states (sub-divisions)
#################################
grab<-c("EAST RAJASTHAN","EAST UTTAR PRADESH","WEST RAJASTHAN","WEST U.P. PLAINS","PUNJAB", "HARYANA")

# merge idnames with P_anomaly, then subset to just the divisions we need.
P<-merge(P_anomaly, P_act, by=c("id","Area","Date"))
P<-P[P$Area %in% grab, ]
dim(P)  # 10,224 x 5

# plot just the NR region
ggplot(P, aes(x=Date, y=P_Anomaly_mm, group=Area, colour=Area)) + geom_line() + scale_y_continuous(name="Precipitation Anomly (mm)") + labs(title="Monthly Precipitation Anomaly for N. India Meteorological Subdivisions")

# plot just the two most recent years...
P2<-P[P$Date > "2011-01-15", ]

# HARYANA includes Haryana, Chandigrah and Delhi  
# combine EAST/WEST UTTAR PRADESH 
# combine EAST/WEST RAJASTHAN
# UTTARANCHAL == UTTARAKHAND
# UTTARANCHAL, HP AND JK NOT INCLUDED IN IITM Regional Rainfall Data Sets

# plot the last two years for NR region 
# reorder factor levels
P2$Area<-factor(P2$Area,levels=c("WEST RAJASTHAN","EAST RAJASTHAN","PUNJAB","HARYANA","WEST U.P. PLAINS","EAST UTTAR PRADESH"))

ggplot(P2, aes(x=Date, y=P_Anomaly_mm)) + geom_line() + scale_y_continuous(name="Precipitation Anomly (mm)") + labs(title="Monthly Precipitation Anomaly for N. India Meteorological Subdivisions") + facet_wrap(~Area) + theme_bw() + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%Y"))

ggplot(P2, aes(x=Date, y=P_Act_mm)) + geom_line() + scale_y_continuous(name="Precipitation (mm)") + labs(title="Monthly Actual Precipitation for N. India Meteorological Subdivisions") + facet_wrap(~Area) + theme_bw() + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%Y"))

ggplot(P2, aes(x=Date, y=P_Act_mm, group=Area, colour=Area)) + geom_line() + scale_y_continuous(name="Precipitation (mm)") + labs(title="Monthly Actual Precipitation for N. India Meteorological Subdivisions") + theme_bw() + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%Y"))


## up to here on Dec. 19 2013.
## combine East/West for Rajasthan and UP, respectively
## Assign Haryana values to Delhi and Chandigarh
## Add Monthly precip accumulation to "data" and "model"
## drop JK, HP and Uttarakhand from model
## test Requirement ~ fn(Temp + Precip)

# combine East/West
P2$State<-P2$Area
P2$State<-as.character(P2$State)
UPE<-which(P2$Area=="EAST UTTAR PRADESH")
UPW<-which(P2$Area=="WEST U.P. PLAINS")
P2$State[c(UPE,UPW)]<-"UP"

RE<-which(P2$Area=="EAST RAJASTHAN")
RW<-which(P2$Area=="WEST RAJASTHAN")
P2$State[c(RE,RW)]<-"Rajasthan"

P3<-ddply(P2, .(State, Date), numcolwise(mean))
H<-subset(P3, State=="HARYANA")
H$State<-"Haryana" #first letter capitalization
D<-H              #assign Haryana precip values to Delhi
D$State<-"Delhi"
# C<-H            #assign Haryana precip values to Chandigarh
# C$State<-"Chandigarh"

P4<-rbind(P3, D)
P4$State<-as.factor(P4$State)
levels(P4$State)

save(P4, file="P4.rsav")

# merge precip with data
load("data.rsav")
load("P4.rsav")
data$ID<-as.factor(tolower(data$Beneficiary))
P4$ID<-as.factor(tolower(P4$State))
data2<-merge(data,P4, by=c("ID", "Date"))
# drop redundant columns
data2<-subset(data2, select = -State)
data2<-subset(data2, select = -ID)
data2<-droplevels(data2)
save(data2, file="data2.rsav")

#############################################
# ## Create a spatial surface plot
# ## Need gridded or staiton data to do this.... 
# par(mfrow=c(1,1)) 
# # par(mar=c(5,4,4,2)+0.1) # default
# par(mar=c(2,2,1.5,1)+0.1) 
# par(oma=c( 0,0,0,4))     # margin of 3 spaces width at right hand side
# set.panel(3,4)           # 3X4 matrix of plots
# 
# # now draw plots using image() command
# for (i in 1:12){
#   data<-subset(surf, Date==mons[i])
#   zz0<-interp(x=data$Long, y=data$Lat, z=data$temp.total)
#   image(zz0, col=topo.colors(n=64,alpha=1), main=paste(data$Mon[1], data$Yr[1], "Total temp (mm)", sep=" "), lab.breaks=NULL)
#   contour(zz0, add=T)
#   world(add=TRUE, lwd=4)
# }
# 
# set.panel(1,4) # nXm matrix of plots
# par(oma=c(0,0,0,1))# reset margin to be much smaller.
# par(mar=c(0,0,0,0)+0.0) 
# image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(0,max(surf$temp.total)), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
# # image.plot tricked into  plotting in margin of old setting 
# # end plot





# Data Description:
#   
#   Network of  rain-gauge stations:
#   
#   While selecting the network of rain-gauge stations, an 
# effort was made to select a network which would provide one 
# representative station per district having a reliable record 
# for the longest possible period. The network selected under 
# these constraints consist of 306 almost uniformly distributed 
# stations for which rainfall data are available from 1871. The 
# hilly regions consisting of four meteorological subdivisions 
# of India which are parallel to Himalayan mountain range have 
# not been considered in view of the meagre  rain-gauge  network 
# and low areal representation of a rain-gauge in a hilly area. 
# Two island subdivisions far away from mainland have also not 
# been included. Thus, the contiguous area having network of 306 
# stations over 30 meteorological subdivisions measures about 
# 2,880,000 sq.km., which is about 90 percent of the total area 
# of the country.     
# 
# Preparation of Subdivisional/Regional rainfall series:
# 
# The monthly (January - December) area  weighted rainfall 
# series for each of the 30 meteorological subdivisions have 
# been prepared by assigning the district area as the weight for 
# each rain-gauge station in that subdivision. Similarly assigning 
# the subdivision area as the weight to each of the subdivisions in
# the region, area weighted monthly rainfall series are prepared for
# Homogeneous regions of India as well as for all India.
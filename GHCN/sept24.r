require(reshape2)
require(plyr)
load("Stationwise.rsav")
load("CGSmeta.rsav")
str(Stationwise)
Stationwise$Stn_code <- as.character(Stationwise$Stn_code)

# ## fix DADRI
# whDadri <- which(Stationwise$Stn_code=='DADRI')
# Stationwise$Stn_code[whDadri] <-
#   paste(Stationwise$Stn_code[whDadri],
#         Stationwise$Plant_type[whDadri],sep='-')
# 
# ## this is a sneak subset trick for later,
# ## will stuff values into here from the add and avg vars.
# dadri2Sub <- subset(Stationwise, Stn_code=='DADRI-II')
# 
# whDadri2 <- which(Stationwise$Stn_code=='DADRI-II')
# Stationwise$Stn_code[whDadri2] <-
#   paste('DADRI',Stationwise$Plant_type[whDadri2],sep='-')
# 
# whDadriTPS <- which(Stationwise$Stn_code=='DADRI-TPS')
# dadriTpsSub <- subset(Stationwise, Stn_code=='DADRI-TPS')
# 
# ## columns to add
# colsAdd <- c("MW", "Chandigarh", "Delhi", "HP", "Haryana",
#              "JK", "Punjab", "Rajasthan", "Uttarakhand",
#              "UP", "NR", "Year",'Mon')
# dts <- melt(dadriTpsSub[,colsAdd], id=c("Year",'Mon'))
# dts$uniqueMon <- paste(dts$Year,dts$Mon,sep='-')
# dtsAdd <- ddply(dts, .(uniqueMon, variable), summarize,
#                 sum=sum(value))
# addDf <- dcast(dtsAdd, uniqueMon ~ variable )
# addDf$Year <- laply(strsplit(addDf$uniqueMon,'-'),'[',1)
# addDf$Mon <- laply(strsplit(addDf$uniqueMon,'-'),'[',2)
# addDf <- addDf[sort(13*as.numeric(addDf$Year)+
#                     as.numeric(addDf$Mon), index.return=TRUE,
#                     dec=FALSE)$ix,]
# 
# colsAvg <- c("RateOfSale", "PAFM", "Year",'Mon')
# dts <- melt(dadriTpsSub[,colsAvg], id=c("Year",'Mon'))
# dts$uniqueMon <- paste(dts$Year,dts$Mon,sep='-')
# dtsAvg <- ddply(dts, .(uniqueMon, variable), summarize,
#                 mean=mean(value))
# avgDf <- dcast(dtsAvg, uniqueMon ~ variable )
# avgDf$Year <- laply(strsplit(avgDf$uniqueMon,'-'),'[',1)
# avgDf$Mon <- laply(strsplit(avgDf$uniqueMon,'-'),'[',2)
# avgDf <- avgDf[sort(13*as.numeric(avgDf$Year)+
#                     as.numeric(avgDf$Mon), index.return=TRUE,
#                     dec=FALSE)$ix,]
# str(dadri2Sub)
# 
# dadri2Sub[,c('Year','Mon')]==addDf[,c('Year','Mon')]
# dadri2Sub[,c('Year','Mon')]==avgDf[,c('Year','Mon')]
# 
# dadri2Sub[,colsAvg] <- avgDf[,-1]
# dadri2Sub[,colsAdd] <- addDf[,-1]
# dadri2Sub$Stn_code <- 'DADRI-TPS'
# 
# Stationwise <- subset(Stationwise, Stn_code!='DADRI-TPS')
# Stationwise2 <- rbind(Stationwise,dadri2Sub)
# save(Stationwise2, file='Stationwise2.rsav')
# 

##'#####################################################################
## look at data from beneficiaries' perspective
beneDf <- Stationwise[,c('Stn_name','Stn_code','Fuel','Fueltype',
                         'POSIXct','Date','Metric','Mon','Year',
                         'Chandigarh','Delhi','HP','Haryana','JK',
                         'Punjab','Rajasthan','Uttarakhand','UP',
                         'RateOfSale','PAFM')]

beneDfMelt <-
  melt(beneDf, id=c('Stn_name','Stn_code','Fuel','Fueltype',
                 'POSIXct','Date','Metric','Mon','Year',
                 'RateOfSale','PAFM') )
names(beneDfMelt)[12:13] <- c("beneficiary",'stnAlloc')
beneDfMelt$uniqueMon <- format(beneDfMelt$POSIXct, '%Y-%m')

##'##################################################################
## sum beneficiaries' monthly PSP (power supply position)
## over all stations
beneMonPsp <- ddply( beneDfMelt, .(uniqueMon, beneficiary),
                    summarize, PSP=sum(stnAlloc) )
beneMonSale <- ddply( beneDfMelt, .(uniqueMon, beneficiary),
                    summarize, sale=sum(RateOfSale*stnAlloc) )
beneMonPsp$sale <- beneMonSale$sale
beneMonPsp$AvgRate <- beneMonPsp$sale/beneMonPsp$PSP


beneMonPsp$beneficiary <-
  factor(beneMonPsp$beneficiary,
         levels=sort(as.character(unique(beneMonPsp$beneficiary))) )
beneMonPsp$POSIXct <-
  as.POSIXct(paste(beneMonPsp$uniqueMon,'15',sep='-'),'%Y-%m-%d')
ggplot(beneMonPsp, aes(x=POSIXct,y=PSP,color=beneficiary)) +
  geom_line()+ geom_point() +
  facet_wrap(~beneficiary, scales='free')

## analyze contribs
indivContribs <-
  subset(beneDfMelt, beneficiary=='Delhi' & uniqueMon %in% c('2011-07','2011-08','2011-09'))
ggplot( indivContribs, aes(x=POSIXct, y=stnAlloc, color=Stn_code) ) +
  geom_point() + geom_line() + facet_wrap(~Stn_code)



##'##################################################################
## sum beneficiaries' monthly PSP (power supply position)
## over all stations *by type*
beneMonPsp <- ddply( beneDfMelt, .(uniqueMon, beneficiary, fuel),
                    summarize, PSP=sum(stnAlloc) )
                                 
beneMonPsp$POSIXct <-
  as.POSIXct(paste(beneMonPsp$uniqueMon,'15',sep='-'),'%Y-%m-%d')
ggplot(beneMonPsp, aes(x=POSIXct,y=PSP,color=beneficiary)) +
  geom_line()+ geom_point() +
  facet_wrap(~beneficiary, scales='free')

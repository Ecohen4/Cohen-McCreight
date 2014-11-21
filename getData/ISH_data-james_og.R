options(warn=1)
if (!require(rgdal)) install.packages('rgdal')
if (!require(spdep)) install.packages('spdep')
if (!require(sp)) install.packages('sp')
if (!require(MBA)) install.packages('MBA')
if (!require(fields)) install.packages('fields')
options(warn=2)

##DOWNLOAD STATION LIST----
##SET THIS TO WHERE YOU WANT TO DOWNLOAD YOUR FILES##
setwd("~/Desktop/Dropbox/Cohen-McCreight")

file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
repeat {
  try(download.file(file, "data/ish-history.csv", quiet=TRUE))
  if (file.info("data/ish-history.csv")$size > 0) {break}
}
st <- read.csv("data/ish-history.csv")
dim(st)
names(st)
names(st)[c(3,10)] <- c("NAME", "ELEV")
st <- st[,-5]
#FILTER STATION LIST BY LAT / LON AND RANGE----
##ALTERNATE OR IN ADDITION CAN FILTER BY COUNTRY BELOW##
##ENTER IN MIN / MAX LAT / LON AND TIME RANGE##
##LAT / LON IN (LAT / LON) X 1000
##TIME IN YYYYMMDD
lon.min <- 70000
lon.max <- 90000
lat.min <- -22000
lat.max <- 38000
dat.start <- 20110401
dat.end <- 20130401
st <- subset(st,LON > lon.min & LON < lon.max & LAT > lat.min & LAT < lat.max & BEGIN <= dat.start & END >= dat.end) #230 stations in lat-long region. 136 of which in India

#FILTER BY COUNTRY----
##ENTER COUNTRY A2 CODES IN QUOTES("") BELOW##
##A2 CODES CAN BE FOUND HERE http://www.worldatlas.com/aatlas/ctycodes.htm ##
##TO INCLUDE COUNTRY USE ==, TO EXCLUDE COUNTRY USE !=
ctry <-c("IN")
##IF NOT SUBSETTING BY COUNTRY, DON'T USE...SEE BELOW----
st.set <- subset(st,CTRY == ctry) # 136 stations in Northern India....
##THIS LINE INSTEAD
# st.set <- st
#EDIT LAT, LON, ELEV, BEGIN, AND END COLUMNS----
st.set$LAT <- st.set$LAT/1000
st.set$LON <- st.set$LON/1000
st.set$ELEV <- st.set$ELEV/10
st.set$BEGIN <- as.numeric(substr(st.set$BEGIN, 1, 4))
st.set$END <- as.numeric(substr(st.set$END, 1, 4))

#outputs <- as.data.frame(matrix(NA, dim(st.set)[1], 2))
#names(outputs) <- c("FILE", "STATUS")
## STATUS equal to 0 equals downloaded, 256 equals not downloaded    
## what does 127 mean??? EC 10-24-2012

outputs2 <- data.frame(FILE=NA, STATUS=NA)
for (y in as.numeric(substr(dat.start,1,4)):as.numeric(substr(dat.end,1,4))) {
  for (s in 1:dim(st.set)[1]) {
    #outputs[s,1] <- paste(sprintf("%06d", st.set[s,1]), "-", sprintf("%05d", st.set[s,2]), "-", y, ".gz", sep="")
    theFile <- paste(sprintf("%06d", st.set[s,1]), "-", sprintf("%05d", st.set[s,2]), "-", y, ".gz", sep="")
#    wget <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y, "/", outputs[s,1], sep="")
    theWget <- paste("wget -P data/raw http://www1.ncdc.noaa.gov/pub/data/noaa/", y, "/", theFile, sep="")
    print(theWget)
    #outputs[s,2] <- try(system(wget, intern=FALSE, ignore.stderr=TRUE))
    theTry <- try(system(theWget, intern=FALSE, ignore.stderr=TRUE))
    print(theTry)
    outputs2 <- rbind(outputs2, data.frame(FILE=theFile, STATUS=theTry))
  }
}

#head(outputs)
#sum(outputs$STATUS == 256)
#sum(outputs$STATUS == 0)

outputs2 <- outputs2[-1,]
save(outputs2, file='outputs2.rsav')

length(outputs2$STATUS)
sum(outputs2$STATUS)

system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)

##'#####################################################################

# Need to identify which column widths to extract....  EC 10-24-2012
files <- list.files("data/raw")
column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1,4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)
stations <- as.data.frame(matrix(NA, length(files), 6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")
for (i in 1:length(files)) {
  data <- read.fwf(paste("data/raw/", files[i], sep=""), column.widths)
  data <- data[,c(2:8,10:11,13,16,19,29,31,33)]
  names(data) <- c("USAFID","WBAN","YR","M","D","HR","MIN","LAT","LONG","ELEV","WIND.DIR", "WIND.SPD", "TEMP","DEW.POINT","ATM.PRES")
  data$LAT <- data$LAT/1000
  data$LONG <- data$LONG/1000
  data$WIND.SPD <- data$WIND.SPD/10
  data$TEMP <- data$TEMP/10
  data$DEW.POINT <- data$DEW.POINT/10
  data$ATM.PRES <- data$ATM.PRES/10
  write.csv(data, file=paste("data/csv/", files[i], ".csv", sep=""), row.names=FALSE)
  stations[i,1:3] <- data[1,1:3]
  stations[i,4:6] <- data[1,8:10]
}
write.csv(stations, file="data/stations.csv", row.names=FALSE)


###################################################
### chunk number 10: Test data
###################################################
files <- list.files("data/csv")
st <- read.csv(file=paste("data/csv/", files[85], sep=""))
head(st)
dim(st)


###################################################
### chunk number 11: Temp-plot
###################################################
sum(st$TEMP == 999.9)
sum(st$WIND.SPD == 999.9)
sum(st$WIND.DIR == 999)
sum(st$DEW.POINT == 999.9)
sum(st$ATM.PRES == 9999.9)
st$WIND.DIR <- st$DEW.POINT <- st$ATM.PRES <- NULL
st$TEMP[st$TEMP == 999.9] <- NA
st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
st <- st[st$MIN == 0,]
dim(st)
365*24
st <- st[order(st$M, st$D, st$HR), ]
st$DATE <- as.Date(paste(st$YR, st$M, st$D, sep="-"), format="%Y-%m-%d")
d.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE, "%Y-%m-%d")), mean, na.rm=T)
m.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE, "%Y-%m")), mean, na.rm=T)
d.mean$DATE <- as.Date(d.mean$DATE)
m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15", sep=""))
plot(st$DATE, st$TEMP, main="Temperature readings", ylab="Temperature (Degrees C)", xlab="Month", col="grey")
points(d.mean$DATE, d.mean$x, col="brown")
lines(m.mean$DATE, m.mean$x, type="b", pch=16)
legend("topleft", c("Hourly", "Daily mean", "Monthly mean"), inset=0.02, pch=c(1,1,16), col=c("grey", "red", "black"))



###################################################
### chunk number 1: libs
###################################################
options(width = 65)
library(rgdal)
library(spdep)
library(sp)
library(MBA)
library(fields)


###################################################
### chunk number 2: Import master station list
###################################################
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
st <- st[st$CTRY == "US",]


###################################################
### chunk number 3: Edit master station list
###################################################
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))


###################################################
### chunk number 4: Count NAs
###################################################
dim(st)
sum(is.na(st$BEGIN))
sum(is.na(st$END))
sum(st$STATE == "")


###################################################
### chunk number 5: Subset station data
###################################################
mi.list <- st[st$STATE == "MI" & (st$BEGIN <= 2005 & st$END >= 2005 & !is.na(st$BEGIN)), ]
dim(mi.list)


###################################################
### chunk number 6: Subset station data 2
###################################################
mi.list.2 <- st[st$STATE == "MI" & (st$BEGIN <= 2009 & st$END >= 2002 | is.na(st$BEGIN)), ]
dim(mi.list.2)


###################################################
### chunk number 7: Create wget loop
###################################################
## Start yearly loop
outputs <- as.data.frame(matrix(NA, dim(mi.list)[1], 2))
names(outputs) <- c("FILE", "STATUS")
## STATUS equal to 0 equals downloaded, 256 equals not downloaded
for (y in 2005:2005) {
  y.mi.list <- mi.list[mi.list$BEGIN <= y & mi.list$END >= y, ]
  ## Start looping through available stations
  for (s in 1:dim(y.mi.list)[1]) {
    outputs[s,1] <- paste(sprintf("%06d", y.mi.list[s,1]), "-", sprintf("%05d", y.mi.list[s,2]), "-", y, ".gz", sep="")
    wget <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y, "/", outputs[s,1], sep="")
    outputs[s,2] <- try(system(wget, intern=FALSE, ignore.stderr=TRUE))
  }
}
head(outputs)
sum(outputs$STATUS == 256)
sum(outputs$STATUS == 0)


###################################################
### chunk number 8: Decompress files
###################################################
system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)


###################################################
### chunk number 9: Import
###################################################
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
mi.state <- readOGR("data/gis/.", "michigan", verbose=FALSE)
mi <- mi.state[mi.state$AREA > 0.01,]
plot(mi, xlab = "Degrees", ylab = "Degrees", axes=T)
points(stations$LONG, stations$LAT, pch=16, col='red')


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


###################################################
### chunk number 12: Wind-plot
###################################################
d.mean <- aggregate(st$WIND.SPD, list(DATE = format(st$DATE, "%Y-%m-%d")), mean, na.rm=T)
m.mean <- aggregate(st$WIND.SPD, list(DATE = format(st$DATE, "%Y-%m")), mean, na.rm=T)
d.mean$DATE <- as.Date(d.mean$DATE)
m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15", sep=""))
plot(st$DATE, st$WIND.SPD, main="Wind speed readings", ylab="Wind Speed (meters/second)", xlab="Month", col="grey")
points(d.mean$DATE, d.mean$x, col="brown")
lines(m.mean$DATE, m.mean$x, type="b", pch=16)
legend("topleft", c("Hourly", "Daily mean", "Monthly mean"), inset=0.02, pch=c(1,1,16), col=c("grey", "red", "black"))


###################################################
### chunk number 13: Read summer data
###################################################
files <- list.files("data/csv")
daily.data <- vector("list", length(files))
dates <- seq(as.Date("2005-06-21"), as.Date("2005-09-21"), 1)
for (i in 1:length(files)) {
  daily.data[[i]]$DATE <- dates
  daily.data[[i]]$TEMP <- daily.data[[i]]$WIND.SPD <- rep(NA, length(dates))
  daily.data[[i]]$USAFID <- daily.data[[i]]$WBAN <- rep(NA, length(dates))
  daily.data[[i]]$LAT <- daily.data[[i]]$LONG <- rep(NA, length(dates))
  daily.data[[i]] <- as.data.frame(daily.data[[i]])
  st <- read.csv(paste("data/csv/", files[i], sep=""))
  st$WIND.DIR <- st$DEW.POINT <- st$ATM.PRES <- NULL
  st$DATE <- as.Date(paste(st$YR, st$M, st$D, sep="-"), format="%Y-%m-%d")
  st <- st[st$DATE >= "2005-06-21" & st$DATE <= "2005-09-21",]
  st$TEMP[st$TEMP == 999.9] <- NA
  st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
  st <- st[!is.na(st$TEMP) | !is.na(st$WIND.SPD), ]
  u <- unique(st$LAT)
  max.u <- vector("numeric", length(u))
  for (z in 1:length(u)) {
    max.u[z] <- sum(st$LAT == u[z])
  }
  pos <- which(max.u == max(max.u))
  st <- st[st$LAT == u[pos], ]
  if (length(unique(st$LONG)) > 1) {
    u <- unique(st$LONG)
    max.u <- vector("numeric", length(u))
    for (z in 1:length(u)) {
      max.u[z] <- sum(st$LONG == u[z])
    }
    pos <- which(max.u == max(max.u))
    st <- st[st$LONG == u[pos], ]
  }
  ## nudge observations
  if (sum(st$MIN == 0) < 2232) {
    st$HR[st$MIN > 50] <- st$HR[st$MIN > 50]+1
    st$MIN[st$MIN > 50] <- 0
    st$MIN[st$MIN < 10] <- 0
    st$DATE[st$MIN == 24] <- st$DATE[st$MIN == 24]+1
    st$YR <- as.numeric(format(st$DATE, "%Y"))
    st$M <- as.numeric(format(st$DATE, "%m"))
    st$D <- as.numeric(format(st$DATE, "%d"))
    }  
  st <- st[st$MIN == 0,]
  daily.data[[i]]$USAFID <- rep(st$USAFID[1], length(dates))
  daily.data[[i]]$WBAN <- rep(st$WBAN[1], length(dates))
  daily.data[[i]]$LAT <- rep(st$LAT[1], length(dates))
  daily.data[[i]]$LONG <- rep(st$LONG[1], length(dates))
  for (j in dates) {
    sub.st <- st[st$DATE == j, ]
    if (dim(sub.st)[1] >= 18) {
      daily.data[[i]]$TEMP[daily.data[[i]]$DATE == j] <- mean(sub.st$TEMP)
      daily.data[[i]]$WIND.SPD[daily.data[[i]]$DATE == j] <- mean(sub.st$WIND.SPD)
    }
  }
}


###################################################
### chunk number 14: Obs-plot
###################################################
stations$TEMP.OBS <- rep(NA, dim(stations)[1])
stations$WIND.SPD.OBS <- stations$TEMP.OBS
stations$ELEV <- NULL
for (i in 1:length(daily.data)) {
  n.temp <- sum(!is.na(daily.data[[i]]$TEMP))
  n.wind <- sum(!is.na(daily.data[[i]]$WIND.SPD))
  stations$TEMP.OBS[stations$USAFID == daily.data[[i]]$USAFID[1] & stations$WBAN == daily.data[[i]]$WBAN[1]] <- n.temp
  stations$WIND.SPD.OBS[stations$USAFID == daily.data[[i]]$USAFID[1] & stations$WBAN == daily.data[[i]]$WBAN[1]] <- n.wind
  stations$LAT[stations$USAFID == daily.data[[i]]$USAFID[1] & stations$WBAN == daily.data[[i]]$WBAN[1]] <- daily.data[[i]]$LAT[1]
  stations$LONG[stations$USAFID == daily.data[[i]]$USAFID[1] & stations$WBAN == daily.data[[i]]$WBAN[1]] <- daily.data[[i]]$LONG[1]
}
plot(mi, xlab = "Degrees", ylab = "Degrees", axes=T)
symbols(stations$LONG, stations$LAT, circles=stations$TEMP.OBS, inches=0.1, fg="black", bg="green", add=T)
symbols(c(-91,-91,-91), c(43,42.5,42), circles=c(1,42,93), inches=0.1, fg="black", bg="green", add=T)
text(c(-90.8,-90.8,-90.8), c(43,42.5,42), c("1", "42", "93"), pos=4)
text(-91.7, 43.4, "Daily observations", pos=4, cex=1.05)


###################################################
### chunk number 15: Obs-plot-2
###################################################
plot(mi, xlab = "Degrees", ylab = "Degrees", axes=T)
symbols(stations$LONG, stations$LAT, circles=stations$WIND.SPD.OBS, inches=0.1, fg="black", bg="red", add=T)
symbols(c(-91,-91,-91), c(43,42.5,42), circles=c(1,42,93), inches=0.1, fg="black", bg="red", add=T)
text(c(-90.8,-90.8,-90.8), c(43,42.5,42), c("1", "42", "93"), pos=4)
text(-91.7, 43.4, "Daily observations", pos=4, cex=1.05)


###################################################
### chunk number 16: Plot-temp-surf
###################################################
daily.obs <- do.call(rbind, daily.data)
t.obs <- daily.obs[daily.obs$DATE == dates[1] & !is.na(daily.obs$TEMP), c("LONG", "LAT", "TEMP")]
mba.bbox <- c(-91,-82,41,49)
surf <- mba.surf(t.obs, 75, 75, extend=TRUE, sp=TRUE, b.box=mba.bbox)$xyz.est
surf@data <- surf@data * !is.na(overlay(surf, mi))
surf$z[surf$z == 0] <- NA
surf$z <- surf$z*(9/5)+32
image.plot(as.image.SpatialGridDataFrame(surf), asp=1.25)
title(main="Temperature surface (degrees F)")
plot(mi, add=TRUE)
points(t.obs$LONG, t.obs$LAT, pch=20)


###################################################
### chunk number 17: Plot-wind-surf
###################################################
w.obs <- daily.obs[daily.obs$DATE == dates[1] & !is.na(daily.obs$WIND.SPD), c("LONG", "LAT", "WIND.SPD")]
surf.w <- mba.surf(w.obs, 75, 75, extend=TRUE, sp=TRUE, b.box=mba.bbox)$xyz.est
surf.w@data <- surf.w@data * !is.na(overlay(surf.w, mi))
surf.w$z[surf.w$z == 0] <- NA
surf.w$z <- surf.w$z*2.23693629
image.plot(as.image.SpatialGridDataFrame(surf.w), asp=1.25)
title(main="Wind speed surface (miles per hour)")
plot(mi, add=TRUE)
points(w.obs$LONG, w.obs$LAT, pch=20)


###################################################
### chunk number 18: Create surface loop
###################################################
t.surfs <- as.data.frame(matrix(NA, length(surf$z), length(dates)))
w.surfs <- t.surfs
for (i in 1:length(dates)) {
  t.obs <- daily.obs[daily.obs$DATE == dates[i] & !is.na(daily.obs$TEMP), c("LONG", "LAT", "TEMP")]
  w.obs <- daily.obs[daily.obs$DATE == dates[i] & !is.na(daily.obs$WIND.SPD), c("LONG", "LAT", "WIND.SPD")]
  ## First, temperature
  surf <- mba.surf(t.obs, 75, 75, extend=TRUE, sp=TRUE, b.box=mba.bbox)$xyz.est
  surf@data <- surf@data * !is.na(overlay(surf, mi))
  surf$z[surf$z == 0] <- NA
  t.surfs[,i] <- surf$z*(9/5)+32
  ## Next, wind speed
  surf <- mba.surf(w.obs, 75, 75, extend=TRUE, sp=TRUE, b.box=mba.bbox)$xyz.est
  surf@data <- surf@data * !is.na(overlay(surf, mi))
  surf$z[surf$z == 0] <- NA
  w.surfs[,i] <- surf$z*2.23693629
}


###################################################
### chunk number 19: Plot-wind-sd
###################################################
wind.sd <- apply(t.surfs, 1, mean)
surf$z <- wind.sd
par(mfrow=c(2,2))
image.plot(as.image.SpatialGridDataFrame(surf), asp=1.25)
title(main="Mean daily temperature (deg F)")
surf$z <- apply(t.surfs, 1, sd)
image.plot(as.image.SpatialGridDataFrame(surf), asp=1.25)
title(main="St.Dev. of daily temperature (deg F)")
surf$z <- apply(w.surfs, 1, mean)
image.plot(as.image.SpatialGridDataFrame(surf), asp=1.25)
title(main="Mean daily wind speed (mph)")
surf$z <- apply(w.surfs, 1, sd)
image.plot(as.image.SpatialGridDataFrame(surf), asp=1.25)
title(main="St.Dev. of daily wind speed (mph)")



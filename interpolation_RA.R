

columns <- c("TEMP", "DEW.POINT")
index <- NULL
index.count <- as.data.frame(matrix(nrow=1, ncol=length(columns)))
colnames(index.count) <- columns
for (L in 1:length(st.interest))
{
  for (i in 1:length(columns))
  {
    index <- which(st.interest[[L]][columns[i]]==999.9)
    index.count[L,i] <- length(index)
    for (j in 1:length(index))
    {
      st.interest[[L]][,columns[i]][index][j] <- st.interest[[L]][,columns[i]][index-1][j]
    }
  }
}
index.count
for(i in 1:length(st.interest)){print(max(st.interest[[i]]["TEMP"]))}

temp <- st.interest[[1]][c(2,4,5,6,7,8,10,11,13,29,31,35,36,37)]
colnames(temp)


clean.df <- ddply(temp, .(city, USAFID, rank, YR, M, D, HR), summarise ,LAT=mean(LAT),
                  LONG=mean(LONG), ELEV=mean(ELEV),
                  TEMP=mean(TEMP), DEW.POINT=mean(DEW.POINT))


hourly <- data.frame(hours=seq(
  from=as.POSIXct("2011-1-1 0:00", tz="UTC"),
  to=as.POSIXct("2013-12-31 23:00", tz="UTC"),
  by="hour"
))

p.dates <- data.frame(dates=as.POSIXct(paste(paste(temp$YR,"-",temp$M,"-",temp$D,
        " ",temp$HR,sep=""),":",0,":",0,sep=""),"%Y-%m-%d %H:%M:%S", tz="UTC"))

clean.df$dates <- p.dates$dates

df.merge <- merge(hourly, clean.df, by.x="hours", by.y="dates", all.x=TRUE)
df.merge$city <- unique(df.merge$city)[2]
df.merge$USAFID <- unique(df.merge$USAFID)[2]
df.merge$rank <- unique(df.merge$rank)[2]
df.merge$LAT <- unique(df.merge$LAT)[2]
df.merge$LONG <- unique(df.merge$LONG)[2]
df.merge$ELEV <- unique(df.merge$ELEV)[2]
df.merge$YR <- as.numeric(format(df.merge$hours,"%Y"))
df.merge$M <- as.numeric(format(df.merge$hours,"%m"))
df.merge$D <- as.numeric(format(df.merge$hours,"%d"))
df.merge$HR <- as.numeric(format(df.merge$hours,"%H"))


temp.int <- approx(x = df.merge$hours, y = df.merge$TEMP, xout = df.merge$hours)
df.merge$TEMP <- temp.int$y

dew.int <- approx(x = df.merge$hours, y = df.merge$DEW.POINT, xout = df.merge$hours)
df.merge$DEW.POINT <- dew.int$y

df.merge[1:6,c(12,13)] <- df.merge[7,c(12,13)]

setwd("~/Desktop/Temp_Data/")
write.csv(df.merge, "Nairobi_Hourly_Temp_interpolated_.csv", row.names=FALSE)

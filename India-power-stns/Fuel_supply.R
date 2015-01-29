options(stringsAsFactors=FALSE)
Mons<-c("apr","may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar")
Yr<-c("11","12","13")
UniqueMons<-outer(Mons,Yr, paste, sep="")
#dfname<-c(1:length(UniqueMons))  #define vector of dfnames
dfnames<-paste("coal",UniqueMons, sep=".")
theColNames<-c("Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","<7days","<4days","Reason")
#DF<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=15, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames)

#for (i in 1:length(UniqueMons)){
getData <- function(i) {
  print(i)
  theFile <- paste("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/",UniqueMons[i],".csv", sep="")
  if (!file.exists(theFile)) next
  dfname<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=15, header=FALSE, check.names=TRUE, comment="@", col.names=theColNames)

  #dfname <- subset(dfname, as.numeric(as.character(Stn_num)) > 0) #remove all rows that are not associated with a station number

  dfname <- subset(dfname, !is.na(as.numeric(Stn_num)) ) #remove all rows that are not associated with a station number
  
  dfname<-droplevels(dfname)
  dfname$Capacity<-as.numeric(dfname$Capacity)
  dfname$Norm_Stock_Days<-as.numeric(dfname$Norm_Stock_Days)
  dfname$Daily_Req_MT<-as.numeric(dfname$Daily_Req_MT)
  dfname$Act_Stock_MT<-as.numeric(dfname$Act_Stock_MT)
  dfname$Act_Stock_Days<-as.numeric(dfname$Act_Stock_Days)
  dfname$Stn_num<-as.numeric(DF$Stn_num)
  
  n<-dim(dfname)[1]
  for (j in 1:n){
    if(dfname$Act_Stock_Days[j]<7) {dfname$Critical[j]=1} else 
      (dfname$Critical[j]=0)
  }
  
  #if(i==1) {DF <- dfname} else {DF <- rbind(DF,dfname)}
  dfname
}

require(plyr)
DF<-ldply(as.list(1:length(UniqueMons)), getData)

if (any(is.na(dfname)) warn("there are NA's" ,.immed=TRUE)
    

dfn#   # number of days reported in month
#   dim(dfname[i])[1]/length(levels(dfname[i]$Stn_name))
#   # number of stations reported
#   length(levels(dfname[i]$Stn_name))
#   # number of stations reported
#   length(levels(dfname[i]$Stn_num))
  
}

#Fuel Supply data
coal<-read.table(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/apr11.csv",sep=",",strip.white=TRUE, blank.lines.skip=TRUE,fill=TRUE, skip=15, header=FALSE, check.names=TRUE)

names(coal)<-c("Date","Stn_num","Transport","Stn_name","Capacity","Norm_Stock_Days","Daily_Req_MT","Act_Stock_MT","Act_Stock_Days","<7days","<4days","Reason")

coal <- subset(coal, as.numeric(as.character(Stn_num)) > 0) #remove all rows that are not associated with a station number
which(is.na(coal))
coal<-droplevels(coal)
coal$Capacity<-as.numeric(coal$Capacity)
coal$Norm_Stock_Days<-as.numeric(coal$Norm_Stock_Days)
coal$Daily_Req_MT<-as.numeric(coal$Daily_Req_MT)
coal$Act_Stock_MT<-as.numeric(coal$Act_Stock_MT)
coal$Act_Stock_Days<-as.numeric(coal$Act_Stock_Days)

n<-dim(coal)[1]
for (i in 1:n){
  if(coal$Act_Stock_Days[i]<7) {coal$Critical[i]=1} else (coal$Critical[i]=0)
  }

# number of days reported in month
dim(coal)[1]/length(levels(coal$Stn_name))
# number of stations reported
length(levels(coal$Stn_name))
# number of stations reported
length(levels(coal$Stn_num))

save(coal, file="coal_supply.rsav")
#############################
############################
test<-read.table(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Fuel Supply/csv_data/coal/apr11.csv",sep=",",strip.white=TRUE, blank.lines.skip=TRUE,fill=TRUE, skip=15, header=FALSE, check.names=TRUE)
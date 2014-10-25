
install.packages("XLConnect")
install.packages("lubridate")


require(reshape)
require(lubridate)



# Save all work now!  
# Loading this crashed my R session. 
require(XLConnect) 

# Set your working directory where you have the files saved
setwd("/Users/mrp/Downloads/")

# Get a list of the files to load in
files <- list.files(getwd(),pattern=".xls")
files

# Loop through the files, then using rbind() to combine them one by one
# The end dataframe will be called 'combined.df'
combined.df <- demand.data <- data.frame()
for (i in 1:length(files))
{
  demand.data <- readWorksheet(loadWorkbook(files[i]), 
                             sheet=1, startCol=1, endCol=8, startRow=2, endRow=51, header=TRUE)
  demand.data <- demand.data[-1,]
  colnames(demand.data)[1] <- "Time"
  
  temp.df <- melt(demand.data, id="Time")
  colnames(temp.df)[2:3] <- c("Date", "Demand")
  
  temp.df$Date <- as.character(temp.df$Date)
  temp.df$Date <- substr(temp.df$Date, 2,11)
  temp.df$Date <- dmy(temp.df$Date)
  
  combined.df <- rbind(combined.df , temp.df)
  assign("combined.df", combined.df)
}


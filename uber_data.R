library(plyr)
library(ggplot2)

#############################################################################


jul_dat <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv", stringsAsFactors = FALSE)

jul_dat$Date.Time <- as.POSIXct(jul_dat$Date.Time, format = "%m/%d/%Y %H:%M:%S") # convert character to time

# feature extraction
print("Data preprocessing ...")

# Function to get rides each day of the week
jul_dat$Date <- as.Date(jul_dat$Date.Time)
jul_dat$hr <- format(jul_dat$Date.Time, "%H") # gives hour of ride in string
jul_dat$hr_map_r <- factor(floor(as.numeric(jul_dat$hr)/24*8))
jul_dat$hr_map <- mapvalues(jul_dat$hr_map_r,
                                     from = 0:7,
                                     to = c("0AM-3AM",
                                            "3AM-6AM",
                                            "6AM-9AM",
                                            "9AM-12AM",
                                            "12AM-15AM",
                                            "15AM-18AM",
                                            "18AM-21AM",
                                            "21AM-24AM"))
  
  
jul_dat$wd <- weekdays(jul_dat$Date) # gives weekday

tt <- table(jul_dat$Date)
tt_names <- names(tt) # already ordered
first_indices <- rep(0,length(tt_names)) # first indices contain the changepoints
ll=length(tt_names)



i=1
for (t in tt_names){
  first_indices[i] <- as.numeric(rownames(jul_dat[jul_dat$Date==t,][1,]))
  print(paste(i/ll*100, "percent of data preprocessing done ..."))
  i=i+1
}

head(tt)
head(jul_dat)
summary(jul_dat)

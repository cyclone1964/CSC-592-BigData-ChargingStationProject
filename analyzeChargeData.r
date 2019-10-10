# This is a simple script to try to analyze the data in the charning database
#
# First, use the lubridate library: this lets us parse times directory in the CSV
library('lubridate')

# Read the data
chargeData = read.csv("AllData.csv", stringsAsFactors=FALSE)

goodIndices = which(chargeData$User.ID != "")
chargeData = chargeData[goodIndices,]

# Parse the start time column
startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

# Find the ones that are in EDT and UTC
edtIndices = which(chargeData$Start.Time.Zone=="EDT")
startTime[edtIndices] = startTime[edtIndices] + 3600
endTime[edtIndices] = endTime[edtIndices] + 3600

# 

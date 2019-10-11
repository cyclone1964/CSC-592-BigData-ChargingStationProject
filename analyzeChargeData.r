#heavyHitters - this is the heavy hitters algorithm as described in Misra-Griegs
heavyHitters <- function(hitterList,numHitters) {
  # The killer here is that R does not natively support a dictionary, 
  # at least not really. So I am going to use named lists for the 
  # same thing. But since the names are not hashed, this means there's a lot
  # of searching for indices to remove etc, so this is only efficient for a 
  # low number of top hitters
  hitCount = list()
  
  for (hitter in hitterList) {
    name = as.character(hitter)
    index = which(names(hitCount) == name)
    if (length(index) > 0) {
      hitCount[index] = as.integer(hitCount[index]) +1
    } else  if (length(hitCount) < numHitters) {
      hitCount[name] = 1
    } else {
      hitCount[names(hitCount)] = as.integer(hitCount[names(hitCount)]) -1 
      hitCount[which(hitCount == 0)] = NULL
    }
  }
  
  return (hitCount)
}
# This is a simple script to try to analyze the data in the charning database
#
# First, use the lubridate library: this lets us parse times directory in the CSV
library('lubridate')

# Read the data
chargeData = read.csv("RI-ChargingData-2019-10-10.csv", stringsAsFactors=FALSE)

goodIndices = which(chargeData$User.ID != "")
chargeData = chargeData[goodIndices,]

# Parse the start time column
startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

# Find the ones that are in EDT and UTC
edtIndices = which(chargeData$Start.Time.Zone=="EDT")
startTime[edtIndices] = startTime[edtIndices] + 3600
endTime[edtIndices] = endTime[edtIndices] + 3600

# HeavyHitters for station and users (frequency)
numHitters = 50
startTime = Sys.time()
stationHitters = heavyHitters(chargeData$Station.Name,50)
endTime = Sys.time()
heavyHittersTime = endTime - startTime

# Now, let's get the count for EVERY one of them
startTime = Sys.time()
hitCount = list()
for (name in chargeData$Station.Name) {
  hitCount[name] = length(which(chargeData$Station.Name == name))
}
endTime = Sys.time()
exhaustiveTime = endTime - startTime

# Compare them
hitCount = hitCount[order(unlist(hitCount),decreasing = TRUE)]
stationHitters = stationHitters[order(unlist(stationHitters),decreasing=TRUE)]

hitCount = hitCount[1:10]
stationHitters = stationHitters[1:10]

print("Exhaustive Search Identifies The Top Ten:")
for (name in names(hitCount)){
  print(paste(name,": ",hitCount[name]))
}
print(paste("Run Time: ",as.numeric(exhaustiveTime)))
      
print("HeavyHitters Identifies The Top Ten:")
for (name in names(stationHitters)){
  print(paste(name,": ",hitCount[name]))
}
print(paste("Run Time: ",as.numeric(heavyHittersTime)))
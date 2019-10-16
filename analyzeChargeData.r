#heavyHitters - this is the heavy hitters algorithm as described in Misra-Griegs
heavyHitters <- function(hitterList,numHitters) {
  # The killer here is that R does not natively support a dictionary, 
  # at least not really. So I am going to use named lists for the 
  # same thing. I have no idea how they are implemented but it may not be the 
  # most efficient storate methodology.
  
  # Initialize the hitCounts
  hitCount = list()

  # Now, for every hitter in the incoming list ...  
  for (hitter in hitterList) {
    # ... Convert it to a character (it's some weird factor thing)...
    name = as.character(hitter)
    
    # ... and find which of the current hitCounts that corresponds to.
    index = which(names(hitCount) == name)
    
    if (length(index) > 0) {
      # If this name exists in the list, increment the list
      hitCount[index] = as.integer(hitCount[index]) +1
    } else  if (length(hitCount) < numHitters) {
      # If not, but there are not too many hitters, 
      # add it to the list with a count of 1
      hitCount[name] = 1
    } else {
      # Otherwise, decrement all the counts and remove the 0 ones
      hitCount[names(hitCount)] = as.integer(hitCount[names(hitCount)]) -1 
      hitCount[which(hitCount == 0)] = NULL
    }
  }

  # Return the list  
  return (hitCount)
}
# This is a simple script to try to analyze the data in the charging database
# At this point it's just testing the heavyhitters algorithm on our data

# First, use the lubridate library: this lets us parse times directory in the CSV
library('lubridate')

# Read the data
chargeData = read.csv("RI-ChargingData-2019-10-10.csv", stringsAsFactors=FALSE)

# Now, get rid of the entries with no User.ID
goodIndices = which(chargeData$User.ID != "")
chargeData = chargeData[goodIndices,]

# Parse the start and end time columns
startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

# Find the ones that are in EDT and add an hour
edtIndices = which(chargeData$Start.Time.Zone=="EDT")
startTime[edtIndices] = startTime[edtIndices] + 3600
endTime[edtIndices] = endTime[edtIndices] + 3600

# Now invoke the heavy hitters algorithm. We ask for the top 50 then 
# whittle it down to the top 10 for analysis. Let's time how long this takes
numHitters = 50
startTime = Sys.time()
stationHitters = heavyHitters(chargeData$Station.Name,50)
endTime = Sys.time()
heavyHittersTime = endTime - startTime

# Now, let's get the hit count for EVERY one of them
# using an exhaustive search, and time how long THAT takes
startTime = Sys.time()
hitCount = list()
for (name in chargeData$Station.Name) {
  index=which(names(hitCount)==name)
  if (length(index) ==1) {
    hitCount[name] = as.numeric(hitCount[name]) + 1
  } else {
    hitCount[name] = 0
  }
}
endTime = Sys.time()
exhaustiveTime = endTime - startTime

# Sort the both and get the top 10
hitCount = hitCount[order(unlist(hitCount),decreasing = TRUE)]
stationHitters = stationHitters[order(unlist(stationHitters),decreasing=TRUE)]

hitCount = hitCount[1:10]
stationHitters = stationHitters[1:10]

# Print all that stuff
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
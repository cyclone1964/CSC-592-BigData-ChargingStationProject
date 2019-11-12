##makeCommunities - streaming community generator as described in Hollocau et al
##
## edgeList - an M x 2 list of edges, each entry is an integer.
## vMax - the maximum volume
makeCommunities <- function(edgeList, vMax, numVertices) {
    ## This is meant to be an R implementation as ssen on page 4.

    ## Initialize communities, volumes, and degrees
    communities = rep(0,numVertices)
    volumes = rep(0,numVertices)
    degrees = rep(0,numVertices)

    ## The next community index
    nextCommunity = 1

    ## Now process all edges. This is probably syntactically wrong
    for (edgeIndex in 1:dim(edgeList)[1]) {

        ## Get the two vertex indices from the selected edge
        iVertex = edgeList[edgeIndex,1]
        jVertex = edgeList[edgeIndex,2]

        ## If either of them is new, add it to their own community.
        if (communities[iVertex] == 0) {
            communities[iVertex] = nextCommunity
            nextCommunity = nextCommunity + 1
        }
        if (communities[jVertex] == 0) {
            communities[jVertex] = nextCommunity
            nextCommunity = nextCommunity + 1
        }

        ## Increase the degree of each vertex
        degrees[iVertex] = degrees[iVertex] + 1
        degrees[jVertex] = degrees[jVertex] + 1

        ## Update the volume of the communities of the two guys
        volumes[communities[iVertex]] = volumes[communities[iVertex]] + 1
        volumes[communities[jVertex]] = volumes[communities[jVertex]] + 1

        ## See if we want to combine the two communities
        if (volumes[communities[iVertex]] <= vMax &&
            volumes[communities[jVertex]] <= vMax) {
            ## i joins the community of j
            if (volumes[communities[iVertex]] <=
                volumes[communities[jVertex]]) {
                volumes[communities[jVertex]] =
                    volumes[communities[jVertex]] +
                    degrees[iVertex]
                volumes[communities[iVertex]] =
                    volumes[communities[iVertex]] -
                    degrees[iVertex]
                communities[iVertex] = communities[jVertex]
            } else {
                ## j joins the community of i
                volumes[communities[iVertex]] =
                    volumes[communities[iVertex]] +
                    degrees[jVertex]
                volumes[communities[jVertex]] =
                    volumes[communities[jVertex]] -
                    degrees[jVertex]
                communities[jVertex] = communities[iVertex]
            }
        }
    }

    return(communities)
}
##heavyHitters - heavy hitters algorithm as described in Misra-Griegs
heavyHitters <- function(hitterList,numHitters,numReturn = -1) {
    
    ## The killer here is that R does not natively support a
    ## dictionary, at least not really. So I am going to use named
    ## lists for the same thing. I have no idea how they are
    ## implemented but it may not be the most efficient storate
    ## methodology.
    
    ## Initialize the hitCounts
    hitCount = list()
    
    ## Now, for every hitter in the incoming list ...  
    for (hitter in hitterList) {
        ## ... Convert it to a character (it's some weird factor thing)...
        name = as.character(hitter)
        
        ## ... and find which of the current hitCounts that corresponds to.
        index = which(names(hitCount) == name)
        
        if (length(index) == 1) {
            ## If this name exists in the list, increment the list
            hitCount[name] = as.integer(hitCount[name]) + 1
        } else  if (length(hitCount) < numHitters) {
            ## If not, but there are not too many hitters, 
            ## add it to the list with a count of 1
            hitCount[name] = 1
        } else {
            ## Otherwise, decrement all the counts and remove the 0 ones
            hitCount[names(hitCount)] =
                as.integer(hitCount[names(hitCount)]) -1 
            hitCount[which(hitCount == 0)] = NULL
        }
    }
    
    ## Sort and reduce the lis as needed
    hitCount = hitCount[order(unlist(hitCount),decreasing=TRUE)]
    if (numReturn > 0) {
        hitCount = hitCount[1:numReturn]
    }
    
    ## Return the list
    return (hitCount)
}
## This function computes the counts for each unique data intpu
countElements <-  function(elements,numReturn = -1) {
    count = list()
    for (name in elements) {
        index=which(names(count)==name)
        if (length(index) ==1) {
            count[name] = as.numeric(count[name]) + 1
        } else {
            count[name] = 1
        }
    }
    
    count = count[order(unlist(count),decreasing=TRUE)]
    
    if (numReturn > 0) {
        count = count[1:numReturn]
    }
    return(count)
}

## This is a simple script to try to analyze the data in the charging
## database At this point it's just testing the heavyhitters algorithm
## on our data First, use the lubridate library: this lets us parse
## times directory in the CSV
library('lubridate')

## Read the data
chargeData = read.csv("RI-ChargingData-2019-10-10.csv", stringsAsFactors=FALSE)

## Now, get rid of the entries with no User.ID
goodIndices = which(chargeData$User.ID != "")
chargeData = chargeData[goodIndices,]

## Parse the start and end time columns
startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

## Find the ones that are in EDT and add an hour
edtIndices = which(chargeData$Start.Time.Zone=="EDT")
startTime[edtIndices] = startTime[edtIndices] + 3600
endTime[edtIndices] = endTime[edtIndices] + 3600

if (0) {
## Now invoke the heavy hitters algorithm. We ask for the top 50 then 
## whittle it down to the top 10 for analysis. Let's time how long this takes
stationHitters = heavyHitters(chargeData$Station.Name,50, 10)

## Now, let's get the hit count for EVERY one of them
## using an exhaustive search, and time how long THAT takes
stationCount = countElements(chargeData$Station.Name, 10)

## Print all that stuff
print("Exhaustive Search Identifies The Top Ten Stations:")
for (name in names(stationCount)) {
  print(paste(name,": ",stationCount[name]))
}
print("")      
print("HeavyHitters Identifies The Top Ten Stations:")
for (name in names(stationHitters)){
  print(paste(name,": ",stationHitters[name]))
}

## Now let's try the same thing with users
userCount = countElements(chargeData$User.ID,10)
userHitters = heavyHitters(chargeData$User.ID,50,10)


print("Exhaustive Search Identifies Top Ten Users  :")
for (name in names(userCount)) {
  print(paste(name,": ",userCount[name]))
}
print("")      
print("HeavyHitters Identifies The Top Ten Users:")
for (name in names(userHitters)){
  print(paste(name,": ",userHitters[name]))
}
}

## Now lets set up a matrix for the graph edges. For this one, we
## treat the userIds and station names as vertices: assign them
print("Load adjacency list")
numVertices = 0
vertexNumber = c()
vertexNames = c()
for (userId in unique(chargeData$User.ID)) {
    numVertices = numVertices + 1
    vertexNumber[userId] = numVertices
    vertexNames[numVertices] = userId
}
numUsers = numVertices
for (stationName in unique(chargeData$Station.Name)) {
    numVertices = numVertices + 1
    vertexNumber[stationName] = numVertices
    vertexNames[numVertices] = stationName
}
numStations = numVertices - numUsers
edgeList = matrix(0,nrow = nrow(chargeData), ncol = 2)
for (index in 1:nrow(chargeData)) {
    edgeList[index,1] = vertexNumber[chargeData$User.ID[index]]
    edgeList[index,2] = vertexNumber[chargeData$Station.Name[index]]
}
print(paste("Found ",numUsers," users and ", numStations," Stations"))

## This function returns the community ID for every vertex in the
## list. Note that these need not be sequential
vMax = 70000
vertexCommunities = makeCommunities(edgeList,vMax,numVertices)

## Now, once we have this, we want to print out the list of members of
## each community, or at least the large communities ... or maybe the
## small communities. The output is a list
communityIds = unique(vertexCommunities)
print(paste("Found ",
            length(communityIds),
            " Unique communities at Volume ",
            vMax))

##for (communityId  in communityIds) {
##    vertexIndices = which(vertexCommunities == communityId)
##    numMembers = length(vertexIndices)
##    print(paste("Community Id ",communityId))
##    for (vertexIndex in vertexIndices) {
##        print(paste("    ",vertexNames[vertexIndex]))
##    }
##}    


## Now, that doesn't really do us much good: we get almost as many
## communities as we have nodes. My suspicion is that this is due to
## one of two things: 1) the graph is bi-partite and so does not
## really match the "spoke and hub" model of the algorithm or 2) It is
## way too reptitive: the same people plug into the same stations a
## lot of times, meaning the "volumes" fill up and then can't be
## split.

## To address that second one, let's "uniquefy" the matrix: only one
## copy of each user/station edge.
for (vMax in c(250,500,1000,2500,5000)){
  print(paste("Try vMax : ",vMax))
  uniqueEdgeList = unique(edgeList)
  vertexCommunities = makeCommunities(uniqueEdgeList,vMax,numVertices)
  communityIds = unique(vertexCommunities)
  print(paste("  Found ",
              length(communityIds),
              " Unique communities at Volume ",
              vMax,"With Unique Elements"))

  for (communityId  in communityIds) {
      vertexIndices = which(vertexCommunities == communityId)
      numMembers = length(vertexIndices)
      if (numMembers > 3) {
      print(paste("  Community Id ",
                  communityId,
                  " has ",
                  numMembers,
                  " Members"))
      } 
      if (numMembers >= 10) {
          stationIndices = unique(vertexIndices[which(vertexIndices >= numUsers)])
          print(paste("    And ", length(stationIndices), " Stations"))
          print(vertexNames[stationIndices])
      }
  }
##    for (vertexIndex in vertexIndices) {
##        print(paste("    ",vertexNames[vertexIndex]))
##    }
}    

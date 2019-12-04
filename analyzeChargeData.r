## This is a simple script to try to analyze the data in the charging
## database. These are the libraries we need.
library('lubridate')
library('gridExtra')
library('grid')
library('igraph')
library('zipcode')
library('geosphere')
library('Rtsne')
library('umap')

data(zipcode)

## cleanData - clean the charging station data
##
## This function cleans the input data by doing the following
##
## Removing entries with bad data in them
## Adding a Start.Time and End.Time field 
## Sorts entries by Start.Time
##
## Arguments
## chargeData - the input chargeData frame
##
## returns
##
## chargeData - the cleaned data
cleanData <- function(chargeData) {
    
    ## First, get rid of the entries with missing fields of interest
    print(paste("Raw data set has ",nrow(chargeData), " records"))
    goodIndices = which(chargeData$User.ID != "" &
                        chargeData$User.ID != "0" & 
                        chargeData$End.Date != "" & 
                        chargeData$Start.Date != "" &
                        chargeData$Total.Duration..hh.mm.ss. != "")
    chargeData = chargeData[goodIndices,]
    print(paste("Editing for bad User ID, start and end dates,",
                "and duration leaves ",nrow(chargeData), " records"))
    
    ## Let's sort them by starting time
    print("Sort data by start date")
    startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
    chargeData = chargeData[order(unlist(startTime)),]

    ## take out those that have durations that are too long
    print("Remove durations in ecess of a week or less than 5 minutes")
    duration = hms(chargeData$Total.Duration)
    duration = as.numeric(duration)
    indices = which(duration < 7*24*60*60 & duration > 5*60)
    chargeData = chargeData[indices,]
    print(paste("Editing for bad duration leaves ",
                nrow(chargeData), " records"))

    ## Parse the start and end time columns
    startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
    endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

    ## Also compute the duration in hours
    duration = hms(chargeData$Total.Duration)
    duration = as.numeric(duration)/3600

    ## Add times to the data frame
    chargeData$Start.Time = startTime
    chargeData$End.Time = endTime
    chargeData$Duration = duration
    
    ## Now let's remove all occurences of this string from station names
    chargeData$Station.Name = gsub("NATIONAL GRID / ","",
                                   chargeData$Station.Name)

    ## Now, there are repeats in there due to formatting: take
    ## out the #, ', and spaces
    chargeData$Station.Name = gsub("[# ']","",
                                   chargeData$Station.Name)

    ## Now, let's convert all the zip codes to only 5 digits
    ## by removing anything after a -
    chargeData$Driver.Postal.Code = sub("-.*","",
                                        chargeData$Driver.Postal.Code)

    ## Now let's convert the zip codes to actual numbers and get rid
    ## of ones that fail
    chargeData$Postal.Code = clean.zipcodes(chargeData$Postal.Code)
    chargeData$Driver.Postal.Code = clean.zipcodes(chargeData$Driver.Postal.Code)
    chargeData = chargeData[which(!is.na(chargeData$Driver.Postal.Code)),]
    print(paste("Removing bad zip codes leaves ",nrow(chargeData)," records"))

    ## Now let's remove zipcodes that are not in new england.
    localCodes = zipcode[which(zipcode$state == "RI" |
                               zipcode$state == "MA" |
                               zipcode$state == "CT"),]

    ## Now let's sort the reduced data base entry by zip code
    localCodes = localCodes[order(as.numeric(localCodes$zip)),]

    ## Now find the first and last entry in that list
    temp = as.numeric(localCodes$zip)
    firstCode = temp[1]
    lastCode = temp[length(temp)]

    ## load a matrix with the lat/long
    geoTable = matrix(nrow = lastCode-firstCode+1, ncol=2, byrow = TRUE)
    geoTable[temp-firstCode+1,1] = localCodes$latitude
    geoTable[temp-firstCode+1,2] = localCodes$longitude

    ## Now lets remove any entries in the data base with postal codes
    ## out of that range.
    temp = as.numeric(chargeData$Driver.Postal.Code)
    chargeData = chargeData[which(temp >= firstCode & temp <= lastCode),]
    print(paste("Removing non-New England zip codes leaves ",
                nrow(chargeData)," records"))

    ## now compute the distance by getting the lat and long for the
    ## station and the driver and computing the geodesic.
    stationIndices = as.numeric(chargeData$Postal.Code)-firstCode+1
    userIndices = as.numeric(chargeData$Driver.Postal.Code)-firstCode+1
    stationPoints = geoTable[stationIndices,]
    userPoints = geoTable[userIndices,]
    chargeData$Distance = distGeo(stationPoints,userPoints)
    chargeData$User.Latitude = geoTable[userIndices,1]
    chargeData$User.Longitude = geoTable[userIndices,2]
    chargeData$Station.Latitude = geoTable[stationIndices,1]
    chargeData$Station.Longitude = geoTable[stationIndices,2]

    ## Now get rid of the ones that have NA in them
    chargeData = chargeData[!is.na(chargeData$Distance),]
    print(paste("Removing bad distances leaves",nrow(chargeData), " records"))
    
    ## Now let's enumerate the station and the users
    numVertices = 1
    vertices = matrix(nrow =nrow(chargeData), ncol=1)
    for (name in unique(chargeData$Station.Name)) {
        vertices[which(chargeData$Station.Name == name)] = numVertices
        numVertices = numVertices + 1
    }
    chargeData$Station.Vertex = vertices

    numVertices = 1
    vertices = matrix(nrow =nrow(chargeData), ncol=1)
    for (name in unique(chargeData$User.ID)) {
        vertices[which(chargeData$User.ID == name)] = numVertices
        numVertices = numVertices + 1
    }
    chargeData$User.Vertex = vertices
    
   return(chargeData)
}
##heavyHitters - heavy hitters algorithm as described in Misra-Griegs
##
## This is an implementation of the "Heavy Hitters" algorithm of
## Misra-Griegs.
##
## Arguments:
##
## hitterList - a list of unique names: each nameis a node.
## hitterLimit - the value k in the M-G paper.
## numReturn - the number of entries to return
##
## Returns:
##
## heavyHitters - a named list, with each named entry the count for the entry.
##
heavyHitters <- function(hitterList,hitterLimit,numReturn = -1) {
    
    ## The killer here is that R does not natively support a
    ## dictionary, at least not really. So I am going to use named
    ## lists for the same thing. I have no idea how they are
    ## implemented but it may not be the most efficient storage
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
        } else  if (length(hitCount) < hitterLimit) {
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
    
    ## Sort and reduce the list as needed
    hitCount = hitCount[order(unlist(hitCount),decreasing=TRUE)]
    if (numReturn > 0 && length(hitCount) > numReturn) {
        hitCount = hitCount[1:numReturn]
    }
    
    ## Return the list
    return (hitCount)
}
##countElementNames - countElements in a list 
##
## This function simply counts the occurances of a name in a list. It
## returns a named list in sorted order limited to a specific size if
## so indicated.
##
## Arguments
## elementNames - list of element names
## numReturn - the maximum number to return
##
## Returns
## named list of count for each name.
countElementNames <-  function(elementNames,numReturn = -1) {

    count = list()
    for (name in elementNames) {
        
        ## Does that name exist in the list already?
        index=which(names(count)==name)
        if (length(index) ==1) {
            ## If so, increment the count
            count[name] = as.numeric(count[name]) + 1
        } else {
            ## Otherwise, add it to the list
            count[name] = 1
        }
    }

    ## Sort them in decreasing order
    count = count[order(unlist(count),decreasing=TRUE)]

    ## Downselect the list and return the counts
    if (numReturn > 0 && length(count) > numReturn) {
        count = count[1:numReturn]
    }
    return(count)
}
##computeLoading - compute the loading for stations in a single pass
##
## This function returns the loading for the stations in the
## chargeData data frame in a streaming fashion, which is assumed to
## have the following fields:
##
## Station.Name - the unique name of each station
## Start.Time - the start time of each session
## End.Time - the end time of each session
##
## Arguments
## chargeData - a data frame of charging data augmented as necessary
##
## Returns
## loading - a named list of loading for each station
computeLoading <- function(chargeData)
{
    loading = c()
    sessions = c()
    firstTime = c()
    for (index in seq(1,nrow(chargeData))) {
        name = as.character(chargeData$Station.Name[index])
        if (length(which(name == names(loading))) == 0) {
            loading[name] = difftime(chargeData$End.Time[index],
                                     chargeData$Start.Time[index],
                                     units = "hours")
            firstTime[name] = index
            sessions[name] = 1
        } else {
            loading[name] = loading[name] +
                difftime(chargeData$End.Time[index],
                         chargeData$Start.Time[index],
                         units = "hours")
            sessions[name] = sessions[name] + 1
        }
    }
    for (name in names(loading)) {
        totalTime = difftime(chargeData$End.Time[nrow(chargeData)],
                             chargeData$Start.Time[firstTime[name]],
                             units = "hours")
        loading[name] = loading[name]/as.numeric(totalTime)
        sessions[name] = sessions[name]/(as.numeric(totalTime)/24)
    }
    loading = loading[order(unlist(loading),decreasing=TRUE)]
    sessions = sessions[order(unlist(sessions),decreasing=TRUE)]
    temp = c()
    temp$Loading = loading
    temp$Sessions = sessions
    return(temp)
}
## quickHitters - look for quick hitters
##
## this funtion finds "quick hitters" in the sense that it counts the
## number sessions that follow within a few moments of another
## session., in this case an hour by default.
##
## Arguments:
## chargeData - the chargeData DataFrame
## period - the time limit for declaring a quick hit
##
## Returns
## a named list of counts of quick hits
quickHitters <- function(chargeData, period = 1) {

    ## Count the times when it gets used in quick success.
    ## lastTIme holds the last ending time for a station.
    ## quickHits holds the list of quick hits
    lastTime = c()
    quickHits = c()
    for (index in seq(1,nrow(chargeData))) {

        ## Get the staion name as a list lookup
        stationName = as.character(chargeData$Station.Name[index])

        ## If it's not there, add it, otherwise compute the length of
        ## the time between sessions and check to see if it sover the
        ## period.
        if (length(which(names(lastTime) == stationName)) == 0) {
            lastTime[stationName] = index
        } else {
          difference = difftime(chargeData$Start.Time[index],
                                chargeData$End.Time[lastTime[stationName]],
                                units='hours')
           if (difference < period) {
                quickHits = c(quickHits,stationName)
                
           }
          lastTime[stationName] = index
        }
    }

    return(quickHits)
}
##buildEdgeList - build a list of edges
##
## This function builds a list of edges and returns them along with some
## bookkeeping information
##
## Arguments:
## chargeData - the input data frame
##
## Returns:
## named list with the following entries:
##
## "edgeList" - an edge list, an Nx2 matrix of vertex integers
## "vertexNames" - a vector of names for the vertexes
## "vertexIndices" - named list of indices for each vertex
buildEdgeList <- function(chargeData) {
    
    ## Now lets set up a matrix for the graph edges. For this one, we
    ## treat the userIds and station names as vertices: assign them
    print("Load adjacency list")
    numVertices = 0
    vertexNames = c()
    vertexNumbers = c()

    for (stationName in unique(chargeData$Station.Name)) {
        numVertices = numVertices + 1
        vertexNumbers[stationName] = numVertices
        vertexNames[numVertices] = stationName
    }
    numStations = numVertices
    for (userId in unique(chargeData$User.ID)) {
      numVertices = numVertices + 1
      vertexNumbers[userId] = numVertices
      vertexNames[numVertices] = userId
    }
    numUsers = numVertices - numStations
    edgeList = matrix(0,nrow = nrow(chargeData), ncol = 2)
    for (index in 1:nrow(chargeData)) {
        edgeList[index,1] = vertexNumbers[chargeData$User.ID[index]]
        edgeList[index,2] = vertexNumbers[chargeData$Station.Name[index]]
    }
    print(paste("Found ",numUsers," users and ", numStations," Stations"))
    temp = c()
    temp$Edge.List = edgeList
    temp$Vertex.Names = vertexNames
    temp$Vertex.Numbers = vertexNumbers
    return(temp)
}
##makeCommunities - streaming community generator as described in Hollocau et al
##
##
## Arguments
## edgeList - an M x 2 list of edges, each entry is an integer.
## vMax - the maximum volume
## numVertices - the number of unique vertices in the edgeList
##
## Returns
## communities - a named list for each vertex of which community they belong
makeCommunities <- function(edgeList, vMax, numVertices) {

    ## This is meant to be an R implementation as seen on page 4.
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

                ## This is an attempt to hold the communities
                communities[iVertex] = communities[jVertex]
                ##communities[which(communities == communities[iVertex])] =
                  ##communities[jVertex]
            } else {
                ## j joins the community of i
                volumes[communities[iVertex]] =
                    volumes[communities[iVertex]] +
                    degrees[jVertex]
                volumes[communities[jVertex]] =
                    volumes[communities[jVertex]] -
                    degrees[jVertex]

                ## This is an attempt to hold the communities
                communities[jVertex] = communities[iVertex]
                ##communities[which(communities == communities[jVertex])] =
                ##  communities[iVertex]
            }
        }
    }

    return(communities)
}
## Now to support the PCA analysis, we have to create a matrix for the data elements for which that makes sense. Specifically: start hour of the day, duration, distance from home, total energy, 

## Now, the actual program: first, read the charge data if necessary
if (!exists("chargeData")) {

    ## Read the data
    chargeData = read.csv("RI-ChargingData-2019-10-10.csv",
                          stringsAsFactors=FALSE)
    chargeData = cleanData(chargeData)
}

## This is the station heavy hitter analysis and associated plots
if (!exists("stationHitters")) {
    
    ## Now invoke the heavy hitters algorithm. We ask for the top 10
    ## then whittle it down to the top 10 for analysis. 
    print("Run Misra-Gries looking for only the top 10 stations")
    stationHitters = heavyHitters(chargeData$Station.Name, 10, 10)
    stationCount = countElementNames(chargeData$Station.Name, 10)
    
    ## Now, let's combine them into a table for plotting
    allNames = unique(c(names(stationCount),names(stationHitters)))
    print('Station: M-G naive');
    tableData = matrix(0,nrow=length(allNames),ncol=2,byrow=TRUE)
    for (index in seq(1,length(allNames))) {
        name = allNames[index]
        print(paste(name,' : ',stationHitters[name],',',stationCount[name]))
        if (hasName(stationCount,name)) {
            tableData[index,1] = as.numeric(stationCount[name])
        }
        if (hasName(stationHitters,name)){
            tableData[index,2] = as.numeric(stationHitters[name])
        }
    }
    rownames(tableData) = allNames
    colnames(tableData) = c("\nCount","Count\nM-G");
    plot.new()
    grid.table(tableData)

    ## Now do it for the hitters with k=50
    stationHitters = heavyHitters(chargeData$Station.Name, 50, 10)
    allNames = unique(c(names(stationHitters),names(stationCount)))
    print('Station: M-G naive');
    tableData = matrix(0,nrow=length(allNames),ncol=2,byrow=TRUE)
    for (index in seq(1,length(allNames))) {
        name = allNames[index]
        print(paste(name,' : ',stationHitters[name],',',stationCount[name]))
        if (hasName(stationCount,name)) {
            tableData[index,1] = as.numeric(stationCount[name])
        }
        if (hasName(stationHitters,name)){
            tableData[index,2] = as.numeric(stationHitters[name])
        }
    }
    rownames(tableData) = allNames
    colnames(tableData) = c("\nCount","Count\nM-G");
    plot.new()
    grid.table(tableData)

    ## Now try it for different values of k
    tableData = matrix(0,nrow=5,ncol=2,byrow=TRUE)
    for (index in seq(1,5)) {
      numBins = index * 10
      stationHitters = heavyHitters(chargeData$Station.Name, numBins, 10)
      stationCount = countElementNames(chargeData$Station.Name, 10)
      
      numMatches = 0
      for (name in names(stationCount)) {
        if (length(which(name == names(stationHitters))) == 1) {
          numMatches = numMatches + 1
        }
      }
      
      tableData[index,1] = numBins
      tableData[index,2] = round(100*numMatches/10)
      print(paste('numBins: ',numBins,'Match',numMatches/10))
    }
    colnames(tableData) <-  c("k","Top 10 \nAccuracy (%)")
    plot.new()
    grid.table(tableData)
}

## Now let's try the same thing with users: however, we need to do
## this for different settings of k since there are so many users
if (!exists("userHitters")) {
    print("Use Misra-Gries at various settings of k on users")
    tableData = matrix(0,ncol=2,nrow=10,byrow = TRUE)
    for (index in seq(1,10)) {
        numBins = 50 * index
        userCounts = countElementNames(chargeData$User.ID,numBins)
        userHitters = heavyHitters(chargeData$User.ID,numBins,numBins)
        
        ## compute the number of matches: we do this by catenating the
        ## names in both, uniquing that, and the number that the
        ## unique removed are the number that don't match
        numMatches = 0
        for (name in names(userCounts)) {
            if (length(which(name == names(userHitters))) == 1) {
                numMatches = numMatches + 1
            }
        }
        tableData[index, 1] = numBins
        tableData[index, 2] = round(100*numMatches/numBins)
        print(paste('NumBins: ',numBins,'Match',numMatches/numBins))
    }
    colnames(tableData) <-  c("k","Accuracy (%)")
    plot.new()
    grid.table(tableData)
}
## Now let's compute the business of the stations, specifically the
## average number of session per day and the loading (percentage of
## time) it is used
if (!exists("sessionsPerDay")) {

    ## Compute the running sums
    print("Compute loading of stations")
    stationCounts = countElementNames(chargeData$Station.Name,10)
    temp = computeLoading(chargeData)
    stationLoading = temp$Loading
    sessionsPerDay = temp$Sessions

    ## Having done, that, let's look at the station heavy hitters and
    ## determine how "saturated" they are, which is to say how much of the
    ## time they are full. We can do this easily by summing up the
    ## "duration" column.
    tableData = matrix(0,nrow = length(stationCounts),ncol=1,byrow=TRUE)
    stationNames = names(stationCounts)
    for (index in seq(1,length(stationNames))) {
        tableData[index,1] = round(100*stationLoading[stationNames[index]])
    }
    rownames(tableData) = stationNames
    colnames(tableData) = c("Loading (%)")
    plot.new()
    grid.table(tableData)
    print(" Notice that some of these are really REALLY high")

    plot(sessionsPerDay,
         type='o',
         ylab = 'Sessions Per Day', 
         main = 'Station Usage (Sorted)')
}

## Now, let's try finding some communities using the algorithm. To do
## this we need to build a graph, which is in this case is an
## undirected multi-graph. with each Station and User a vertex and an
## edge between them when there's a session.
if (!exists("edgeList")) {

    temp = buildEdgeList(chargeData)
    edgeList = temp$Edge.List
    vertexNames = temp$Vertex.Names
    vertexIndices = temp$Vertex.Indices
    numVertices = length(vertexNames)

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
    
    ## Now, that doesn't really do us much good: we get almost as many
    ## communities as we have nodes. My suspicion is that this is due to
    ## one of two things: 1) the graph is bi-partite and so does not
    ## really match the "spoke and hub" model of the algorithm or 2) It is
    ## way too reptitive: the same people plug into the same stations a
    ## lot of times, meaning the "volumes" fill up and then can't be
    ## split.
    
    ## To address that second one, let's "uniquefy" the matrix: only one
    ## copy of each user/station edge. While we are at it, let's compute the
    ## count of each unique edge
    uniqueEdgeList = unique(edgeList)
    uniqueEdgeCount = matrix(0,nrow=nrow(uniqueEdgeList),ncol=1)
    for (index in seq(1,nrow(uniqueEdgeList))) {
        uniqueEdgeCount[index] =
            length(which(edgeList[,1] == uniqueEdgeList[index,1] &
                         edgeList[,2] == uniqueEdgeList[index,2]))
    }

    vmaxList = c(10,25,50,100,250,500,1000,2500,5000,10000,20000,40000, 80000)
    tableData = matrix(0,nrow = length(vmaxList),ncol=2,byrow=TRUE)

    for (index in seq(1,length(vmaxList))) {
        vMax = vmaxList[index]
        vertexCommunities = makeCommunities(uniqueEdgeList,vMax,numVertices)
        communityIds = unique(vertexCommunities)
        tableData[index,1] = vMax
        tableData[index,2] = length(communityIds)
        
        ## Now, let's compute the ratio of intra-community edges to inter
        ## community edges
        numIntraEdges = length(which(vertexCommunities[uniqueEdgeList[,1]] ==
                                     vertexCommunities[uniqueEdgeList[,2]]))
        numInterEdges = nrow(uniqueEdgeList)-numIntraEdges
        print(paste('vMax: ',vMax,
                    ' # Com: ',length(communityIds), 
                    'Ratio: ',numIntraEdges/numInterEdges))
        
    }
    
    colnames(tableData) = c("vMax","# Communities")
    plot.new()
    grid.table(tableData)
}

## Now, lets try some experiments with closeness Centrality.
if (!exists('chargeGraph')) {

    ## First, let's make a graph from the entire edge list
    print("Plot of closeness centrality as a function of month")
    chargeGraph =
        graph(as.vector(t(uniqueEdgeList)),n=numVertices,directed=FALSE)

    ## Now, let's compute the centrality of all the stations across
    ## the entire time (using no weights) and see what that looks
    ## like.
    allCloseness = closeness(chargeGraph,
                             vids = seq(1,length(stationNames)),
                             mode="all",
                             normalized=TRUE)
    plot(allCloseness,
         xlab='Station (Ordered by Busyness)',
         ylab='Closeness Centrality',
         main='Station Closeness Centrality (All Data)');
         
    ## Now, let's compute the centrality of each o the stations on a
    ## monthy basis to see if thre's any sort of trend. 
    monthlyClosenessCentrality = c()
    periodDates = c()
    for (thisYear in seq(year(chargeData$Start.Time[1]),
                         year(chargeData$End.Time[nrow(chargeData)]))) {
        for (thisMonth in seq(1,12)) {
            monthIndices = which(year(chargeData$Start.Time) == thisYear &
                                 month(chargeData$Start.Time) == thisMonth)
            if (length(monthIndices) > 1) {
                monthEdgeList = edgeList[monthIndices,];
                uniqueMonthEdgeList = unique(monthEdgeList)
                monthEdgeCount =
                    matrix(0,nrow = nrow(uniqueMonthEdgeList),ncol=1)
                for (index in seq(1,nrow(uniqueMonthEdgeList))) {
                    monthEdgeCount[index] =
                        length(which(monthEdgeList[,1] ==
                                     uniqueMonthEdgeList[index,1] &
                                     monthEdgeList[,2] ==
                                     uniqueMonthEdgeList[index,2]))
                }
                periodDates = c(periodDates,
                                chargeData$Start.Time[monthIndices[1]])
                temp = graph(as.vector(t(uniqueMonthEdgeList)),
                             n=length(unique(as.vector(uniqueMonthEdgeList))),
                             directed=FALSE)
                monthCloseness = closeness(temp,
                                           vids = seq(1,length(stationNames)),
                                           mode="all",
                                           normalized = TRUE)

                monthlyClosenessCentrality =
                    cbind(monthlyClosenessCentrality,monthCloseness)
            }
        }
    }
    plot.new()
    matplot(t(as.matrix(monthlyClosenessCentrality)),t='l',
            xlab='Month Into Data',ylab='Centrality',
            main='Closeness Centrality of All Stations')
    rm("allCloseness")
    rm("periodDates")
    rm("temp")
    rm("monthEdgeCount")
    rm("monthEdgeList")
    rm("uniqueMonthEdgeList")
    rm("monthCloseness")
    rm("montlyClosenessCentrality")
}

if (!exists('stationUserCount')) {

    ## Let's go through and count the number of unique users for each station
    stationUserCount = c()
    for (stationName in names(sessionsPerDay)) {
        indices = which(chargeData$Station.Name == stationName)
        stationUserCount[stationName] =
            length(unique(chargeData$User.ID[indices]))
    }

    plot(stationUserCount,
         xlab='Station (Sorted)',
         ylab='# Unique Users',
         main='Station Unique User Count')
}

## Now, that's interesting. So the next question is to check the
## promiscuity of users with respect to stations
if (!exists("userStationCount")) {
    ## Let's go through and count the number of unique users for each station
    userStationCount = c()
    for (userId in unique(chargeData$User.ID)) {
        indices = which(chargeData$User.ID == userId)
        userStationCount[userId] =
            length(unique(chargeData$Station.Name[indices]))
    }
    
    data = hist(userStationCount,breaks = seq(0,30),plot=FALSE);
    barplot(pmin(100,data$counts),
            ylab='# of Users ',
            xlab='# Stations',
            names.arg = seq(1,30),
            main='User Station Set Size')
}
## Now let's see if we can identify times when charging stations are
## used in quick succession. We can use this as an indication that a
## specific station is could stand to be upgraded to more slots.
if (!exists("quickHits")) {
    quickHits = quickHitters(chargeData)

    ## Now use MGHH and naive to find most quick hits
    quickCount = countElementNames(quickHits,10)

    allNames = names(quickCount)
    tableData = matrix(0,nrow=length(allNames),ncol=1)
    for (index in seq(1,length(allNames))) {
        name = allNames[index]
        if (length(which(name == names(quickCount))) > 0) {
            tableData[index,1] = as.integer(quickCount[name])
        }
    }
    colnames(tableData) = c("QuickHits")
    rownames(tableData) = allNames
    plot.new()
    grid.table(tableData)
}

## Now, lets do a simple PCA on a reduced data set.
if (!exists("reducedChargeData")) {

    ## First, t-SNE and UMAP embeddings on a 6 dimension set
    reducedChargeData = cbind(round(hour(chargeData$Start.Time)),
                              chargeData$Duration,
                              chargeData$Energy..kWh,
                              chargeData$Distance,
                              chargeData$User.Latitude,
                              chargeData$User.Longitude,
                              chargeData$Station.Latitude,
                              chargeData$Station.Longitude)

    ## Most of the coloring options we implement can be done directly
    ## from the reduced data set, but the coloring by heavy hitter
    ## user id has to be done in the chargeData data space in order to
    ## use the list of userCounts
    colors = rainbow(length(unique(chargeData$Station.Vertex)))
    stationColors = colors[chargeData$Station.Vertex]

    colors = rainbow(length(unique(reducedChargeData[,1])))
    hourColors = colors[reducedChargeData[,1]+1]

    colors = rainbow(length(userCounts)+1)
    userColors=matrix(0,nrow=nrow(chargeData),ncol=1)
    index = 1
    for (name in names(userCounts)) {
          userColors[chargeData$User.ID == name] = colors[index]
          index = index+1
    }
    userColors[userColors == 0] = colors[length(colors)]
    rm("colors")

    ## Now reduce the data by a factor of 16 and set up the colorings
    reducedChargeData = reducedChargeData[seq(1,nrow(reducedChargeData),16),]
    userColors = userColors[seq(1,length(userColors),16)]
    hourColors = hourColors[seq(1,length(hourColors),16)]
    stationColors = stationColors[seq(1,length(stationColors),16)]
    
    ## Now let's do a t-SNE embedding
    if (TRUE) {
        tsneData = Rtsne(reducedChargeData,
                         dims=2,
                         perplexity=50,
                         verbose=TRUE,
                         max_iter = 1000)
        plot(tsneData$Y,t='n',
             main="chargeData t-SNE Embedding (Color By Station)",
             xlabel="X",
             ylabel="Y")
        text(tsneData$Y,
             labels='.',
             cex=5,
             col = stationColors)
        
        plot(tsneData$Y,t='n',
             main="chargeData t-SNE Embedding (Color By Hour)",
             xlabel="X",
             ylabel="Y")
        text(tsneData$Y,
             labels='.',
             cex=5,
             col = hourColors);

        plot(tsneData$Y,t='n',
             main="chargeData t-SNE Embedding (Color By HeavyHitters)",
             xlabel="X",
             ylabel="Y")
        text(tsneData$Y,
             labels='.',
             cex=5,
             col = userColors);
        
        rm("tsneData")
    }
    
    ## And a UMAP embedding
    umapConfig = umap.defaults
    umapConfig$n_neighbors = 50
    if (TRUE) {
        umapData = umap(reducedChargeData,config=umapConfig)
        plot(umapData$layout,
             t='n',
             main="chargeData UMAP Embedding (Color By Station)",
             xlabel="X",
             ylabel="Y")
        text(umapData$layout,
             labels='.',
             cex=5,
             col = stationColors)

        plot(umapData$layout,
             t='n',
             main="chargeData UMAP Embedding (Color By Hour)",
             xlabel="X",
             ylabel="Y")
        text(umapData$layout,
             labels='.',
             cex=5,
             col = hourColors)

        plot(umapData$layout,
             t='n',
             main="chargeData UMAP Embedding (Color By HeavyHitters)",
             xlabel="X",
             ylabel="Y")
        text(umapData$layout,
             labels='.',
             cex=5,
             col = userColors);
        rm("umapData")
    }
}


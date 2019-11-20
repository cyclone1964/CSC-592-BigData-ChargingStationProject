##makeCommunities - streaming community generator as described in Hollocau et al
##
## edgeList - an M x 2 list of edges, each entry is an integer.
## vMax - the maximum volume
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
##heavyHitters - heavy hitters algorithm as described in Misra-Griegs
heavyHitters <- function(hitterList,numHitters,numReturn = -1) {
    
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
    if (numReturn > 0 && length(hitCount) > numReturn) {
        hitCount = hitCount[1:numReturn]
    }
    
    ## Return the list
    return (hitCount)
}
## This function computes the counts for each unique data input by
## simply going through the list and counting each unique entity
countElements <-  function(elements,numReturn = -1) {

    count = list()
    for (name in elements) {
        
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

## This is a simple script to try to analyze the data in the charging
## database At this point it's just testing the heavyhitters algorithm
## on our data First, use the lubridate library: this lets us parse
## times directory in the CSV
library('lubridate')
library('gridExtra')
library('grid')
library('igraph')

if (!exists("chargeData")) {

    ## Read the data
    chargeData = read.csv("RI-ChargingData-2019-10-10.csv", stringsAsFactors=FALSE)
    
    ## Now, get rid of the entries with missing fields of interest
    goodIndices = which(chargeData$User.ID != "" &
                        chargeData$User.ID != "0" & 
                        chargeData$End.Date != "" & 
                        chargeData$Start.Date != "" &
                        chargeData$Total.Duration..hh.mm.ss. != "")
    chargeData = chargeData[goodIndices,]
    
    ## Let's sort them by starting time
    startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
    chargeData = chargeData[order(unlist(startTime)),]

    ## Parse the start and end time columns
    startTime = as.POSIXct(chargeData$Start.Date,format="%m/%d/%y %H:%M")
    endTime = as.POSIXct(chargeData$End.Date,format="%m/%d/%y %H:%M")

    ## Find the ones that are in EDT and add an hour
    edtIndices = which(chargeData$Start.Time.Zone=="EDT")
    startTime[edtIndices] = startTime[edtIndices] + 3600
    endTime[edtIndices] = endTime[edtIndices] + 3600
    
    ## Now let's remove all occurences of this string rom station names
    chargeData$Station.Name = gsub("NATIONAL GRID / ","",
                                   chargeData$Station.Name)

    ## Now, there are repeast in there due to formatting: take
    ## out the #, ', and spaces
    chargeData$Station.Name = gsub("[# ']","",
                                   chargeData$Station.Name)

}
## This is the station heavy hitter analysis
if (!exists("stationHitters")) {
    
    ## Now invoke the heavy hitters algorithm. We ask for the top 10
    ## then whittle it down to the top 10 for analysis. 
    print("First, run Misra-Gries looking for only the top 10 stations")
    
    stationHitters = heavyHitters(chargeData$Station.Name, 10, 10)
    stationCount = countElements(chargeData$Station.Name, 10)
    
    ## Now, let's combine them into a table for plotting
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
    
    print("Notice that there is almost no agreement there.")
    print("This is because M-G does not work well when there are insufficient")
    print("Entries for the top hitters. Try it again with 50 table entries")
    print("and keep just the top 10")
    
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


    print("Notice we have good agreement here.")
    
    print("Now, let's do it for an increasing number of bins")
    tableData = matrix(0,nrow=5,ncol=2,byrow=TRUE)
    for (index in seq(1,5)) {
      numBins = index * 10
      stationHitters = heavyHitters(chargeData$Station.Name, numBins, 10)
      stationCount = countElements(chargeData$Station.Name, 10)
      
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
    rownames(tableData) <- 10*seq(1,5)
    colnames(tableData) <-  c("# Bins","Top 10 \nAccuracy (%)")
    plot.new()
    grid.table(tableData)
}


## Now let's try the same thing with users: however, we need to do
## this for different levels

if (!exists("userHitters")) {
    print("Now, there are only 79 stations in the data base, so picking the")
    print("top 50 was almost exhaustive. Let's do the same thing with users")
    print("of which there are over 4000. Let's use M-G to find the top N")
    print(" users for values of N between 10 and 100 and make a table")
    print(" of the agreement.");
    tableData = matrix(0,ncol=10,nrow=2,byrow = TRUE)
    for (index in seq(1,10)) {
        numBins = 50 * index
        userCount = countElements(chargeData$User.ID,numBins)
        userHitters = heavyHitters(chargeData$User.ID,numBins,numBins)
        
        ## compute the number of matches: we do this by catenating the
        ## names in both, uniquing that, and the number that the
        ## unique removed are the number that don't match
        numMatches = 0
        for (name in names(userCount)) {
            if (length(which(name == names(userHitters))) == 1) {
                numMatches = numMatches + 1
            }
        }
        tableData[1, index] = numBins
        tableData[2,index] = round(100*numMatches/numBins)
        print(paste('NumBins: ',numBins,'Match',numMatches/numBins))
    }
    colnames(tableData) <- 50*seq(1,10)
    rownames(tableData) <-  c("# Bins","Accuracy (%)")
    plot.new()
    grid.table(tableData)
}

if (!exists("sessionsPerDay")) {
    ## Having done, that, let's look at the station heavy hitters and
    ## determine how "saturated" they are, which is to say how much of the
    ## time they are full. We can do this easily by summing up the
    ## "duration" column.
    duration = hms(chargeData$Total.Duration)
    duration = as.numeric(duration)/(3600)
    tableData = matrix(0,nrow = length(stationCount),ncol=1,byrow=TRUE)
    stationNames = names(stationCount)
    for (index in seq(1,length(stationNames))) {
        stationName = stationNames[index]
        stationIndices = which(chargeData$Station.Name == stationName)
        totalTime = max(endTime[stationIndices]) -
            min(startTime[stationIndices])
        totalTime = as.numeric(totalTime)*24;
        totalDuration = sum(duration[stationIndices])
        print(paste(stationName, " Loading: ",100 * totalDuration/totalTime))
        tableData[index,1] = round(100*totalDuration/totalTime)
    }
    rownames(tableData) = stationNames
    colnames(tableData) = c("Loading (%)")
    plot.new()
    grid.table(tableData)
    print(" Notice that some of these are really REALLY high")

    ## Now, for fun, let's compute the average number of charging
    ## sessions per day for each station.
    stationNames = unique(chargeData$Station.Name)
    sessionsPerDay = c()
    for (stationName in stationNames) {
        stationIndices = which(chargeData$Station.Name == stationName)
        numSessions = length(stationIndices)
        totalDays = max(endTime[stationIndices]) -
            min(startTime[stationIndices])
        sessionsPerDay[stationName] = numSessions/as.numeric(totalDays);
    }
    sessionsPerDay = sessionsPerDay[order(unlist(sessionsPerDay),
                                          decreasing=TRUE)]
    plot.new()
    plot(sessionsPerDay,
         type='o',
         ylab = 'Sessions Per Day', 
         main = 'Station Usage (Sorted)')
}

if (!exists("numVertices")) {
    ## Now lets set up a matrix for the graph edges. For this one, we
    ## treat the userIds and station names as vertices: assign them
    print("Load adjacency list")
    numVertices = 0
    vertexNumber = c()
    vertexNames = c()

    for (stationName in unique(chargeData$Station.Name)) {
        numVertices = numVertices + 1
        vertexNumber[stationName] = numVertices
        vertexNames[numVertices] = stationName
    }
    numStations = numVertices
    for (userId in unique(chargeData$User.ID)) {
      numVertices = numVertices + 1
      vertexNumber[userId] = numVertices
      vertexNames[numVertices] = userId
    }
    numUsers = numVertices - numStations
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
    
    ## Now, that doesn't really do us much good: we get almost as many
    ## communities as we have nodes. My suspicion is that this is due to
    ## one of two things: 1) the graph is bi-partite and so does not
    ## really match the "spoke and hub" model of the algorithm or 2) It is
    ## way too reptitive: the same people plug into the same stations a
    ## lot of times, meaning the "volumes" fill up and then can't be
    ## split.
    
    ## To address that second one, let's "uniquefy" the matrix: only one
    ## copy of each user/station edge.
    uniqueEdgeList = unique(edgeList)

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
    
    ## So that really didn't tell us much: the nature of that community 
    ## algorithm is such that it does not really retain community structure. 
    ##
    ## Instead, let's execute the following analysis:
    ## Let's use heavy hitters to find the most popular edges!!
    numEdges = 1
    
    edgeUser = c()
    edgeIndex = c()
    edgeCount = c()
    edgeEnergy = c()
    edgeStation = c()
    
    for (index in seq(1,nrow(chargeData))) {
        edge = toString(c(chargeData$User.ID[index],chargeData$Station.Name[index]))
        if (length(edgeUser) == 0 ||
            is.na(edgeUser[edge])) {
            edgeUser[edge] = chargeData$User.ID[index]
            edgeCount[edge] = 0
            edgeIndex[edge] = numEdges
            edgeEnergy[edge] = 0
            edgeStation[edge] = chargeData$Station.Name[index]
            numEdges = numEdges + 1
        }
        edgeEnergy[edge] = edgeEnergy[edge] + chargeData$Energy..kWh.[index]
        edgeCount[edge] = edgeCount[edge] + 1
    }
}
if (TRUE || !exists('monthlyClosenessCentrality')) {
    ## first, let's make a graph from the entire edge list
    print("Let's try a plot of closness centrality for all of the ")
    print("stations as a function of month")
    chargeGraph = graph(as.vector(t(edgeList)),n=numVertices,directed=FALSE)
  
    ## Now, get the year of the first and last time
    monthlyClosenessCentrality = c()
    periodDates = c()
    for (thisYear in seq(year(startTime[1]),year(endTime[length(endTime)]))){
        for (thisMonth in seq(1,12)) {
            monthIndices = which(year(startTime) == thisYear &
                                 month(startTime) == thisMonth)
            if (length(monthIndices) > 0) {
                periodDates = c(periodDates,startTime[monthIndices[1]])
                temp = graph(as.vector(t(edgeList[monthIndices,])),
                             n=numVertices,directed=FALSE)
                closeness = estimate_closeness(temp,
                                               vids = seq(1,numStations),
                                               mode="all",
                                               normalized = TRUE,
                                               100)
                monthlyClosenessCentrality = cbind(monthlyClosenessCentrality,
                                                   closeness)
            }
        }
    }
    plot.new()
    matplot(t(as.matrix(monthlyClosenessCentrality)),t='l',
            xlab='Month Into Data',ylab='Centrality',
            main='Closeness Centrality of All Stations')
}


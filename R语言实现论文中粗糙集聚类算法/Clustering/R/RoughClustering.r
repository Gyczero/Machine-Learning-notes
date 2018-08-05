
###########################################################
# RoughKMeans_PE 
###########################################################


RoughKMeans_PE = function(dataMatrix, meansMatrix = NA, nClusters = 2, maxIterations = 50, threshold = 1.5, weightLower = 0.7) {
  
  # Setting of variables and matrices
  nObjects    = nrow(dataMatrix)
  nFeatures   = ncol(dataMatrix)
  threshold   = threshold^2   # squared: comparison with squared distances
  dataMatrix  = convertDataFrame2Matrix(dataMatrix)
  meansMatrix = convertDataFrame2Matrix(meansMatrix)
  
  previousUpperMShips = matrix(999, nrow = nObjects, ncol = nClusters)
  
  parametersCorrect = checkParameters(nObjects, nFeatures, nClusters, weightLower, threshold, maxIterations, meansMatrix)
  if (!parametersCorrect$fctStatus) {
    return(parametersCorrect)
  }
  
  meansMatrix = initializeMeansMatrix(dataMatrix, nClusters, meansMatrix)
  upperMShipMatrix = assignObj2upperApproxPE(dataMatrix, meansMatrix, threshold)
  lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
  
  # Starting the iteration
  counter = 0
  print("Iteration:", quote = FALSE)
  
  # Repeat until there is no change in the classification 
  while ( !identical(previousUpperMShips, upperMShipMatrix) && counter < maxIterations ) {
    
    meansMatrixLower = crossprod(lowerMShipMatrix, dataMatrix)      # t(lowerMShipMatrix) %*% dataMatrix
    meansMatrixUpper = crossprod(upperMShipMatrix, dataMatrix)      # t(upperMShipMatrix) %*%  dataMatrix
    
    for (i in 1:nClusters) {
      
      meansMatrixLower[i,] = meansMatrixLower[i,] / sum(lowerMShipMatrix[,i])
      meansMatrixUpper[i,] = meansMatrixUpper[i,] / sum(upperMShipMatrix[,i])
      
    }
    meansMatrix = weightLower * meansMatrixLower + (1-weightLower) * meansMatrixUpper
    
    # Saving upper approximations of previous iteration (i-1) to compare them with current upper approximations (i)
    previousUpperMShips = upperMShipMatrix
    
    upperMShipMatrix = assignObj2upperApproxPE(dataMatrix, meansMatrix, threshold)
    lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
    
    print( counter <- counter + 1 )
  }
  
  cat("\n\n")
  return ( list(fctStatus = TRUE, lowerApprox = lowerMShipMatrix, upperApprox=upperMShipMatrix, clusterMeans=meansMatrix, nIterations=counter) )
}


###########################################################
# assignObj2upperApproxPE (for Peters and PI)
###########################################################		
assignObj2upperApproxPE = function(dataMatrix, meansMatrix, threshold) {
  
  nObjects  = nrow(dataMatrix)
  nClusters = nrow(meansMatrix)
  
  # Initialization of upperMShipMatrix and distanceMatrix
  upperMShipMatrix = matrix(0,  nrow = nObjects, ncol = nClusters)
  distanceMatrix   = matrix(NA, nrow = nObjects, ncol = nClusters)
  
  # Calculation the distances between objects and cluster centers j
  for(i in 1:nObjects) {
    for (j in 1:nClusters) {
      distanceMatrix[i,j] = sum( (dataMatrix[i,] - meansMatrix[j,])^2 )
    }
  }
  
  # Assigning the closest object to each cluster
  remainingObjects = c(1:nObjects)
  tempDisMatrix    = distanceMatrix

  for(i in 1:nClusters){

    # Identifying cluster and object with minimum distance
    minPosVector = which(tempDisMatrix == min(tempDisMatrix), arr.ind = TRUE)
    closestObjects2Mean  = minPosVector[1,1]
    correspondingCluster = minPosVector[1,2]

    # Overwrite distance for the identified cluster and object with infinity
    tempDisMatrix[closestObjects2Mean, ] = Inf
    tempDisMatrix[,correspondingCluster] = Inf

    # Assigning the identified object to cluster
    upperMShipMatrix[closestObjects2Mean, correspondingCluster] = 1
    remainingObjects = remainingObjects[-which(remainingObjects == closestObjects2Mean)]
  }

  # Assigning of the remaining objects to the clusters
  for (i in remainingObjects) {
    
    # Calculate new cluster for the object
    minDistance = max(min(distanceMatrix[i,]), 1e-99)
    identifiedClusters = which((distanceMatrix[i,] / minDistance) <= threshold)
    upperMShipMatrix[i, identifiedClusters] = 1
  }
  
  return(upperMShipMatrix)
}


###########################################################
# checkParameters
###########################################################
checkParameters = function(nObjects, nFeatures, nClusters, weightLower, threshold, maxIterations, meansMatrix) {
  
  
  #validation of the number of cluster centres
    if ( !( datatypeInteger(nClusters) &&  2<=nClusters && nClusters<nObjects ) ) {
    return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <nClusters = [2, nObjects)> with <datatype(nClusters) = integer>"))
  }
  
  #validation of the weight
  if ( !( 0.0 <= weightLower && weightLower <= 1.0 ) ) {
    return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <weightLower = [0.0, 1.0]> with <datatype(weightLower) = real>"))
  }
  
  #validation of the threshold
  if (  !( threshold >= 1.0 ) ) {
    return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <threshold >= 1.0> with <datatype(threshold) = real>"))
  }
  
  #validation of the maximal number of iterations
  if ( !( datatypeInteger(maxIterations) && maxIterations>1 ) ) {
    return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <iterations >= 1>  with <datatype(iterations) = integer>"))
  }
  
  if ( is.matrix(meansMatrix) ){
	if ( !( nrow(meansMatrix)==nClusters && ncol(meansMatrix)==nFeatures ) ) 
		return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <dim(meansMatrix) = [nClusters x nFeatures]>"))

  }else if ( datatypeInteger(meansMatrix) ){
	if ( !( as.integer(meansMatrix)== 1 || as.integer(meansMatrix)==2 ) )
		return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <[meansMatrix] = 1> for random means, <2> for maximal distances between means, or a matrix with <dim(meansMatrix) = [nClusters x nFeatures] for pre-defined means (2)"))
	
  }else{
		return (list(fctStatus = FALSE, fctMessage =  "ERROR: Select <datatype(meansMatrix) = integer.or.matrix> with <meansMatrix = 1> for random means, <meansMatrix = 2> for maximal distances between means, or a matrix with <dim(meansMatrix) = [nClusters x nFeature] for pre-defined means (2)"))
  }
  
  return (list(fctStatus = TRUE, fctMessage =  NA))
}

################################################################
# normalizeMatrix
################################################################


normalizeMatrix <- function ( dataMatrix, normMethod=1, byrow = TRUE ) {
  
  dataMatrix = as.matrix(dataMatrix)
  nCol = ncol(dataMatrix)
  
  if ( byrow == FALSE ) {
    dataMatrix = t(dataMatrix)
  }
  
  if ( normMethod == 1 ) {           # Unity Interval
    for ( i in 1:nCol ) {
      dataMatrix [,i] = ( dataMatrix[,i]-min(dataMatrix[,i]) ) / ( max(dataMatrix[,i])-min(dataMatrix[,i]) ) 
    }
  } else if ( normMethod == 2 ) {    # Gauss with sample variance (divider: n-1)
    for ( i in 1:nCol ) {
      dataMatrix [,i] = ( dataMatrix[,i]-mean(dataMatrix[,i]) ) / sd(dataMatrix[,i])  
    }
  } else if ( normMethod == 3 ) {    # Gauss with population variance (divider: n)
    nRow = nrow(dataMatrix)
    for ( i in 1:nCol ) {
      dataMatrix [,i] = ( dataMatrix[,i]-mean(dataMatrix[,i]) ) / ( (nRow-1)/nRow * sd(dataMatrix[,i]) ) 
    }
  }else {                            # Return matrix unchanged
    
  }
  
  if ( byrow == FALSE ) {
    dataMatrix = t(dataMatrix)
  }
  
  return( dataMatrix )
  
}

###########################################################
# initializeMeansMatrix
###########################################################


initializeMeansMatrix = function(dataMatrix, nClusters, meansMatrix) {
  
  dataMatrix = as.matrix(dataMatrix)
 
  if(is.matrix(meansMatrix)) { # means pre-defined
    # no action required
   		
  }else if (meansMatrix == 1) {            # random means
    
    nFeatures = ncol(dataMatrix)
    meansMatrix = matrix(0, nrow=nClusters, ncol=nFeatures)
    for (i in 1:nFeatures) {
      meansMatrix[,i] = c(runif(nClusters, min(dataMatrix[,i]), max(dataMatrix[,i])))
    }
    
  }else if (meansMatrix == 2) {      # maximum distance means
    
    meansObjects      = seq(length=nClusters,from=0,by=0) # <-c(0, 0, ...)
    objectsDistMatrix = as.matrix(dist(dataMatrix))
    
    posVector = which(objectsDistMatrix == max(objectsDistMatrix), arr.ind = TRUE)
    meansObjects[1] = posVector[1,1]
    meansObjects[2] = posVector[1,2]
    
    for(i in seq(length=(nClusters-2), from=3, by=1) ) {
      meansObjects[i] = which.max( colSums(objectsDistMatrix[meansObjects, -meansObjects]) )
    }
    
    meansMatrix = dataMatrix[meansObjects,]
	
  }else {
    print("ERROR: Select <[meansMatrix] = 1> for random means), <2> for maximal distances between means, or a <matrix> for pre-defined means")
	stop("yes")
  }
  
  return( as.matrix(meansMatrix) )
  
}


###########################################################
# createLowerMShipMatrix
###########################################################

createLowerMShipMatrix = function(upperMShipMatrix) {
  
  # Initialization of lowerMShipMatrix
  lowerMShipMatrix = 0 * upperMShipMatrix
  
  lowerMShips = which( rowSums(upperMShipMatrix) == 1 )
  
  lowerMShipMatrix[lowerMShips,] = upperMShipMatrix[lowerMShips,]
  
  return(lowerMShipMatrix)
}

###########################################################
# convertDataFrame2Matrix
###########################################################	 

convertDataFrame2Matrix = function(dataMatrix) {
	
  if( is.data.frame(dataMatrix) ) {
	dataMatrix = as.matrix(dataMatrix)
  }
  
  return(dataMatrix)
}

###########################################################
# plotRoughKMeans
###########################################################


plotRoughKMeans = function(dataMatrix, upperMShipMatrix, meansMatrix, plotDimensions = c(1:2), colouredPlot=TRUE ) {
  
  # plotRoughKMeans(D2Clusters, cl$upperApprox, cl$clusterMeans, plotDimensions = c(1:2) ) 
  
  graphics.off()
  
  if( !is.logical(colouredPlot) ) {
    return("No plot selected")
  }
  
  nObjects  = nrow(dataMatrix)
  nFeatures = ncol(dataMatrix)
  nClusters = meansMatrix
  
  allObjects = c(1:nObjects)
  
  if( length(plotDimensions) != 2 || min(plotDimensions) < 1 || max(plotDimensions) > nFeatures ) {
    return("ERROR: Set < plotDimensions) != 2 || min(plotDimensions) < 1 || max(plotDimensions) > nFeatures >")
  }
 
  # Set colours and symbols for objects
  objectSymbols  = c( 0, 1, 5, 2, 6, 3, 4 , 7, 8, 9, 11, 12)
  meansSymbols   = c(22,21,23,24,25)
  boundarySymbol = 8
  boundaryColor  = 1 
  
  if(colouredPlot){
    clusterColors  = c(2:20)
  }else{
    clusterColors  = c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  }

  
  dataMatrix  = dataMatrix [, plotDimensions]
  #meansMatrix = meansMatrix[, plotDimensions]
  
  lowerMShips  = which( rowSums(upperMShipMatrix) == 1 )
  
  plot(dataMatrix, col = "white")
  
  # Plot members of lower approximations
  for(i in lowerMShips) {
    clusterNumber = which( upperMShipMatrix[i,] == 1 )
    points( dataMatrix[i,1],dataMatrix[i,2], col=clusterColors[clusterNumber], pch=objectSymbols[clusterNumber] )
  }
  
  # Plot members of boundary
  for(i in allObjects[-lowerMShips] ) {
    points( dataMatrix[i,1],dataMatrix[i,2], col=boundaryColor, pch=boundarySymbol )
  }
  
  # Plot means
  #for(i in 1:nClusters ) {
  #  points( meansMatrix[i,1],meansMatrix[i,2], col="black", bg=clusterColors[i], pch=meansSymbols[i], cex=1.5 )
  # }
  
  return("Plot created")
  
}

###########################################################
# datatypeInteger
###########################################################


datatypeInteger <- function(x)
{
	return ( as.integer(x)==x )

}



KLDTRSRoughKMeans_PE = function(dataMatrix, meansMatrix = NA, p = 2.0, m = 2.0, nClusters = 2, maxIterations = 50, threshold = 1.1, weightLower = 0.96) {
  
  # Setting of variables and matrices
  nObjects    = nrow(dataMatrix)
  nFeatures   = ncol(dataMatrix)
  threshold   = threshold^2   # squared: comparison with squared distances
  dataMatrix  = convertDataFrame2Matrix(dataMatrix)
  meansMatrix = convertDataFrame2Matrix(meansMatrix)
  
  previousUpperMShips = matrix(999, nrow = nObjects, ncol = nClusters)
  
  parametersCorrect = checkParameters(nObjects, nFeatures, nClusters, weightLower, threshold, maxIterations, meansMatrix)
  if (!parametersCorrect$fctStatus) {
    return(parametersCorrect)
  }
  
  meansMatrix = initializeMeansMatrix(dataMatrix, nClusters, meansMatrix)
  upperMShipMatrix = assignObj2upperApproxPEKLDTRS(dataMatrix, meansMatrix, p, m, threshold)
  lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
  
  # Starting the iteration
  counter = 0
  print("Iteration:", quote = FALSE)
  
  # Repeat until there is no change in the classification 
  while ( !identical(previousUpperMShips, upperMShipMatrix) && counter < maxIterations ) {
    
    meansMatrixLower = crossprod(lowerMShipMatrix, dataMatrix)      # t(lowerMShipMatrix) %*% dataMatrix
    meansMatrixUpper = crossprod(upperMShipMatrix, dataMatrix)      # t(upperMShipMatrix) %*%  dataMatrix
    
    for (i in 1:nClusters) {
      
      meansMatrixLower[i,] = meansMatrixLower[i,] / sum(lowerMShipMatrix[,i])
      meansMatrixUpper[i,] = meansMatrixUpper[i,] / sum(upperMShipMatrix[,i])
      
    }
    meansMatrixLower[is.nan(meansMatrixLower)] = 0
    meansMatrixUpper[is.nan(meansMatrixUpper)] = 0
    
    meansMatrix = weightLower * meansMatrixLower + (1-weightLower) * meansMatrixUpper
    
    # Saving upper approximations of previous iteration (i-1) to compare them with current upper approximations (i)
    previousUpperMShips = upperMShipMatrix
    
    upperMShipMatrix = assignObj2upperApproxPEKLDTRS(dataMatrix, meansMatrix, p, m, threshold)
    lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
    
    print( counter <- counter + 1 )
  }
  
  cat("\n\n")
  return ( list(fctStatus = TRUE, lowerApprox = lowerMShipMatrix, upperApprox=upperMShipMatrix, clusterMeans=meansMatrix, nIterations=counter) )
}


assignObj2upperApproxPEKLDTRS = function(dataMatrix, meansMatrix, p, m, threshold) {
        nObjects  = nrow(dataMatrix)
        nClusters = nrow(meansMatrix)
        
        # Initialization of upperMShipMatrix and distanceMatrix
        upperMShipMatrix = matrix(0,  nrow = nObjects, ncol = nClusters)
        distanceMatrix   = matrix(NA, nrow = nObjects, ncol = nClusters)
        conditionMatrix   = matrix(0, nrow = nObjects, ncol = nClusters)
        neighborMatrix = matrix(0, nrow = nObjects, ncol = nObjects)
        similarMatrix = matrix(1, nrow = nObjects, ncol = nClusters)
        riskMatrix = matrix(NA, nrow = nObjects, ncol = nClusters)
        objectsDistMatrix = matrix(NA, nrow = nObjects, ncol = nObjects)
        
        # Calculation the distances between objects and cluster centers j
        for(i in 1:nObjects) {
                for (j in 1:nClusters) {
                        distanceMatrix[i,j] = LaplacesDemon::KLD(dataMatrix[i,], meansMatrix[j,])$mean.sum.KLD ^ 2 
                }
        }
        
        if (0)
        {
                conditionMatrix = as.matrix(dataMatrix)
        } else {
                # Calculation conditionMatrix
                for (i in 1:nObjects)
                {
                        objectBelongCategory = which(distanceMatrix[i,] == 0)
                        conditionMatrix[i,] <- 1 /
                                (((distanceMatrix[i,]) ^ (1 / (m - 1))) *
                                         sum((1 / (distanceMatrix[i,])) ^ (1 /(m - 1))))
                        conditionMatrix[i, objectBelongCategory] = 1
                }
                
        }
        
        # Calculation the neighborMatrix
        neighborcost = apply(distanceMatrix ^ (1/2), 1, min) / p
        for(i in 1:nObjects) {
                for (j in 1:nObjects) {
                        objectsDistMatrix[i,j] = LaplacesDemon::KLD(dataMatrix[i,], dataMatrix[j,])$mean.sum.KLD

                }
        }
        
        for(i in 1:nObjects)
        {
                identifiedObjects = which(objectsDistMatrix[i,] <= neighborcost[i] )
                neighborMatrix[i, identifiedObjects] = 1
                neighborMatrix[i, i] = 0
                
        }
        
        # Calculation Similar Cluster Matrix:Tx
        for(i in 1:nObjects)
        {
                identifiedClusters = which(conditionMatrix[i,] > 1 /nClusters )
                similarMatrix[i, identifiedClusters] = 0
        }
        
        

        # Create a diag Matrix for lambda Ci
        M = matrix(1, nrow = nClusters, ncol = nClusters)
        M[row(M) == col(M)] = 0
        # sigma is the denominator
        sigma = sum(objectsDistMatrix ^ 2) / (nObjects ^ 2)
        # Calculation Risk associated with Actions
        for(i in 1:nObjects)
        {
                clusterActionMatrix = matrix(NA, nrow = nClusters, ncol = nClusters)
                neighborsDistance = exp(-(objectsDistMatrix[which(neighborMatrix[i,] == 1),i]) ^ 2 / sigma)
                neighborsDistance = matrix(rep(neighborsDistance, nClusters), nrow = nClusters, byrow = T)
                objectActionMatrix = similarMatrix[which(neighborMatrix[i,] == 1), ]
                clusterActionMatrix = neighborsDistance %*% objectActionMatrix
                clusterActionMatrix = clusterActionMatrix + M
                riskMatrix[i, ] = conditionMatrix[i, ] %*% clusterActionMatrix
        }
        
        
        # Assigning of the objects to the clusters
        for (i in 1:nObjects) {
                
                minDistance = max(min(riskMatrix[i,]), 1e-99)
                identifiedClusters = which((riskMatrix[i,] / minDistance) <= threshold)
                upperMShipMatrix[i, identifiedClusters] = 1
        }
        
        return(upperMShipMatrix)
}




#######################################################################################

DTRSRoughKMeans_PE = function(dataMatrix, meansMatrix = NA, p = 2.0, m = 2.0, nClusters = 2, maxIterations = 50, threshold = 1.1, weightLower = 0.96) {
        
        # Setting of variables and matrices
        nObjects    = nrow(dataMatrix)
        nFeatures   = ncol(dataMatrix)
        threshold   = threshold^2   # squared: comparison with squared distances
        dataMatrix  = convertDataFrame2Matrix(dataMatrix)
        meansMatrix = convertDataFrame2Matrix(meansMatrix)
        
        previousUpperMShips = matrix(999, nrow = nObjects, ncol = nClusters)
        
        parametersCorrect = checkParameters(nObjects, nFeatures, nClusters, weightLower, threshold, maxIterations, meansMatrix)
        if (!parametersCorrect$fctStatus) {
                return(parametersCorrect)
        }
        
        meansMatrix = initializeMeansMatrix(dataMatrix, nClusters, meansMatrix)
        upperMShipMatrix = assignObj2upperApproxPEDTRS(dataMatrix, meansMatrix, p, m, threshold)
        lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
        
        # Starting the iteration
        counter = 0
        print("Iteration:", quote = FALSE)
        
        # Repeat until there is no change in the classification 
        while ( !identical(previousUpperMShips, upperMShipMatrix) && counter < maxIterations ) {
                
                meansMatrixLower = crossprod(lowerMShipMatrix, dataMatrix)      # t(lowerMShipMatrix) %*% dataMatrix
                meansMatrixUpper = crossprod(upperMShipMatrix, dataMatrix)      # t(upperMShipMatrix) %*%  dataMatrix
                
                for (i in 1:nClusters) {
                        
                        meansMatrixLower[i,] = meansMatrixLower[i,] / sum(lowerMShipMatrix[,i])
                        meansMatrixUpper[i,] = meansMatrixUpper[i,] / sum(upperMShipMatrix[,i])
                        
                }
                meansMatrixLower[is.nan(meansMatrixLower)] = 0
                meansMatrixUpper[is.nan(meansMatrixUpper)] = 0
                
                meansMatrix = weightLower * meansMatrixLower + (1-weightLower) * meansMatrixUpper
                
                # Saving upper approximations of previous iteration (i-1) to compare them with current upper approximations (i)
                previousUpperMShips = upperMShipMatrix
                
                upperMShipMatrix = assignObj2upperApproxPEDTRS(dataMatrix, meansMatrix, p, m, threshold)
                lowerMShipMatrix = createLowerMShipMatrix(upperMShipMatrix)
                
                print( counter <- counter + 1 )
        }
        
        cat("\n\n")
        return ( list(fctStatus = TRUE, lowerApprox = lowerMShipMatrix, upperApprox=upperMShipMatrix, clusterMeans=meansMatrix, nIterations=counter) )
}


assignObj2upperApproxPEDTRS = function(dataMatrix, meansMatrix, p, m, threshold) {
        nObjects  = nrow(dataMatrix)
        nClusters = nrow(meansMatrix)
        
        # Initialization of upperMShipMatrix and distanceMatrix
        upperMShipMatrix = matrix(0,  nrow = nObjects, ncol = nClusters)
        distanceMatrix   = matrix(NA, nrow = nObjects, ncol = nClusters)
        conditionMatrix   = matrix(0, nrow = nObjects, ncol = nClusters)
        neighborMatrix = matrix(0, nrow = nObjects, ncol = nObjects)
        similarMatrix = matrix(1, nrow = nObjects, ncol = nClusters)
        riskMatrix = matrix(NA, nrow = nObjects, ncol = nClusters)
        objectsDistMatrix = matrix(NA, nrow = nObjects, ncol = nObjects)
        
        # Calculation the distances between objects and cluster centers j
        for(i in 1:nObjects) {
                for (j in 1:nClusters) {
                        #distanceMatrix[i,j] = LaplacesDemon::KLD(dataMatrix[i,], meansMatrix[j,])$mean.sum.KLD ^ 2 
                        distanceMatrix[i,j] = sum( (dataMatrix[i,] - meansMatrix[j,])^2 )
                }
        }
        
        if (0)
        {
                conditionMatrix = as.matrix(dataMatrix)
        } else {
                # Calculation conditionMatrix
                for (i in 1:nObjects)
                {
                        objectBelongCategory = which(distanceMatrix[i,] == 0)
                        conditionMatrix[i,] <- 1 /
                                (((distanceMatrix[i,]) ^ (1 / (m - 1))) *
                                         sum((1 / (distanceMatrix[i,])) ^ (1 /(m - 1))))
                        conditionMatrix[i, objectBelongCategory] = 1
                }
                
        }
        
        # Calculation the neighborMatrix
        neighborcost = apply(distanceMatrix ^ (1/2), 1, min) / p
        objectsDistMatrix = as.matrix(dist(dataMatrix))
        
        for(i in 1:nObjects)
        {
                identifiedObjects = which(objectsDistMatrix[i,] <= neighborcost[i] )
                neighborMatrix[i, identifiedObjects] = 1
                neighborMatrix[i, i] = 0
                
        }
        
        # Calculation Similar Cluster Matrix:Tx
        for(i in 1:nObjects)
        {
                identifiedClusters = which(conditionMatrix[i,] > 1 /nClusters )
                similarMatrix[i, identifiedClusters] = 0
        }
        
        
        
        # Create a diag Matrix for lambda Ci
        M = matrix(1, nrow = nClusters, ncol = nClusters)
        M[row(M) == col(M)] = 0
        # sigma is the denominator
        sigma = sum(objectsDistMatrix ^ 2) / (nObjects ^ 2)
        # Calculation Risk associated with Actions
        for(i in 1:nObjects)
        {
                clusterActionMatrix = matrix(NA, nrow = nClusters, ncol = nClusters)
                neighborsDistance = exp(-(objectsDistMatrix[which(neighborMatrix[i,] == 1),i]) ^ 2 / sigma)
                neighborsDistance = matrix(rep(neighborsDistance, nClusters), nrow = nClusters, byrow = T)
                objectActionMatrix = similarMatrix[which(neighborMatrix[i,] == 1), ]
                clusterActionMatrix = neighborsDistance %*% objectActionMatrix
                clusterActionMatrix = clusterActionMatrix + M
                riskMatrix[i, ] = conditionMatrix[i, ] %*% clusterActionMatrix
        }
        
        
        # Assigning of the objects to the clusters
        for (i in 1:nObjects) {
                
                minDistance = max(min(riskMatrix[i,]), 1e-99)
                identifiedClusters = which((riskMatrix[i,] / minDistance) <= threshold)
                upperMShipMatrix[i, identifiedClusters] = 1
        }
        
        return(upperMShipMatrix)
}


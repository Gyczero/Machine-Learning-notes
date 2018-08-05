data <- read.table('winred-probability')
wineData <- read.csv('winequality-red.csv', sep = ';', skip = 1)

wineDataIndex <- wineData[, c(2,3)]
i <<- -49

getData <- function(data)
{
        i <<- i + 50
        j <- i + 49
        return (data[i:j, ])
}

seqTime = 10

############################### slice ##############################################
sliceData <- getData(data)
result <- KLDTRSRoughKMeans_PE(sliceData, 2, nClusters = 11)
print(result$upperApprox)
Sys.sleep(seqTime)

############################### slice ##############################################
sliceData <- getData(data)
result <- KLDTRSRoughKMeans_PE(sliceData, 2, nClusters = 11)
print(result$upperApprox)
Sys.sleep(seqTime)

############################### slice ##############################################
sliceData <- getData(data)
result <- KLDTRSRoughKMeans_PE(sliceData, 2, nClusters = 11)
print(result$upperApprox)
Sys.sleep(seqTime)

############################### slice ##############################################
sliceData <- getData(data)
result <- KLDTRSRoughKMeans_PE(sliceData, 2, nClusters = 11)
print(result$upperApprox)






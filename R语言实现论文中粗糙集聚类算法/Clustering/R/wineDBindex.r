data <- read.table('Wine_probability')
realData <- read.csv('Wine')



# realData <- realData[, -5]
# resultKLDTRCM <- DTRSRoughKMeans_PE(data, 2, nClusters = 3)
# nObjects <- nrow(realData)
# resultCategory <- vector()
# for(i in 1:nObjects)
# {
#         resultCategory[i] <- which(resultKLDTRCM$upperApprox[i, ] == 1)
# }
# realData <- as.matrix(realData)

realClusters <- realData[, 5]
clusterData <- realData[, -5]
DBV <- vector()
DunnV <- vector()

# for(j in 1:100)
# {
resultKLDTRCM <- KLDTRSRoughKMeans_PE(data,meansMatrix = 1, nClusters = 3)    # ????????????
nObjects <- nrow(data)
resultCategoryupper <- vector()
resultCategoryLower <- vector()
index <- vector()
for(i in 1:nObjects)
{
        if(sum(resultKLDTRCM$lowerApprox[i,] == 1))
        {
                resultCategoryupper[i] <- which(resultKLDTRCM$upperApprox[i, ] == 1)
        } else {
                resultCategoryupper[i] <- 0
        }
}
resultCategoryupper = as.integer(resultCategoryupper)


for(i in 1:nObjects)
{
        if(sum(resultKLDTRCM$lowerApprox[i,]) == 1)
        {
                resultCategoryLower[i] <- which(resultKLDTRCM$lowerApprox[i, ] == 1)
        }
        
}
# get DB index 
clusterData <- as.matrix(clusterData)
data <- as.matrix(data)
print(clusterCrit::intCriteria(data, part = resultCategoryupper, 'davies_bouldin')$davies_bouldin)
print(clusterCrit::intCriteria(data, part = resultCategoryupper, 'dunn')$dunn)
#print("Iteration ------------------", quote = FALSE)
# print(j)
#DBV[j] <- clusterCrit::intCriteria(data, part = resultCategoryupper, 'davies_bouldin')$davies_bouldin
#DunnV[j] <- clusterCrit::intCriteria(data, part = resultCategoryupper, 'dunn')$dunn

# }


# get ACC --------------????????????????????????????????????resultCategoryLower??????50????????????50?????????50???????????????????????????????????????????????????????????????????????????
realClustersInteger <- vector()
for(i in 1:nObjects)
{
        if(i <= 50)
                realClustersInteger[i] <- 1
        else if(i > 50 && i <= 100)
                realClustersInteger[i] <- 3
        else
                realClustersInteger[i] <- 2
}
realClustersInteger <- as.integer(realClustersInteger)
N <- 0
right <- 0
for(i in 1:nObjects)
{
        if(!is.na(resultCategoryLower[i]))   
        {
                N <- N + 1
                if(resultCategoryLower[i] == realClustersInteger[i])
                        right <- right + 1
        }
}
right / N



# for(i in 1:nObjects)
# {
#         if(re)
# }


# categories <- realData[, c(5)]
# num <- 3 #?????????
# nObjects <- nrow(data)
# resultCategory <- vector()
# 
# resultKLDTRCM <- KLDTRSRoughKMeans_PE(data, 2 , nClusters = num)
# for(i in 1:nObjects)
# {
#         resultCategory[i] <- which(resultKLDTRCM$upperApprox[i, ] == 1)
# }
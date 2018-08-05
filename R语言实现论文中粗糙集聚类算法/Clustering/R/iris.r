data <- read.table('iris_probability')
realData <- read.csv('iris.csv')
realData <- realData[ ,c(1,2)]
i <- 3         # ??????
num <- 3 #?????????

#############                           RCM????????????               ##########################

resultRCM <- RoughKMeans_PE(realData, 1, nClusters = num)
plotRoughKMeans(realData, resultRCM$upperApprox, num)
Sys.sleep(i)

#############                           DTRCM????????????              ##########################

resultDTRCM <- DTRSRoughKMeans_PE(realData, 1,  nClusters = num)
plotRoughKMeans(realData, resultDTRCM$upperApprox, num)
Sys.sleep(i)

#############                  RCM????????????        ##########################

resultKLDTRCM <- RoughKMeans_PE(data, 1,  nClusters = num)
plotRoughKMeans(realData, resultKLDTRCM$upperApprox, num)
Sys.sleep(i)

#############                  DTRCM????????????        ##########################

resultKLDTRCM <- DTRSRoughKMeans_PE(data, 1,  nClusters = num)
plotRoughKMeans(realData, resultKLDTRCM$upperApprox, num)
Sys.sleep(i)

#############                 DTRCM KL????????????????????????   ##########################

resultKLDTRCM <- KLDTRSRoughKMeans_PE(data, 1 , nClusters = num)
plotRoughKMeans(realData, resultKLDTRCM$upperApprox, num)

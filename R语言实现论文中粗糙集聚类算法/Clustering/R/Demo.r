data <- read.table('Probability.txt')
realData <- read.table('DemoDataC2D2a.txt', skip = 1)
i <- 3

#############                           RCM                 ##########################

resultRCM <- RoughKMeans_PE(realData, 2)
plotRoughKMeans(realData, resultRCM$upperApprox, 2)
Sys.sleep(i)

#############                           DTRCM               ##########################

resultDTRCM <- DTRSRoughKMeans_PE(realData, 2)
plotRoughKMeans(realData, resultDTRCM$upperApprox, 2)
Sys.sleep(i)

#############                  RCM  for probability         ##########################

resultKLDTRCM <- RoughKMeans_PE(data, 2)
plotRoughKMeans(realData, resultKLDTRCM$upperApprox, 2)


#############                 DTRCM  with KL divergence     ##########################

resultKLDTRCM <- KLDTRSRoughKMeans_PE(data, 2)
plotRoughKMeans(realData, resultKLDTRCM$upperApprox, 2)

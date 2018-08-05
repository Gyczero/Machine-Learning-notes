corr <- function(directory, threshold = 0) {
        setwd("/Users/admin/Desktop/R Learning/specdata")
        v <- vector(mode = "numeric", 0)
        for (i in 1:332) {
                number <- formatC(i, width = 3, flag = "0") #format character
                link <- paste(number, ".csv", sep = "")
                content <- read.csv(link)
                bad1 <- is.na(content["sulfate"])
                bad2 <- is.na(content["nitrate"])
                if (sum(!bad1) > threshold && sum(!bad2) > threshold) {
                        calc <- cor(content["sulfate"], content["nitrate"], use = "na.or.complete")
                        v <- c(v, calc)
                }
        }
        v
}
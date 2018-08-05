pollutantmean <- function(directory, pollutant, id = 1:332) {
        setwd(paste(getwd(), "/", directory, sep = ""))
        sum1 <- 0
        rows <- 0
        for (i in id) {
                number <- formatC(i, width = 3, flag = "0") #format character
                link <- paste(number, ".csv", sep = "")
                content <- read.csv(link)
                m <- is.na(content[pollutant])
                n <- sum(!m)
                if (n > 0) {
                rows <- rows + n
                content[m, pollutant] <- 0
                sum1 <- sum1 + sum(content[pollutant])
                }
        }
        sum1 / rows
}
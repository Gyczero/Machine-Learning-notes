complete <- function(directory, id = 1:332) {
        setwd(paste(getwd(), "/", directory, sep = ""))
        result <- data.frame(id = id, nobs = 0)
        cout <- 1
        for (i in id) {
                number <- formatC(i, width = 3, flag = "0") #format character
                link <- paste(number, ".csv", sep = "")
                content <- read.csv(link)
                good <- complete.cases(content)
                sum <- sum(good)
                result[cout, 1] = i
                result[cout, 2] = sum
                cout <- cout + 1
        }
        result
}

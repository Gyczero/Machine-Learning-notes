best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv")
        
        goodstate <- as.vector(data["State"] == state)
        badout <- (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") 
        if (sum(goodstate) == 0) {
                stop("invalid state")
        } else if (badout) {
                stop("invalid outcome")
        } else {
                data <- data[goodstate, ]
                outname <- capwords(outcome)
                outname <- chartr(" ", ".", outname)
                outname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outname, sep = "")
                data1 <- data[outname]
                con <- data1[names(data1)[1]] == "Not Available"
                con <- as.vector(con)
                data1[con, 1] <- 0
                con <- !is.na(data1)
                lowest <- data1[con, 1]
                lowest <- as.vector(lowest)
                lowest <- as.numeric(lowest)
                lowest <- min(lowest)
                hoscon <- data1[names(data1)[1]] == lowest
                con2 <- (hoscon & !is.na(hoscon))
                hos <- data[con2, ]
                hosname <- as.vector(hos[ , "Hospital.Name"])
                sort(hosname)
        }
        hosname[1]
}
# how collapse works?
capwords <- function(s) {
        cap <- function(s) 
                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
        sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

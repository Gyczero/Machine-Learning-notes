rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv")
        
        goodstate <- as.vector(data["State"] == state)
        badout <- (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") 
        
        if(sum(goodstate) == 0) {
                stop("invalid state")
        } else if(badout) {
                stop("invalid outcome")
        } else {
                data <- data[goodstate, ]
                outname <- capwords(outcome)
                outname <- chartr(" ", ".", outname)
                outname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outname, sep = "")
                index <- order(as.numeric(as.vector(data[ ,outname])), data["Hospital.Name"])
                bad <- data[ , outname] == "Not Available"
                availnum <- nrow(data) - sum(bad)
                if(num == "best") {
                        m <- 1
                } else if(num == "worst") {
                        m <- availnum
                } else {
                        m <- num
                }  
        }
        as.vector(data[index[m], "Hospital.Name"])
}
capwords <- function(words) {
        cap <- function(s) 
                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " " )
        sapply(strsplit(words, split = " "), cap)
}


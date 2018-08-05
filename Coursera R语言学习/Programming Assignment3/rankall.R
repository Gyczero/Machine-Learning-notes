rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv")
        
        badout <- (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") 
        if(badout) {
                stop("invalid outcome")
        } else {
                outname <- capwords(outcome)
                outname <- chartr(" ", ".", outname)
                outname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outname, sep = "")
                
                listbystate <- split(data, data$State)
                statenames <- names(listbystate)
                hospitalnames <- vector(length = length(statenames))
                
                hospitalnames <- sapply(listbystate, rankstate, outname, num)
                }
        data.frame(row.names = statenames, hospital = hospitalnames, state = statenames)
}

capwords <- function(words) {
        cap <- function(s)
                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
        sapply(strsplit(words, " "), cap)
}

rankstate <- function(data, name, num1 = "best") {
        index <- order(as.numeric(as.vector(data[ ,name])), data["Hospital.Name"])
        bad <- data[ , name] == "Not Available"
        availnum <- nrow(data) - sum(bad)
        
        if(num1 == "best") {
                m <- 1
        } else if(num1 == "worst") {
                m <- availnum
        } else {
                m <- num1
        }  
        as.vector(data[index[m], "Hospital.Name"])
}
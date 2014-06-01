best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "", "NA", "NAN"), stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        sub = subset(data, data$State == state)
        if (nrow(sub) == 0) {
                stop("invalid state")
        }
        
        ## function to find the best hospital
        compare <- function(sub, x) {
                ## keep Hospital.Name and the corresponding outcome only.
                sub1 <- data.frame(1:nrow(sub))
                sub1$V1 <- sub$Hospital.Name
                sub1$V2 <- sub[, x]
                sub1 <- na.omit(sub1)
                min = min(sub1$V2)
                sub2 <- subset(sub1, sub1$V2 == min) 
                sort(sub2$V1,decreasing=FALSE)[1]
        }
        
        ## keep only the columns of "Hospital.Name", "Hospital.30.Day.
        ## Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.
        ## Death..Mortality..Rates.from.Heart.Failure", or " Hospital.
        ## 30.Day.Death..Mortality..Rates.from.Pneumonia"
        ## Check if the outcome is one of "heart attack", "heart failure", or "pneumoni"        
        if (outcome == "heart attack") {
                ## set the value to the column of "heart attack"
                compare(sub, 11)
        } else if (outcome == "heart failure") {
                compare(sub, 17)
        } else if (outcome == "pneumonia") {
                compare(sub, 23)
        } else {
                stop("invalid outcome")
        }
}




best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "", "NA", "NAN"), stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        sub = subset(data, data$State == state)
        if (nrow(sub) == 0) {
                stop("invalid state")
        }
        
        ## function to find the best hospital based on the given column number.
        compare <- function(sub, x) {      
                ## creating an empty data frame with the same rows as sub
                sub1 <- data.frame(1:nrow(sub))  
                
                ## copy Hospital.Name and the given column to sub1
                sub1$V1 <- sub$Hospital.Name
                sub1$V2 <- sub[, x]
                
                ## remove nas from sub1
                sub1 <- na.omit(sub1)
                min = min(sub1$V2)
                
                ## get the subset with corresponding minimum death rate.
                sub2 <- subset(sub1, sub1$V2 == min) 
                
                ## sort in alphabetical order(increasing) and return the first.
                sort(sub2$V1,decreasing=FALSE)[1]
        }
        
        ## Check if the outcome is one of "heart attack", "heart failure", or "pneumoni"   
        ## and choose the corresponding column number if outcome is right, otherwise
        ## stop the program.
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




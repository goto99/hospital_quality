rankhospital <- function(state, outcome, rank = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "", "NA", "NAN"), stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        sub = subset(data, data$State == state)
        if (nrow(sub) == 0) {
                stop("invalid state")
        }
        
        ## function to find the hospital of given rank based on the given column number.
        compare <- function(sub, x, rank) {      
                ## creating an empty data frame with the same rows as sub
                sub1 <- data.frame(1:nrow(sub))  
                
                ## copy Hospital.Name and the given column to sub1
                sub1$V1 <- sub$Hospital.Name
                sub1$V2 <- sub[, x]
                
                ## remove nas from sub1
                sub1 <- na.omit(sub1)
                
                ## if rank is the default value
                if (rank == "best") {
                        ## sort in increasing order and return the given rank hospital name.
                        min = min(sub1$V2)
                        
                        ## get the subset with corresponding minimum death rate.
                        sub2 <- subset(sub1, sub1$V2 == min) 
                        
                        ## sort in alphabetical order(increasing) and return the first.
                        sort(sub2$V1,decreasing=FALSE)[1]
                } else if (rank == "worst") {
                        max = max(sub1$V2)
                        
                        ## get the subset with corresponding maximum death rate.
                        sub2 <- subset(sub1, sub1$V2 == max) 
                        
                        ## sort in alphabetical order(increasing) and return the first.
                        sort(sub2$V1,decreasing=FALSE)[1]
                } else {
                        ## sort sub1 by V2 first, then by V1, then reorder it and place
                        ## the reordered data frame in sub2.
                        ## reoder code found on Stackoverflow
                        sub2 <- sub1[order(sub1$V2, sub1$V1), ]
                        sub2$V1[rank]    
                }       
        }
        
        ## Check if the outcome is one of "heart attack", "heart failure", or "pneumoni"   
        ## and choose the corresponding column number if outcome is right, otherwise
        ## stop the program.
        if (outcome == "heart attack") {
                ## set the value to the column of "heart attack"
                compare(sub, 11, rank)
        } else if (outcome == "heart failure") {
                compare(sub, 17, rank)
        } else if (outcome == "pneumonia") {
                compare(sub, 23, rank)
        } else {
                stop("invalid outcome")
        }
}



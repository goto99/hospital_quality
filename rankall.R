rankall <- function(outcome, rank = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "", "NA", "NAN"), stringsAsFactors = FALSE)
        subs <- data.frame(1: nrow(data))
        subs$State <- data$State
        subs$Hospital <- data$Hospital.Name
        subs$HA <- data[, 11]
        subs$HF <- data[, 17]
        subs$pneumonia <- data[, 23]
        subs <- subs[, 2:6]
        subs$split <- as.factor(subs$State)
        groups <- split(subs, subs$split, drop = FALSE)
        

        
        ## function to find the hospital of given rank based on the given column number.
        compare <- function(sub, col, rank) {      
                ## creating an empty data frame with the same rows as sub
                sub1 <- data.frame(1: nrow(sub))                  
                ## copy Hospital.Name and the given column to sub1
                sub1$Hospital <- sub$Hospital
                sub1$V2 <- sub[, col]
                sub1$State <- sub$State
                sub1 <- sub1[, 2:4]
                ## remove nas from sub1
                temp <- na.omit(sub1)
                if (nrow(temp == 0)) {
                        sub1[1, ]
                }

                ## sort sub1 by V2 first, then by V1, then reorder it and place
                ## the reordered data frame in sub2.
                ## reoder code found on Stackoverflow
                sub2 <- temp[order(temp$V2, temp$Hospital), ]
     
                ## if rank is the default value
                if (rank == "best") {
                        ## print(sub2[1, ])    
                        sub2[1, ]     
                } else if (rank == "worst") {
                        ## print(sub2[nrow(sub2), ])
                        sub2[nrow(sub2), ]
                } else if (rank > nrow(sub2)) {
                        res <- sub1[1, ]
                        res$Hospital <- NA
                        ## print(res)
                        res
                } else {
                        ## print(sub2[rank, ])  
                        sub2[rank, ]    
                }       
        }
        
        multicompare <- function(groups, col, rank) {
                ## create an empty data frame to store the found items
                ## change the data type from factor to character after creating data frame.
                temp <- data.frame(hospital = "", state = "")
                temp$hospital <- as.character(temp$hospital)
                temp$state <- as.character(temp$state)
                
                for (i in 1: length(groups)) {
                        record <- compare(groups[[i]], col, rank)
                        ## add observation to the end of data frame "temp".
                        temp[i, ] <- c(record$Hospital[1], record$State[1])
                }
                temp
        }
        
        ## Check if the outcome is one of "heart attack", "heart failure", or "pneumoni"   
        ## and choose the corresponding column number if outcome is right, otherwise
        ## stop the program.
        if (outcome == "heart attack") {
                ## set the value to the column of "heart attack"
                multicompare(groups, 3, rank)
        } else if (outcome == "heart failure") {
                multicompare(groups, 4, rank)
        } else if (outcome == "pneumonia") {
                multicompare(groups, 5, rank)
        } else {
                stop("invalid outcome")
        }
}


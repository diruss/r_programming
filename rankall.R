rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # read outcome file.
    outcomes <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
    
    # init output
    output <- list()   
    
    # check the validity of outcom argu
    if(outcome == 'heart attack') {
        i <- 11
    }
    else if(outcome == 'heart failure') {
        i <- 17
    }
    else if(outcome == 'pneumonia') {
        i <- 23
    }
    else {
        stop('invalid outcome')
    }
    
    # iterate over states
    for(state in sort(unique(outcomes$State))) {
        
        # set state perimeter
        stateOutcomes <- outcomes[outcomes$State == state, ]
        
        # by specifying colClasses = "character" 
        # we need to coerce the column to be numeric.
        # you may get a warning about NAs being introduced; that is okay.
        stateOutcomes[, i] <- as.numeric(stateOutcomes[, i])
        stateOutcomes <- stateOutcomes[complete.cases(stateOutcomes), ]
        
        # check num and set perimeter
        if(num == "best") {
            my_rank = 1
        }
        else if(num == "worst") {
            my_rank = nrow(stateOutcomes)
            # if(state == 'WI') {
            #   print(num)
            #   print('WI num')
            # }
        }
        else if(is.numeric(num)) {
            # print(num)
            if(num < 1 || num > nrow(stateOutcomes)) {
                output <- rbind(output, list(NA, state))
                print(state)
                next
            }
            else my_rank <- num
            # print(num)
        }
        else {
            stop('invalid num')
        }
        
        # order
        stateOutcomes <- stateOutcomes[
            order(stateOutcomes[,i], stateOutcomes$Hospital.Name), ]        
        orderedNames <- stateOutcomes[my_rank, ]$Hospital.Name
        
        # save result
        output <- rbind(output, list(orderedNames[1], state))
    }
    
    output <- as.data.frame(output)
    colnames(output) <- c('hospital', 'state')
    
    output
}
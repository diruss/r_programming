best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    # read outcome file.
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check the validity of state argument.
    if(!state %in% unique(outcomes$State)){
        stop("invalid state")        
    }    
    else{
        # set perimeter of state argument
        stateOutcomes <- outcomes[outcomes$State == state, ]
        
        # check the validity of outcom argument.
        if (outcome == 'heart attack'){
            colpos <- 11   
        }
        else if (outcome == 'heart failure'){
            colpos <- 17            
        }
        else if (outcome == 'pneumonia'){           
            colpos <- 23
        }
        else {
            stop("invalid outcome")            
        }
        
        
        # by specifying colClasses = "character" 
        # we need to coerce the column to be numeric.
        # you may get a warning about NAs being introduced; that is okay.
        stateOutcomes[, colpos] <- as.numeric(stateOutcomes[, colpos])
        stateOutcomes <- stateOutcomes[complete.cases(stateOutcomes), ]

        # order according to best outcome
        orderedout <- stateOutcomes[
            (stateOutcomes[, colpos] == min(stateOutcomes[, colpos])), ]$Hospital.Name
        # handle ties
        return(sort(orderedout)[1])
    }
}
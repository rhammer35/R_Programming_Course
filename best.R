best <- function(state, outcome) {
    ## initialize character vector of possible outcomes for check later
    poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    source_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(as.character(state) != source_data[, 7]) {
        stop("invalid state")
    }
    
    else if (as.character(outcome) != poss_outcomes) {
        stop("invalid outcome")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}


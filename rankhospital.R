rankhospital <- function(state, outcome, num = "best") {
    
    ## initialize character vector of possible outcomes for check later
    poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    source_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    source_data[, 11] <- as.numeric(source_data[, 11])
    source_data[, 17] <- as.numeric(source_data[, 17])
    source_data[, 23] <- as.numeric(source_data[, 23])
    
    ## Check that state and outcome are valid
    state <- as.character(state)
    outcome <- as.character(outcome)
    poss_states <- unique(source_data[, 7])
    valid_state <- poss_states[poss_states == state]
    valid_outcome <- poss_outcomes[poss_outcomes == outcome]
    
    if(length(valid_state) == 0) {
        stop("invalid state")
    }
    
    else if(length(valid_outcome) == 0) {
        stop("invalid outcome")
    }

    ## Create data frame containing only data for queried state
    state_data <- as.data.frame(source_data[source_data$State == state, ])
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}

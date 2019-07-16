best <- function(state, outcome) {
    ## initialize character vector of possible outcomes for check later
    poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    source_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
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
    
    ## Create variable to use to return specified outcome
    if(outcome == poss_outcomes[1]) {
        outcome_num <- 11
    }
    
    else if(outcome == poss_outcomes[2]) {
        outcome_num <- 17
    }
    
    else {
        outcome_num <- 23
    }
    
    ## Calculate minimum value of outcome in state
    state_data[, outcome_num] <- as.numeric(state_data[, outcome_num])
    min_value <- min(state_data[, outcome_num], na.rm = TRUE)
    
    ## Create vector with any hospital(s) matching calculated minimum
    min_hosps <- na.omit(state_data[state_data[, outcome_num] == min_value, ])
    hosp_name <- min_hosps[, 2]
    
    ## Sort Minimum Hospitals vector alphabetically in event of tie
    hosp_name <- sort(hosp_name)

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    print(hosp_name[1])
    
}


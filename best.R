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
    
    ## Create data frame to use to return data for specified outcome
    col_numbers <- c(11, 17, 23)
    outcome_data <- as.data.frame(cbind(poss_outcomes, col_numbers))
    outcome_data[, 1] <- as.character(outcome_data[, 1])
    outcome_data[, 2] <- as.numeric(outcome_data[, 2])
    
    ## Create variable to use to return specified outcome
    if(outcome == outcome_data[1, 1]) {
        outcome_num <- outcome_data[1, 2]
    }
    
    else if(outcome == outcome_data[2, 1]) {
        outcome_num <- outcome_data[2, 2]
    }
    
    else {
        outcome_num <- outcome_data[3, 2]
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


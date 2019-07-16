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
    
    ## Sort data first by desired outcome then by hospital name to break ties
    ranked_state_data <- state_data[order(state_data[, outcome_num], state_data[, 2], decreasing = FALSE, na.last = NA)]
    
    ## Determine highest possible ranking in new sorted data
    num_rows <- nrow(ranked_state_data)

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if(num == "best") {
        ranked_hosp <- ranked_state_data[1, 2]
    }
    
    else if(num == "worst") {
        ranked_hosp <- ranked_state_data[num_rows, 2]
    }

    else if(as.numeric(num) > num_rows) {
        ranked_hosp <- NA
    }
    
    else {
        num <- as.numeric(num)
        ranked_hosp <- ranked_state_data[num, 2]
    }
    
    print(ranked_hosp)
}

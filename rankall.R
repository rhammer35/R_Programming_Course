rankall <- function(outcome, num = "best") {
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
    
    ## Reorder data frame to rank by state, then outcome, then name
    ranked_data <- source_data[order(source_data[, 7], source_data[, outcome_num], source_data[, 2], decreasing = FALSE, na.last = NA), ]
    
    ## Remove unnecessary data columns and rename remaining columns
    trimmed_data <- cbind(ranked_data[, 2], ranked_data[, 7], ranked_data[, outcome_num])
    colnames(trimmed_data) <- c("Hospital", "State", outcome)
    
    ## Split ranked data by state and count cases for each state
    split_data <- split(ranked_data, ranked_data$State)
    
    ## Use in lapply to make vectors to turn into final data frame
    if(num == "best") {
        ranked_Hosp <- lapply(split_data, function(state_frame) {return(state_frame[1, 1])})    
        ranked_State <- lapply(split_data, function(state_frame) {return(state_frame[1, 2])})
    }
        
    else if(num == "worst") {
        ranked_Hosp <- lapply(split_data, function(state_frame) {return(state_frame[length(state_frame), 1])})    
        ranked_State <- lapply(split_data, function(state_frame) {return(state_frame[length(state_frame), 2])})
    }
    
    else {
        num <- as.numeric(num)
        ranked_Hosp <- lapply(split_data, function(state_frame) {return(state_frame[num, 1])})    
        ranked_State <- lapply(split_data, function(state_frame) {return(state_frame[num, 2])})
    }
    
    ## Combine the two vectors into one data frame and name the frame columns
    ranked_frame <- data.frame(cbind(ranked_Hosp, ranked_State))
    colnames(ranked_frame) <- c("Hospital Name", "State")
    ranked_frame
}

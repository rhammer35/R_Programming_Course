complete <- function(directory, id = 1:332) {
    j <- length(id)
    i <- id[1]
    k <- 1
    id1 <- dir(directory)
    nobs <- c()
    
    while(k <= j) {
        name1 <- paste(directory, "/", id1[i], sep = "")
        current_file <- read.csv(name1)
        count1 <- sum(as.numeric(complete.cases(current_file)))
        nobs <- c(nobs, count1)
        k <- k + 1
        i <- id[k]
    }
    
    my_result <- data.frame(id, nobs)
    print(my_result)
}
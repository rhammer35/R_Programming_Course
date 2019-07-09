complete <- function(directory, id = 1:332) {
    j <- length(id)
    i <- id[1]
    k <- 1
    id1 <- dir(directory)
    
    while(i < j) {
        i <- i + 1
        name2 <- paste(directory, "/", id1[i], sep = "")
        table <- rbind(table, as.matrix(read.csv(name2)))
    }
    my_mean <- mean(as.numeric(table[,pollutant]),na.rm = TRUE)
    my_mean
}
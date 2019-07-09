pollutantmean <- function(directory, pollutant, id = 1:332) {
    j <- length(id)
    i <- id[1]
    k <- i + j - 1
    id1 <- dir(directory)
    name1 <- paste(directory, "/", id1[i], sep = "")
    table <- as.matrix(read.csv(name1))
    while(i < k) {
        i <- i + 1
        name2 <- paste(directory, "/", id1[i], sep = "")
        table <- rbind(table, as.matrix(read.csv(name2)))
    }
    my_mean <- mean(as.numeric(table[,pollutant]),na.rm = TRUE)
    my_mean
}
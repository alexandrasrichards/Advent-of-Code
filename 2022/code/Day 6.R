## Day 6
library(here)

input <-readChar(here("data","data 6-1.txt"), nchar = 10000)
listed <- c()
marker <- c()
marker_points <- c()
## Part 1
for(i in 1:nchar(input)){
    new_char <- substr(input,i,i)
    listed <- c(listed,substr(input,i,i))
    marker[i%%4 + 1] <- new_char
    if(length(unique(na.omit(marker)))== 4) {
        marker_points <- c(marker_points,i)
    }
}

## Part 2

listed <- c()
marker <- c()
marker_points <- c()
for(i in 1:nchar(input)){
    new_char <- substr(input,i,i)
    listed <- c(listed,substr(input,i,i))
    marker[i%%14 + 1] <- new_char
    if(length(unique(na.omit(marker)))== 14) {
        marker_points <- c(marker_points,i)
    }
}

## Day 8
library(here)

input <- read.delim(here("data","data 8.csv"), sep = "\n", header = FALSE, numerals = "no.loss")

grid <- as.data.frame(matrix(data = NA, nrow = nrow(input), ncol = nchar(input[1,])))

for(i in 1:nchar(input[1,])) {
    for(j in 1:nrow(input)) {
        grid[j,i] = substr(input[j,],i,i)
    }
}

visible <- as.data.frame(matrix(data = NA, nrow = nrow(input), ncol = nchar(input[1,])))

#top to bottom
for(i in 1:nrow(visible)){
    tree_max <- 0
    for(j in 1:ncol(visible)){
        if(grid[j,i] > tree_max){
            tree_max = grid[j,i]
            visible[j,i] = "x"
        } else {
            if(is.na(visible[j,i]) || visible[j,i] != "x"){
                visible[j,i] = "_"
            }
        }
    }
}

#bottom to top
for(i in 1:nrow(visible)){
    tree_max <- 0
    for(j in ncol(visible):1){
        if(grid[j,i] > tree_max){
            tree_max = grid[j,i]
            visible[j,i] = "x"
        } else {
            if(is.na(visible[j,i]) || visible[j,i] != "x"){
                visible[j,i] = "_"
            }
        }
    }
}

#left to right
for(i in 1:ncol(visible)){
    tree_max <- 0
    for(j in 1:nrow(visible)){
        if(grid[i,j] > tree_max){
            tree_max = grid[i,j]
            visible[i,j] = "x"
        } else {
            if(is.na(visible[i,j]) || visible[i,j] != "x"){
                visible[i,j] = "_"
            }
        }
    }
}

#left to right
for(i in 1:ncol(visible)){
    tree_max <- 0
    for(j in nrow(visible):1){
        if(grid[i,j] > tree_max){
            tree_max = grid[i,j]
            visible[i,j] = "x"
        } else {
            if(is.na(visible[i,j]) || visible[i,j] != "x"){
                visible[i,j] = "_"
            }
        }
    }
}

visible[1,] = "x"
visible[,1] = "x"
visible[ncol(visible),] = "x"
visible[,ncol(visible)] = "x"

length(which(visible == "x"))


## Part 2
score <- as.data.frame(matrix(data = NA, nrow = nrow(input), ncol = nchar(input[1,])))

for(i in 1:nrow(grid)) {
    for(j in 1:ncol(grid)){
        score_left = 0
        score_right = 0
        score_up = 0
        score_down = 0
        
        up_down <- which(grid[,j] >= grid[i,j])
        up_down <- c(1, up_down, nrow(grid))
        up_down <- up_down[c(which(up_down == i)-1, which(up_down == i), which(up_down == i) + 1)]
        score_up <- i - up_down[1]
        score_down <- up_down[3] - i
        
        left_right <- which(grid[i,] >= grid[i,j])
        left_right <- c(1, left_right, ncol(grid))
        left_right <- left_right[c(which(left_right == j)-1, which(left_right == j), which(left_right == j) + 1)]
        score_left <- j - left_right[1]
        score_right <- left_right[3] - j
        
        score[i,j] <- score_down * score_up * score_left * score_right
    }
}
max(score)

## Day 14
library(here)

input <- read.csv(here("data","data 14.csv"), header = FALSE, sep = "\n")

cave <- as.data.frame(matrix(nrow = 500, ncol = 1000, data = "."))

## set up cave
for(i in seq(1,nrow(input),1)){
    rocks = unlist(strsplit(input[i,1], split = " -> "))
    for(j in seq(2, length(rocks),1)){
        x_start = as.numeric(unlist(strsplit(rocks[j-1],","))[1])
        x_end = as.numeric(unlist(strsplit(rocks[j],","))[1])
        y_start = as.numeric(unlist(strsplit(rocks[j-1],","))[2])
        y_end = as.numeric(unlist(strsplit(rocks[j],","))[2])
        
        
        for(x in seq(x_start, x_end,sign(x_end - x_start))){
            for(y in seq(y_start, y_end, sign(y_end - y_start))){
                cave[y,x] = "#"
            }
        }
    }
}


## Part 1
count_snow = 0
no_space = FALSE
lowest_shelf = max(which(cave == "#", arr.ind = TRUE)[,"row"])

while(no_space == FALSE){
    ##
    print(count_snow)
    snow = data.frame(row = 0, col = 500)
    stopped = FALSE
    while(stopped == FALSE){
        #print(snow)
        if(cave[snow$row+1, snow$col] == "."){
            snow$row = snow$row + 1
        } else {
            if (cave[snow$row+1, snow$col-1] == "."){
                snow$row = snow$row + 1
                snow$col = snow$col-1
            } else if (cave[snow$row+1, snow$col+1] == "."){
                snow$row = snow$row + 1
                snow$col = snow$col+1
            } else {
                cave[snow$row,snow$col] = "o"
                count_snow = count_snow + 1
                stopped = TRUE
            }
        } 
        if(snow$row>lowest_shelf){
            stopped = TRUE
            no_space = TRUE
        }    
    }
}

## Part 2
count_snow = 0
no_space = FALSE

cave <- as.data.frame(matrix(nrow = lowest_shelf + 2, ncol = 1000, data = "."))

## set up cave
for(i in seq(1,nrow(input),1)){
    rocks = unlist(strsplit(input[i,1], split = " -> "))
    for(j in seq(2, length(rocks),1)){
        x_start = as.numeric(unlist(strsplit(rocks[j-1],","))[1])
        x_end = as.numeric(unlist(strsplit(rocks[j],","))[1])
        y_start = as.numeric(unlist(strsplit(rocks[j-1],","))[2])
        y_end = as.numeric(unlist(strsplit(rocks[j],","))[2])
        
        
        for(x in seq(x_start, x_end,sign(x_end - x_start))){
            for(y in seq(y_start, y_end, sign(y_end - y_start))){
                cave[y,x] = "#"
            }
        }
    }
}

cave[lowest_shelf + 2,]= "#"



while(no_space == FALSE){
    ##
    print(count_snow)
    snow = data.frame(row = 0, col = 500)
    stopped = FALSE
    while(stopped == FALSE){
        if(all(cave[1, 499:501] == "o")){
            print(snow)
            count_snow = count_snow + 1
            stopped = TRUE
            no_space = TRUE
        } else if (snow$row + 1 > nrow(cave)){
            cave[snow$row,snow$col] = "o"
            count_snow = count_snow + 1
            print(snow)
            stopped = TRUE
        } else if(cave[snow$row+1, snow$col] == "."){
            snow$row = snow$row + 1
        } else {
             if (cave[snow$row+1, snow$col-1] == "."){
                snow$row = snow$row + 1
                snow$col = snow$col-1
            } else if (cave[snow$row+1, snow$col+1] == "."){
                snow$row = snow$row + 1
                snow$col = snow$col+1
            } else {
                cave[snow$row,snow$col] = "o"
                count_snow = count_snow + 1
                print(snow)
                stopped = TRUE
            }
        } 
    }
    
    
    
}


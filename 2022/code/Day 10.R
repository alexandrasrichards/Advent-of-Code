## Day 10
library(here)

input <- read.csv(here("data","data 10.csv"), header = FALSE)

cycles <- data.frame("cycle" = c(1), "value" = c(1))

count = 1

for(i in 1:nrow(input)){
    if(strsplit(input[i,], split = " ")[[1]][1] == "noop"){
        count = count + 1
        cycles[count,1] = count
        cycles[count,2] = cycles[count-1,2]
    } else {
        count = count + 2
        cycles[count-1,1] = count-1
        cycles[count-1,2] = cycles[count-2,2]
        
        cycles[count,1] = count
        cycles[count,2] = cycles[count-1,2] + as.numeric(strsplit(input[i,], split = " ")[[1]][2])   
    }
}

value = cycles[20,2] * 20 +
    cycles[60,2] * 60 +
    cycles[100,2] * 100 +
    cycles[140,2] * 140 +
    cycles[180,2] * 180 +
    cycles[220,2] * 220


## Part 2
crt <- as.data.frame(matrix(nrow = 6, ncol = 40))
cycles$sprite_low = cycles$value-1
cycles$sprite_high = cycles$value+1
cycles$tick = cycles$cycle %% 40
cycles$tick[which(cycles$tick == 0)] = 40
cycles$tick = cycles$tick - 1
for(i in 1:nrow(cycles)){
    cycles$crt[i] = ifelse(cycles$tick[i] %in% c(cycles$sprite_low[i], cycles$value[i], cycles$sprite_high[i]), "#",".")
}



crt <- (matrix(nrow = 6, ncol = 40, data = cycles$crt[1:240], byrow = TRUE))


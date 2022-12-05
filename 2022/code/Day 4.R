## Day 4

library(here)

example <- read.csv(here("data","example 4-1.csv"), header = FALSE)

part1 <- function(data){

    elf1 <- strsplit(data[,1],"[-]")
    elf2 <- strsplit(data[,2],"[-]")
    
    for(i in 1:nrow(data)){
        #browser()
        data$elf1[i] <- list(seq(as.numeric(elf1[[i]][1]),as.numeric(elf1[[i]][2])))
        data$elf2[i] <- list(seq(as.numeric(elf2[[i]][1]),as.numeric(elf2[[i]][2])))
        
        data$crossover[i] <- list(intersect(unlist(data$elf1[i]), unlist(data$elf2[i])))
        data$crossover[i] <- ifelse(identical(data$crossover[i][[1]], integer(0)),0,data$crossover[i])
        data$subset[i] <- ifelse(setequal(data$crossover[i][[1]],data$elf1[i][[1]]) || setequal(data$crossover[i][[1]], data$elf2[i][[1]]),1,0)
    }

    print(sum(data$subset))
    return(data)
}

part1(example)

data <- read.csv(here("data","data 4-1.csv"), header = FALSE)
data_new <- part1(data)

nrow(data_new) #1000
length(which(unlist(data_new$crossover) == 0)) #105

nrow(data_new) - length(which(unlist(data_new$crossover) == 0))

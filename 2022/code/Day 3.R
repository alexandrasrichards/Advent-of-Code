## Day 3
library(here)
example <- read.csv(here("data","example 3-1.csv"), header = FALSE)

letter_value <- data.frame("letter" = c(letters, LETTERS),
                           "value" = seq(1:52))

part1 <- function(data) {
    names(data) <- "total"
    data$first <- strsplit(substring(data$total, 1, nchar(data$total)/2),"")
    data$second <- strsplit(substring(data$total, nchar(data$total)/2 + 1, nchar(data$total)),"")
    

    
    
    for(i in 1:nrow(data)) {
        first <- unique(unlist(data$first[i]))
        second <- unique(unlist(data$second[i]))
        data$common[i] <- intersect(first, second)
        data$value[i] <- letter_value$value[which(letter_value$letter == data$common[i])]
        
    }
    
    return(sum(data$value))
    
}

part1(example)

data <- read.csv(here("data","data 3-1.csv"), header = FALSE)
part1(data)


part2 <- function(data){
    
    elf1 <- as.data.frame(data[seq(1,nrow(data),3),])
    elf2 <- as.data.frame(data[seq(2,nrow(data),3),])
    elf3 <- as.data.frame(data[seq(3,nrow(data),3),])
    
    group <- data.frame("elf1" = elf1[,1], 
                        "elf2" = elf2[,1], 
                        "elf3" = elf3[,1])

    for(i in 1:(nrow(group))) {
        first <- unique(unlist(strsplit(as.character(group$elf1[i]),"")))
        second <- unique(unlist(strsplit(as.character(group$elf2[i]),"")))
        third <- unique(unlist(strsplit(as.character(group$elf3[i]),"")))
        group$common[i] <- intersect(intersect(first, second),third)
        group$value[i] <- letter_value$value[which(letter_value$letter == group$common[i])]
        
    }
    
    return(sum(group$value))
    
}

part2(example)
part2(data)

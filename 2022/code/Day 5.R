## Day 5

library(here)

input <- read.csv(here("data","data 5-1.csv"), blank.lines.skip = FALSE, header = FALSE)

blank <- which(input[,1] == "")

crates <- input[1:(blank[1]-1),]
instructions <- input[(blank[1]+1:blank[2]-1),]

len <- nchar(crates[1])
piles <- length(seq(2,len,4))
crate_data <- data.frame("piles"=seq(1:piles))

for(i in 1:piles){
    pile <- c()
    let <-seq(2,len,4) 
    for(j in (blank[1]-2):1){
        box <- substr(crates[j],let[i],let[i])
        if(box!= " "){
            pile = c(pile, box)
        }
    }
    crate_data$order[i] <- list(pile)
}

### sort crates
instructions <- instructions[!is.na(instructions)]
instructions <- instructions[instructions!= ""]
for(i in 1:length(instructions)){
    ins <- strsplit(instructions[i]," ")
    move <- as.numeric(ins[[1]][2])
    from <- as.numeric(ins[[1]][4])
    to <- as.numeric(ins[[1]][6])
    for(j in 1:move){
        crate_move <- tail(crate_data$order[from][[1]],1)
        crate_data$order[from][[1]] <- head(crate_data$order[from][[1]],-1)
        crate_data$order[to][[1]] <- c(crate_data$order[to][[1]],crate_move)
    }
    
}

## TBVFVDZPN

## reset crates

crates <- input[1:(blank[1]-1),]
instructions <- input[(blank[1]+1:blank[2]-1),]

len <- nchar(crates[1])
piles <- length(seq(2,len,4))
crate_data <- data.frame("piles"=seq(1:piles))

for(i in 1:piles){
    pile <- c()
    let <-seq(2,len,4) 
    for(j in (blank[1]-2):1){
        box <- substr(crates[j],let[i],let[i])
        if(box!= " "){
            pile = c(pile, box)
        }
    }
    crate_data$order[i] <- list(pile)
}

### sort crates
instructions <- instructions[!is.na(instructions)]
instructions <- instructions[instructions!= ""]
for(i in 1:length(instructions)){
    ins <- strsplit(instructions[i]," ")
    move <- as.numeric(ins[[1]][2])
    from <- as.numeric(ins[[1]][4])
    to <- as.numeric(ins[[1]][6])
        crate_move <- tail(crate_data$order[from][[1]],move)
        crate_data$order[from][[1]] <- head(crate_data$order[from][[1]],-move)
        crate_data$order[to][[1]] <- c(crate_data$order[to][[1]],crate_move)

    
}


##VLCWHTDSZ
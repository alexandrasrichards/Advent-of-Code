## Day 11

library(here)

input <- read.delim(here("data","data 11.txt"),header = FALSE, sep = "\n")
monkeys <- length(which(substr(input[,1],1,6) == "Monkey"))
monkeys_pos <- which(substr(input[,1],1,6) == "Monkey")

game <- data.frame(monkey = seq(0,monkeys - 1,1), holding = 0, operation = 0, test = 0, iftrue = 0, iffalse = 0, inspections = 0)

for(i in 1:monkeys){
    game$holding[i] = list(as.numeric(unlist(strsplit(strsplit(input[monkeys_pos[i] + 1,1],"  Starting items: ")[[1]][2],", "))))
    game$operation[i] = unlist(strsplit(input[monkeys_pos[i] + 2,1], "  Operation: new = old ")[[1]][2])
    game$test[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 3,1], "  Test: divisible by ")[[1]][2]))
    game$iftrue[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 4,1], "  If true: throw to monkey ")[[1]][2]))
    game$iffalse[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 5,1], "  If false: throw to monkey ")[[1]][2]))
  
}


monkey_business <- function(game){
    for(i in seq(1, monkeys,1)){
        if(!is.null(unlist(game$holding[[i]]))){
            items <- unlist(game$holding[[i]])
            game$holding[[i]] = list(c())
            for(j in seq(1, length(items))){
                game$inspections[i] = game$inspections[i] + 1
                old = items[j]
                
                do = substr(game$operation[i],1,1)
                by = strsplit(game$operation[i], " ")[[1]][2]
                
                if(by == "old"){
                    by = old
                }
                if(do == "+"){
                    #print(by)
                    #print(old)
                    new_val = old + as.numeric(by)
                } else if (do == "*") {
                    new_val = old * as.numeric(by)
                }
                
                new_val = floor(new_val/3)
                if ((new_val %% as.numeric(game$test[i]))==0){
                    new_monkey = as.numeric(game$iftrue[i])
                    game$holding[new_monkey + 1][[1]] = list(c(unlist(game$holding[new_monkey + 1][[1]]), new_val))
                } else {
                    new_monkey = as.numeric(game$iffalse[i])
                    game$holding[new_monkey + 1][[1]] = list(c(unlist(game$holding[new_monkey + 1][[1]]), new_val))
                } 
            }
            
            
            
        }
        
        
    }
    return(game)
}





for (k in 1:20){
    print(k)
    game = monkey_business(game)
}
 

prod(game$inspections[tail(order(game$inspections),2)])       ##55944

## Part 2

monkey_business2 <- function(game){
    test_total <- prod(game$test)
    for(i in seq(1, monkeys,1)){
        if(!is.null(unlist(game$holding[[i]]))){
            items = unlist(game$holding[[i]])
            game$holding[[i]] = list(c())
            
            do = substr(game$operation[i],1,1)
            by = strsplit(game$operation[i], " ")[[1]][2]

            new_monkey_false = as.numeric(game$iffalse[i]) + 1
            new_monkey_true = as.numeric(game$iftrue[i]) + 1
            tester = as.numeric(game$test[i])
            for(j in seq(1, length(items))){
                game$inspections[i] = game$inspections[i] + 1
                old = items[j]
                
                if(by == "old"){
                    by = old
                }

                if(do == "+"){
                    new_val = old + as.numeric(by)
                } else if (do == "*") {
                    new_val = old * as.numeric(by)
                }
                
                
                #new_val = new_val - (test_total * (floor(new_val/test_total) - 1))
                while(new_val > test_total){
                    new_val = new_val - test_total
                }
                
                if ((new_val %% tester)==0) {
                    game$holding[new_monkey_true][[1]] = list(c(unlist(game$holding[new_monkey_true][[1]]), new_val))
                } else {
                    game$holding[new_monkey_false][[1]] = list(c(unlist(game$holding[new_monkey_false][[1]]), new_val))
                } 
            }
            
            
            
        }
        
        
    }
    return(game)
}

input <- read.delim(here("data","data 11.txt"),header = FALSE, sep = "\n")
monkeys <- length(which(substr(input[,1],1,6) == "Monkey"))
monkeys_pos <- which(substr(input[,1],1,6) == "Monkey")

game <- data.frame(monkey = seq(0,monkeys - 1,1), holding = 0, operation = 0, test = 0, iftrue = 0, iffalse = 0, inspections = 0)

for(i in 1:monkeys){
    game$holding[i] = list(as.numeric(unlist(strsplit(strsplit(input[monkeys_pos[i] + 1,1],"  Starting items: ")[[1]][2],", "))))
    game$operation[i] = unlist(strsplit(input[monkeys_pos[i] + 2,1], "  Operation: new = old ")[[1]][2])
    game$test[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 3,1], "  Test: divisible by ")[[1]][2]))
    game$iftrue[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 4,1], "  If true: throw to monkey ")[[1]][2]))
    game$iffalse[i] = as.numeric(unlist(strsplit(input[monkeys_pos[i] + 5,1], "  If false: throw to monkey ")[[1]][2]))
    
}  

for (l in 1:10000){
    print(l)
    game = monkey_business3(game)
}                      
                         
  


monkey_business3 <- function(game){
    for(i in seq(1, monkeys,1)){
        if(!is.null(unlist(game$holding[[i]]))){
            items <- unlist(game$holding[[i]])
            game$holding[[i]] = list(c())
            
            

            for(j in seq(1, length(items))){
                game$inspections[i] = game$inspections[i] + 1
                old = items[j]
                
                do = substr(game$operation[i],1,1)
                by = strsplit(game$operation[i], " ")[[1]][2]
                
                if(by == "old"){
                    by = old
                }
                if(do == "+"){
                    #print(by)
                    #print(old)
                    new_val = old + as.numeric(by)
                } else if (do == "*") {
                    new_val = old * as.numeric(by)
                }
                
                n = floor(new_val/prod(game$test))
                
                new_val = new_val - n * prod(game$test)
                
                #new_val = floor(new_val/3)
                if ((new_val %% as.numeric(game$test[i]))==0){
                    new_monkey = as.numeric(game$iftrue[i])
                    game$holding[new_monkey + 1][[1]] = list(c(unlist(game$holding[new_monkey + 1][[1]]), new_val))
                } else {
                    new_monkey = as.numeric(game$iffalse[i])
                    game$holding[new_monkey + 1][[1]] = list(c(unlist(game$holding[new_monkey + 1][[1]]), new_val))
                } 
            }
            
            
            
        }
        
        
    }
    return(game)
}

prod(game$inspections[tail(order(game$inspections),2)])    ## 15117269860
                
library(here)

data = read.delim(here("data","data 21.txt"), header = FALSE, sep = "\n")

data$name = lapply(data[,1], function(x) unlist(strsplit(x, split = ":"))[1])
data$maths = lapply(data[,1], function(x) unlist(strsplit(x, split = ":"))[2])

maths = data$maths
names(maths) = data$name
letters_list = c()

for(i in 1:length(maths)){
    if(!is.na(as.numeric(maths[[i]]))){
        maths[i] = as.numeric(maths[i])
    } else {
        maths[[i]] = list(unlist(strsplit(maths[[i]], split = " "))[2:4])
        letters_list = c(letters_list, i)
    }
        
}

count = 1
while(length(letters_list) > 0){
    print(count)
    allowed = 0
    monkey = unlist(maths[letters_list[count]][[1]])
    if(!is.na(as.numeric(maths[monkey[1]]))){
        monkey[1] = as.character(maths[monkey[1]])
        allowed = allowed + 1
    } 
    if(!is.na(as.numeric(maths[monkey[3]]))){
        monkey[3] = as.character(maths[monkey[3]])
        allowed = allowed + 1
    } 
    
    
    if(allowed ==2){
        maths[letters_list[count]] = do_maths(monkey)
        letters_list = letters_list[-count]
    } else {
        count = count + 1
    }
    if(count > length(letters_list)){
        count = 1
    }
}


do_maths <- function(monkey){
    if(monkey[2] == '*'){
        monkey = as.numeric(monkey[1]) * as.numeric(monkey[3])
    } else if(monkey[2] == '+'){
        monkey = as.numeric(monkey[1]) + as.numeric(monkey[3])
    } else if(monkey[2] == '-'){
        monkey = as.numeric(monkey[1]) - as.numeric(monkey[3])
    } else if(monkey[2] == '/'){
        monkey = as.numeric(monkey[1]) / as.numeric(monkey[3])
    }
    return(monkey)
}

answer = maths$root


data_2 = read.delim(here("data","data 21.txt"), header = FALSE, sep = "\n")

data_2$name = lapply(data_2[,1], function(x) unlist(strsplit(x, split = ":"))[1])
data_2$maths = lapply(data_2[,1], function(x) unlist(strsplit(x, split = ":"))[2])

maths_2 = data_2$maths
names(maths_2) = data_2$name
letters_list = c()

maths_2$root
maths_2$humn
maths_2$root = sub("+", "-", maths_2$root, fixed = TRUE)

for(i in 1:length(maths_2)){
    if(!is.na(as.numeric(maths_2[[i]]))){
        maths_2[i] = as.numeric(maths_2[i])
    } else {
        maths_2[[i]] = list(unlist(strsplit(maths_2[[i]], split = " "))[2:4])
        letters_list = c(letters_list, i)
    }
    
}

humn_start = 3059361894000
letters_copy = letters_list
while(maths_copy$humn !=0){
    print(humn_start)
    maths_copy = maths_2
    maths_copy$humn = humn_start
    count = 1
    letters_list = letters_copy
    while(length(letters_list) > 0){
        #print(count)
        #print(letters_list)
        allowed = 0
        monkey = unlist(maths_copy[letters_list[count]][[1]])
        if(!is.na(as.numeric(maths_copy[monkey[1]]))){
            monkey[1] = as.character(maths_copy[monkey[1]])
            allowed = allowed + 1
        } 
        if(!is.na(as.numeric(maths_copy[monkey[3]]))){
            monkey[3] = as.character(maths_copy[monkey[3]])
            allowed = allowed + 1
        } 
        
        
        if(allowed ==2){
            maths_copy[letters_list[count]] = do_maths(monkey)
            letters_list = letters_list[-count]
        } else {
            count = count + 1
        }
        if(count > length(letters_list)){
            count = 1
        }
    }
    
    print(maths_copy$root)
    if(maths_copy$root == 0){
        print(maths_copy$humn)
        print(humn_start)
        break
    }
    humn_start = humn_start - 1
}



do_maths <- function(monkey){
    if(monkey[2] == '*'){
        monkey = as.numeric(monkey[1]) * as.numeric(monkey[3])
    } else if(monkey[2] == '+'){
        monkey = as.numeric(monkey[1]) + as.numeric(monkey[3])
    } else if(monkey[2] == '-'){
        monkey = as.numeric(monkey[1]) - as.numeric(monkey[3])
    } else if(monkey[2] == '/'){
        monkey = as.numeric(monkey[1]) / as.numeric(monkey[3])
    }
    return(monkey)
}

answer = maths$root
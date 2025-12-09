day = 3


input = as.data.frame(read.csv(paste("../input/","input",day,".txt",sep=""),header=FALSE,fill=TRUE))

input = t(as.data.frame(apply(input,1,function(x) as.data.frame(unlist(str_split(x,""))))))

part_numbers <- list()
a = 0
for(i in 1:nrow(input)) {
    j = 1
    while(j < ncol(input)) {
        if(input[i,j] %in% seq(0,9)){
            k = 0
            while(input[i,j+k+1] %in% seq(0,9)){
                k = k + 1
                if(j+k+1>ncol(input)){break}
            }
            
            top = ifelse(i > 1, list(input[i-1,j:(j+k)]),list())
            bottom = ifelse(i < nrow(input),list(input[i+1,j:(j+k)]),list())
            left = ifelse(j > 1, list(input[i,j-1]), list())
            right = ifelse((j+k) < nrow(input), list(input[i,j+k+1]),list())
            topleft = ifelse(i > 1 & j > 1, list(input[i-1,j-1]), list())
            topright = ifelse(i > 1 & (j+k) < nrow(input), list(input[i-1,j+k+1]), list())
            bottomleft = ifelse(i < nrow(input) & j > 1, list(input[i+1,j-1]), list())
            bottomright = ifelse(i < nrow(input) & (j+k) < nrow(input), list(input[i+1,j+k+1]), list())

            neighbours = unlist(c(top, bottom, left, right, topleft, topright, bottomleft, bottomright))
            
     
            if (length(which(neighbours == ".")) != length(neighbours)){
                a = a + 1
                part_numbers[a] = as.numeric(paste(input[i,j:(j+k)],collapse = ""))

            }
            
           j = j + k + 2                      
            
        } else {
            j = j + 1
        }
    }
}


sum(unlist(part_numbers))


## replaced * with c

gear_ratios = data.frame(i = c(), j = c(),sym = c(), gear = c())

for(i in 1:nrow(input)) {
    j = 1
    while(j < ncol(input)) {
        if(input[i,j] %in% seq(0,9)){
            k = 0
            while(input[i,j+k+1] %in% seq(0,9)){
                k = k + 1
                if(j+k+1>ncol(input)){break}
            }
            neighbours = data.frame("i" = c(),"j" = c(),"sym" = c())
            
            if(i > 1){
                neighbours = rbind(neighbours,data.frame("i" = i-1,
                                 "j" = j:(j+k),
                                 "sym"  = input[i-1,j:(j+k)]))
            }
            if(i < nrow(input)){
               neighbours = rbind(neighbours,data.frame("i" = i+1,
                                                        "j" = j:(j+k),
                                                        "sym" = input[i+1,j:(j+k)])) 
            }
            if(j > 1){
                neighbours = rbind(neighbours,data.frame("i" = i,
                                                         "j" = j-1,
                                                         "sym" = input[i,j-1]))
            }
            if((j+k) < nrow(input)){
                neighbours = rbind(neighbours,data.frame("i" = i,
                                                         "j" = j+k+1,
                                                         "sym" = input[i,j+k+1]))
            }
            if(i > 1 & j > 1){
                neighbours = rbind(neighbours,data.frame("i" = i-1,
                                                         "j" = j-1,
                                                         "sym" = input[i-1,j-1]))
            }
            if(i > 1 & (j+k) < nrow(input)){
                neighbours = rbind(neighbours,data.frame("i" = i-1,
                                                         "j" = j+k+1,
                                                         "sym" = input[i-1,j+k+1]))
            }
            if(i < nrow(input) & j > 1){
                neighbours = rbind(neighbours,data.frame("i" = i+1,
                                                         "j" = j-1,
                                                         "sym" = input[i+1,j-1]))
            }
            if(i < nrow(input) & (j+k) < nrow(input)){
                neighbours = rbind(neighbours,data.frame("i" = i+1,
                                                         "j" = j+k+1,
                                                         "sym" = input[i+1,j+k+1]))
            }
                               

            if ("c" %in% neighbours$sym){
                gears = neighbours[which(neighbours$sym == "c"),]
                gears$gear = as.numeric(paste(input[i,j:(j+k)],collapse = ""))

                gear_ratios = rbind(gear_ratios,gears)
                
            }
            
            j = j + k + 2                      
            
        } else {
            j = j + 1
        }
    }
}

gear_ratios$pos = paste(gear_ratios$i, ",", gear_ratios$j, sep = "")

for(i in 1:nrow(gear_ratios)){
    gear_ratios$check = length(which(gear_ratios$pos %in% gear_ratios$pos[i]))
}

gear_ratios = gear_ratios[which(gear_ratios$check == 2),]

gearing = data.frame(pos = unique(gear_ratios$pos),
                     prod = 0)

for(i in 1:nrow(gearing)){
    gearing$prod[i] = prod(gear_ratios$gear[which(gear_ratios$pos == gearing$pos[i])])
}

sum(gearing$prod)

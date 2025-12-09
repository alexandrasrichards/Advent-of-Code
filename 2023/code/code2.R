day = 2


input = as.data.frame(read.table(paste("../input/","input",day,".txt",sep=""), sep = ":",header=FALSE,fill=TRUE))

min_balls <- data.frame(ID = seq(1,nrow(input)),
                        reds = 0,
                        greens = 0,
                        blues = 0,
                        total = 0)

for(i in 1:nrow(input)){
    j = 2
    while(input[i,j] != ""){
        this_draw = t(as.data.frame(str_split(unlist(str_split(str_trim(input[i,j]),", "))," ")))
        if("red" %in% this_draw[,2]){
            red_count = as.numeric(this_draw[which(this_draw[,2] == "red"),1])
            if(red_count > min_balls$reds[i]){min_balls$reds[i] = red_count}
        }
        if("blue" %in% this_draw[,2]){
            blue_count = as.numeric(this_draw[which(this_draw[,2] == "blue"),1])
            if(blue_count > min_balls$blues[i]){min_balls$blues[i] = blue_count}
        }
        if("green" %in% this_draw[,2]){
            green_count = as.numeric(this_draw[which(this_draw[,2] == "green"),1])
            if(green_count > min_balls$greens[i]){min_balls$greens[i] = green_count}
        }
        j = j + 1
        if(j > ncol(input)){break}
        
    }
}

allowed = min_balls[which(min_balls$reds < 13),]
allowed = allowed[which(allowed$greens < 14),]
allowed = allowed[which(allowed$blues < 15),]
sum(allowed$ID)

min_balls$power = min_balls$reds*min_balls$greens*min_balls$blues
sum(min_balls$power)

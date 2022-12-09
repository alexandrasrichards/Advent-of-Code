## Day 9
library(here)

input <- read.csv(here("data","data 9.csv"), header = FALSE, sep = " ")

head <- as.data.frame(matrix(ncol = 2, nrow = nrow(input)))
tail <- as.data.frame(matrix(ncol = 2, nrow = nrow(input)))
names(head) <- c("X","Y")
names(tail) <- c("X","Y")


head[1,] <- c(0,0)
tail[1,] <- c(0,0)

count = 1
for(k in 1:nrow(input)) {
    for(i in 1:input[k,2]){
        count = count + 1
        if(input[k,1] == "R"){
            head[count,1] = head[count-1,1] + 1
            head[count,2] = head[count-1,2]
        } else if(input[k,1] == "L"){
            head[count,1] = head[count-1,1] - 1
            head[count,2] = head[count-1,2]
        } else if(input[k,1] == "U"){
            head[count,1] = head[count-1,1] 
            head[count,2] = head[count-1,2] + 1
        } else if(input[k,1] == "D"){
            head[count,1] = head[count-1,1] 
            head[count,2] = head[count-1,2] - 1
        }
        x_diff <- head[count,1] - tail[count-1,1]
        y_diff <- head[count,2] - tail[count-1,2]
        
        if(max(c(abs(x_diff),abs(y_diff)))>1){
            if(y_diff==0){
                tail[count,2] = tail[count-1,2]
                if(x_diff==2){
                    tail[count,1] = tail[count-1,1]+1
                } else if(x_diff==-2){
                    tail[count,1] = tail[count-1,1]-1
                }
            } else if(x_diff==0){
                tail[count,1] = tail[count-1,1]
                if(y_diff==2){
                    tail[count,2] = tail[count-1,2]+1
                } else if(y_diff==-2){
                    tail[count,2] = tail[count-1,2]-1
                }
            } else {
                # x = 2
                if(x_diff==2 && y_diff==1){
                    tail[count,1] = tail[count-1,1] + 1
                    tail[count,2] = tail[count-1,2] + 1
                } else if(x_diff==-2 && y_diff==1){
                    tail[count,1] = tail[count-1,1] - 1
                    tail[count,2] = tail[count-1,2] + 1
                } else if(x_diff==2 && y_diff==-1){
                    tail[count,1] = tail[count-1,1] + 1
                    tail[count,2] = tail[count-1,2] - 1
                } else if(x_diff==-2 && y_diff==-1){
                    tail[count,1] = tail[count-1,1] - 1
                    tail[count,2] = tail[count-1,2] - 1
                } else if(x_diff==1 && y_diff==2){
                    tail[count,1] = tail[count-1,1] + 1
                    tail[count,2] = tail[count-1,2] + 1
                } else if(x_diff==-1 && y_diff==2){
                    tail[count,1] = tail[count-1,1] - 1
                    tail[count,2] = tail[count-1,2] + 1
                } else if(x_diff==1 && y_diff==-2){
                    tail[count,1] = tail[count-1,1] + 1
                    tail[count,2] = tail[count-1,2] - 1
                } else if(x_diff==-1 && y_diff==-2){
                    tail[count,1] = tail[count-1,1] - 1
                    tail[count,2] = tail[count-1,2] - 1
                } 
            }
        } else {
            tail[count,] = tail[count-1,]
        }
    }
}

tail$pairs <- paste(tail$X, tail$Y, sep = ",")
length(unique(tail$pairs))


## Part 2
check_tail <- function(head_x, head_y, tail_x, tail_y){
    x_diff <- head_x - tail_x
    y_diff <- head_y - tail_y
    
    if(max(c(abs(x_diff),abs(y_diff)))>1){
        if(y_diff==0){
            new_tail_y = tail_y
            if(x_diff==2){
                new_tail_x = tail_x+1
            } else if(x_diff==-2){
                new_tail_x = tail_x-1
            }
        } else if(x_diff==0){
            new_tail_x = tail_x
            if(y_diff==2){
                new_tail_y = tail_y+1
            } else if(y_diff==-2){
                new_tail_y = tail_y-1
            }
        } else {
            # x = 2
            if(x_diff + y_diff>=3){
                new_tail_x = tail_x + 1
                new_tail_y = tail_y + 1
            } else if(x_diff==-2 && y_diff==1){
                new_tail_x = tail_x - 1
                new_tail_y = tail_y + 1
            } else if(x_diff==2 && y_diff==-1){
                new_tail_x = tail_x + 1
                new_tail_y = tail_y - 1
            } else if(x_diff + y_diff<=-3){
                new_tail_x = tail_x - 1
                new_tail_y = tail_y - 1
            } else if(x_diff==-1 && y_diff==2){
                new_tail_x = tail_x - 1
                new_tail_y = tail_y + 1
            } else if(x_diff==1 && y_diff==-2){
                new_tail_x = tail_x + 1
                new_tail_y = tail_y - 1
            } else if(x_diff == -2 && y_diff == 2){
                new_tail_x = tail_x - 1
                new_tail_y = tail_y + 1
            } else if(x_diff == 2 && y_diff == -2){
                new_tail_x = tail_x + 1
                new_tail_y = tail_y - 1
            }
        }
    } else {
        new_tail_x = tail_x
        new_tail_y = tail_y
    }
    return(c(new_tail_x, new_tail_y))
}

trail <- as.data.frame(matrix(ncol = 20, nrow = 1))
names(trail) <- c("HX","HY","1X","1Y","2X","2Y","3X","3Y","4X","4Y","5X","5Y","6X","6Y","7X","7Y","8X","8Y","9X","9Y")
count = 1
trail[1,] = 0
for(k in 1:nrow(input)) {
    for(i in 1:input[k,2]){
        count = count + 1
        if(input[k,1] == "R"){
            trail[count,"HX"] = trail[count-1,"HX"] + 1
            trail[count,"HY"] = trail[count-1,"HY"]
        } else if(input[k,1] == "L"){
            trail[count,"HX"] = trail[count-1,"HX"] - 1
            trail[count,"HY"] = trail[count-1,"HY"]
        } else if(input[k,1] == "U"){
            trail[count,"HX"] = trail[count-1,"HX"] 
            trail[count,"HY"] = trail[count-1,"HY"] + 1
        } else if(input[k,1] == "D"){
            trail[count,"HX"] = trail[count-1,"HX"] 
            trail[count,"HY"] = trail[count-1,"HY"] - 1
        }
        
        trail1 <- check_tail(trail[count,"HX"],trail[count,"HY"],trail[count-1,"1X"],trail[count-1,"1Y"])
        trail[count,"1X"] = trail1[1]
        trail[count,"1Y"] = trail1[2]
        
        trail2 <- check_tail(trail[count,"1X"],trail[count,"1Y"],trail[count-1,"2X"],trail[count-1,"2Y"])
        trail[count,"2X"] = trail2[1]
        trail[count,"2Y"] = trail2[2]
        
        trail3 <- check_tail(trail[count,"2X"],trail[count,"2Y"],trail[count-1,"3X"],trail[count-1,"3Y"])
        trail[count,"3X"] = trail3[1]
        trail[count,"3Y"] = trail3[2]
        
        trail4 <- check_tail(trail[count,"3X"],trail[count,"3Y"],trail[count-1,"4X"],trail[count-1,"4Y"])
        trail[count,"4X"] = trail4[1]
        trail[count,"4Y"] = trail4[2]
        
        trail5 <- check_tail(trail[count,"4X"],trail[count,"4Y"],trail[count-1,"5X"],trail[count-1,"5Y"])
        trail[count,"5X"] = trail5[1]
        trail[count,"5Y"] = trail5[2]
        
        trail6 <- check_tail(trail[count,"5X"],trail[count,"5Y"],trail[count-1,"6X"],trail[count-1,"6Y"])
        trail[count,"6X"] = trail6[1]
        trail[count,"6Y"] = trail6[2]
        
        trail7 <- check_tail(trail[count,"6X"],trail[count,"6Y"],trail[count-1,"7X"],trail[count-1,"7Y"])
        trail[count,"7X"] = trail7[1]
        trail[count,"7Y"] = trail7[2]
        
        trail8 <- check_tail(trail[count,"7X"],trail[count,"7Y"],trail[count-1,"8X"],trail[count-1,"8Y"])
        trail[count,"8X"] = trail8[1]
        trail[count,"8Y"] = trail8[2]
        
        trail9 <- check_tail(trail[count,"8X"],trail[count,"8Y"],trail[count-1,"9X"],trail[count-1,"9Y"])
        trail[count,"9X"] = trail9[1]
        trail[count,"9Y"] = trail9[2]
    }
}

trail$tail <- paste(trail$`9X`, trail$`9Y`, sep = ",")
length(unique(trail$tail))

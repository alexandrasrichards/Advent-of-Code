## Day 1
library(here)

### Part 1
#### Example data
data_ex <- read.csv(here::here("data","example 1-1.csv"), blank.lines.skip = FALSE, header = FALSE)

cal_max <- 0
cal_cur <- 0

for(i in 1:nrow(data_ex)){
    if(is.na(data_ex[i,1])){
        if (cal_cur > cal_max) {
            cal_max <- cal_cur
            cal_cur <- 0
        } else {
            cal_cur <- 0
        }
    } else {
        cal_cur <- cal_cur + data_ex[i,1]
    }
}


#### Real data
data_re <- read.csv(here::here("data","data 1-1.csv"), blank.lines.skip = FALSE, header = FALSE)

cal_max <- 0
cal_cur <- 0

for(i in 1:nrow(data_re)){
    if(is.na(data_re[i,1])){
        if (cal_cur > cal_max) {
            cal_max <- cal_cur
            cal_cur <- 0
        } else {
            cal_cur <- 0
        }
    } else {
        cal_cur <- cal_cur + data_re[i,1]
    }
}

### Part 2

top_3 <- function(data){
    n_cal <- c()
    cal_cur <- 0
    
    for(i in 1:(nrow(data)+1)){
        if(is.na(data[i,1])){
            n_cal <- c(n_cal, cal_cur)
            cal_cur <- 0
        }
        else {
            cal_cur <- cal_cur + data[i,1]
        }
    }
    tot_cal <- sum(head(sort(n_cal, decreasing = TRUE), n = 3))
    return(tot_cal)
}

top_3(data_ex)
top_3(data_re)

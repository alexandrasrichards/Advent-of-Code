setwd("~/Documents/GitHub/Advent-of-Code/2024")

library(readr)

data <- read_delim("./data/day2.csv", delim = "\n",col_names = FALSE)

part1 <- apply(data,1,function(x) {
  y = diff(as.numeric(unlist(str_split(x," "))));
  ifelse(all(y > 0) && max(y) < 4, "safe",
         ifelse(all(y < 0) && min(y) > -4, "safe",
                "unsafe"))
  
  })

length(which(part1 == "safe"))


part2 <- c()

for(i in 1:nrow(data)){
  a = as.numeric(unlist((str_split(data[i,], ' '))))
  y = diff(a)
  part2[i] <-   ifelse(all(y > 0) && max(y) < 4, "safe",
                  ifelse(all(y < 0) && min(y) > -4, "safe",
                         "unsafe"))
  if(part2[i] == "unsafe"){
    count = 1
    while(count <= length(a)){
      y = diff(a[-count])
      part2[i] <-   ifelse(all(y > 0) && max(y) < 4, "safe",
                           ifelse(all(y < 0) && min(y) > -4, "safe",
                                  "unsafe"))
      count = count + 1
      if(part2[i] == "safe"){
        count = length(a) + 1
      }
      
    }
  }
}

length(which(part2 == "safe"))

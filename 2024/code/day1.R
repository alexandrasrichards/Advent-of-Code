setwd("~/Documents/GitHub/Advent-of-Code/2024")

library(readr)

data <- read_delim("./data/day1.csv", delim = " ", col_names = FALSE)


part1 <- sum(abs(unname(sort(unlist(data[,1]))) - unname(sort(unlist(data[,4])))))

part2 <- sum(unlist(apply(data[,1], 1, function(x) x * length(which(data[,4] == x)))))

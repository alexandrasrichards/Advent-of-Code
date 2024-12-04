setwd("~/Documents/GitHub/Advent-of-Code/2024")

library(readr)
library(stringr)

data <- read_file("./data/day3.txt")

part1 <- sum(unlist(apply(as.data.frame(unlist(str_extract_all(data, "mul\\([0-9]{1,3},[0-9]{1,3}\\)"))),1,
                          function(x) prod(as.numeric(unlist(str_extract_all(x,"[0-9]{1,3}")))))))

part2_data <- sapply(str_split(data, "do\\(\\)") , function(x) gsub("don't\\(\\).*", "",x))
part2 <- sum(unlist(apply(as.data.frame(unlist(str_extract_all(paste0(part2_data, collapse = ""), "mul\\([0-9]{1,3},[0-9]{1,3}\\)"))),1,
                          function(x) prod(as.numeric(unlist(str_extract_all(x,"[0-9]{1,3}")))))))
             
                     
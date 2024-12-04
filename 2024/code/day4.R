setwd("~/Documents/GitHub/Advent-of-Code/2024")

library(readr)
library(stringr)

data <- read_csv("./data/day4.txt", col_names = FALSE) %>%
  apply(1,function(x) str_split(x, pattern = "")) %>%
  as.data.frame() %>%
  t()

names(data) = c()
row.names(data) = c()

check.neighbours <- function(data, col, row, search.for, search.direction = "all"){
  if(search.direction == "all"){
    search.direction = c("tl","t","tr","cl","cr","bl","b","br")
  }
  #tl #t #tr
  #cl ## #cr
  #bl #b #br
  good.neighbours <- c()
  if("tl" %in% search.direction & data[row - 1, col - 1] == search.for){
    good.neighbours = c(good.neighbours, "tl")
  }
  if("t" %in% search.direction & data[row - 1, col     ] == search.for){
    good.neighbours = c(good.neighbours, "t")
  }
  if("tr" %in% search.direction & data[row - 1, col + 1] == search.for){
    good.neighbours = c(good.neighbours, "tr")
  }
  if("cl" %in% search.direction & data[row    , col - 1] == search.for){
    good.neighbours = c(good.neighbours, "cl")
  }
  if("cr" %in% search.direction & data[row    , col + 1] == search.for){
    good.neighbours = c(good.neighbours, "cr")
  }
  
  if("bl" %in% search.direction & data[row + 1, col - 1] == search.for){
    good.neighbours = c(good.neighbours, "bl")
  }
  if("b" %in% search.direction & data[row + 1, col     ] == search.for){
    good.neighbours = c(good.neighbours, "b")
  }
  if("br" %in% search.direction & data[row + 1, col + 1] == search.for){
    good.neighbours = c(good.neighbours, "br")
  }
  return(good.neighbours)
}

count = 0
for(i in 1:ncol){
  for(j in 1:nrow){
    if(data[j,i] == "X"){
      check.neighbours(data, i, j, "A")
    }
  }
}

lr <- as.data.frame(matrix(data = c("X","M","A","S"),ncol = 4))
rl <- as.data.frame(matrix(data = c("S","A","M","X"),ncol = 4))
tb <- as.data.frame(matrix(data = c("X","M","A","S"),ncol = 1))
bt <- as.data.frame(matrix(data = c("S","A","M","X"),ncol = 1))
rd <- as.data.frame(matrix(data = c("X",".",".",".",".","M",".",".",".",".","A",".",".",".",".","S"),ncol = 4))
ru <- as.data.frame(matrix(data = c(".",".",".","X",".",".","M",".",".","A",".",".","S",".",".","."),ncol = 4))
ld <- as.data.frame(matrix(data = c("S",".",".",".",".","A",".",".",".",".","M",".",".",".",".","X"),ncol = 4))
lu <- as.data.frame(matrix(data = c(".",".",".","S",".",".","A",".",".","M",".",".","X",".",".","."),ncol = 4))

cross_match <- function(data, pattern, pattern.length = 4){
  #browser()
  matches = 0
  for(i in 1:(nrow(data)-(nrow(pattern)-1))){
    for(j in 1:(ncol(data) - (ncol(pattern)-1))){
      if(sum(data[i:(i + nrow(pattern)-1), j:(j + ncol(pattern) - 1)] == pattern) == pattern.length){
        matches = matches + 1
      }
    }
  }
  return(matches)
}

cross_match(data, lr) + cross_match(data, rl) +cross_match(data, tb) + cross_match(data, bt) + cross_match(data, rd) + cross_match(data, ru) +cross_match(data, ld) + cross_match(data, lu)

a <- as.data.frame(matrix(data = c("M",".","M",".","A",".","S",".","S"),ncol = 3))
b <- as.data.frame(matrix(data = c("S",".","M",".","A",".","S",".","M"),ncol = 3))
c <- as.data.frame(matrix(data = c("M",".","S",".","A",".","M",".","S"),ncol = 3))
d <- as.data.frame(matrix(data = c("S",".","S",".","A",".","M",".","M"),ncol = 3))

cross_match(data,a,5) + cross_match(data,b,5) + cross_match(data,c,5) + cross_match(data,d,5)


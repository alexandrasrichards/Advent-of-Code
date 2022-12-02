## Day 2
library(here)

data <- read.csv(here("data","data 2-1.csv"), header = FALSE, sep = " ")
#data <- data.frame(X1 <- c("A","B","C"),
#                   X2 <- c("Y","X","Z"))
names(data) <- c("elves","me")
data$points <- 0
data$points[which(data$me=="X")]=1
data$points[which(data$me=="Y")]=2
data$points[which(data$me=="Z")]=3


data$win <- 0
## win/lose/draw options
## AX, AY, AZ, BX, BY, BZ, CX, CY, CZ
## win: AY, BZ, CX
## lose: AZ, BX, CY
## draw: AX, BY, CZ

data$comb <- paste(data$elves,data$me, sep = "")
data$win[which(data$comb %in% c("AY","BZ","CX"))] = 6
data$win[which(data$comb %in% c("AX","BY","CZ"))] = 3
data$win[which(data$comb %in% c("AZ","BX","CY"))] = 0

sum(data$points, data$win)

## part 2

data$win <- 0
data$win[which(data$me=="X")]=0
data$win[which(data$me=="Y")]=3
data$win[which(data$me=="Z")]=6

## win/lose/draw options
## X = lose, if A -> C, B -> A, C -> B 
## Y = draw, if A -> A, B -> B, C -> C
## Z = win, if A-> B, B -> C, C -> A

## XA = C, XB = A, XC = B
## YA = A, YB = B, YC = C

data$points <- 0
data$points[which(data$comb %in% c("AX","CY","BZ"))] <- 3 # play C 
data$points[which(data$comb %in% c("CX","BY","AZ"))] <- 2 # play B
data$points[which(data$comb %in% c("BX","AY","CZ"))] <- 1

sum(data$win, data$points)

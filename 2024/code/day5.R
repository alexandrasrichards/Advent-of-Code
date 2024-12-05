setwd("~/Documents/GitHub/Advent-of-Code/2024")

library(readr)
library(stringr)

data <- read_lines("./data/day5.txt")
data_rules <- data[which(str_detect(data, "\\|"))] %>%
  str_split("\\|") %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()

data_print <- data[-c(which(str_detect(data, "\\|")), which(data == ""))]%>%
  str_split(",")

## part 1
rules_true <- c()
for(i in 1:length(data_print)){
  rules <- apply(data_rules, 1, function(x) all(x %in% data_print[[i]]))
  test <- c()
  for(j in which(rules)){
    if(which(data_print[[i]] == data_rules[j,1]) < which(data_print[[i]] == data_rules[j,2])){
      test <- c(test, TRUE)
    } else {
      test <- c(test, FALSE)
    }
  }
  if(all(test)){
    rules_true <- c(rules_true,i)
  }
}

total <- 0
for(x in rules_true){
  print(data_print[[x]])
  pos <- (length(data_print[[x]]) + 1)/2
  total <- total + as.numeric(data_print[[x]][pos])
}

## part 2
incorrect <- data_print[-rules_true]

count <- 0
for(i in 1:length(incorrect)){
  rules <- as.data.frame(table(data_rules[apply(data_rules, 1, function(x) all(x %in% incorrect[[i]])),1]))
  count <- count + as.numeric(as.character(rules[which(rules$Freq == ((length(incorrect[[i]]) - 1)/2)),1]))
}


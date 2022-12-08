## Day 7

library(here)

input <- read.csv(here("data","data 7.csv"), header = FALSE)

directory <- data.frame("parent" = c("a"),
                        "level" = c(0),
                        "type" = c(0),
                        "size" = c(0),
                        "name" = c(0))

file_level <- 0
parent_folder <- c()
count_files <- 0

for(i in 1:nrow(input)){
    info <- strsplit(input[i,], " ")
    if(info[[1]][1] == "$"){
        if(info[[1]][2] == "cd"){
            if(info[[1]][3] == ".."){
                file_level = file_level - 1
                parent_folder = head(parent_folder,-1)
            } else {
                file_level = file_level + 1
                parent_folder = c(parent_folder, info[[1]][3])
            }
        }
    } else if(info[[1]][1] == "dir") {
        count_files = count_files + 1
        directory[count_files,] = 0
        directory$parent[count_files] = paste(parent_folder, collapse = " ")
        directory$level[count_files] = file_level 
        directory$type[count_files] = "folder"
        directory$size[count_files] = 0
        directory$name[count_files] = info[[1]][2]
        directory$count[count_files] = count_files
        
    } else {
        count_files = count_files + 1
        directory[count_files,] = 0
        directory$parent[count_files] = paste(parent_folder, collapse = " ")
        directory$level[count_files] = file_level 
        directory$type[count_files] = "file"
        directory$size[count_files] = info[[1]][1]
        directory$name[count_files] = info[[1]][2]
        directory$count[count_files] = count_files
    }
    
}

for(j in (max(directory$level):2)) {
    parents <- directory[which(directory$level == (j-1)),]
    parents <- parents[which(parents$type == "folder"),]
    
    children <- directory[which(directory$level == (j)),]
    
    for(k in 1:nrow(children)){
        children$direct_parent[k] <- strsplit(children$parent, " " )[[k]][j]
        children$parent_filepath[k] <- paste(strsplit(children$parent, " ")[[k]][1:(j-1)], collapse = " ")
    }
    
    for(l in 1:nrow(parents)){
        same_name <- children[which(children$direct_parent == parents$name[l]),]
        same_file <- same_name[which(same_name$parent_filepath == parents$parent[l]),]
        parents$size[l] = sum(as.numeric(same_file$size))
        directory$size[which(directory$count == parents$count[l])] = as.numeric(parents$size[l])
    }
    
}

folders <- directory[which(directory$type == "folder"),]
small_folders <- sum(as.numeric(folders$size[which(as.numeric(folders$size) <= 100000)]))


## part 2
total_space <- 70000000
space_needed <- 30000000

used_space <- sum(as.numeric(directory$size[which(directory$parent == "/")]))
free_space <- total_space - used_space

free_up <- space_needed - free_space


options <- directory[which(as.numeric(directory$size)>free_up),]
min(as.numeric(options$size))

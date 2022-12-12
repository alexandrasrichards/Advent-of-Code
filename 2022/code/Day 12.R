## Day 12

library(here)
input <- read.csv(here("data","data 12.csv"), header = FALSE)

heights <- as.data.frame(matrix(nrow = nrow(input), ncol = nchar(input[1,])))


for(i in 1:nrow(input)) {
    for (j in 1:nchar(input[1,])) {
        heights[i,j] = substr(input[i,],j,j)
    }
}

heights_copy = heights


start_pos = paste(which(heights == "S", arr.ind = TRUE), collapse = ", ")
end_pos = paste(which(heights == "E", arr.ind = TRUE), collapse = ", ")
start_row = which(heights == "S", arr.ind = TRUE)[,1]
start_col = which(heights == "S", arr.ind = TRUE)[,2]
current_pos = start_pos

heights[which(heights == "S", arr.ind = TRUE)] = "a"
heights[which(heights == "E", arr.ind = TRUE)] = "z"

visited_points = data.frame(elevation = c(0), pos = c(0), position_row = c(0), position_col = c(0),distance = as.numeric(c(0)), children = c(0))

visited_points[1,] = c("a", start_pos, start_row, start_col, 0, "No")

current_i = 1
while(!(end_pos %in% visited_points$pos)) {
   if(visited_points$children[current_i] == "No"){
       ## up
       up_row = as.numeric(visited_points$position_row[current_i]) - 1
       if(up_row >= 1){
       up_col = as.numeric(visited_points$position_col[current_i])
       up_pos = paste(up_row, up_col, sep =  ", ")
       up_hei = heights[up_row, up_col]
       if(which(letters == up_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
           if(!(up_pos %in% visited_points$pos)){
               len = nrow(visited_points)
               visited_points[len + 1,] = c(up_hei, up_pos, up_row, up_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
           }
       }
       }
       
       ## left
       left_row = as.numeric(visited_points$position_row[current_i])
       left_col = as.numeric(visited_points$position_col[current_i]) - 1
       if(left_col >= 1){
       left_pos = paste(left_row, left_col, sep = ", ")
       left_hei = heights[left_row, left_col]
       if(which(letters == left_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
           if(!(left_pos %in% visited_points$pos)){
               len = nrow(visited_points)
               visited_points[len + 1,] = c(left_hei, left_pos, left_row, left_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
           }
       }
       }
       
       ## down
       down_row = as.numeric(visited_points$position_row[current_i]) + 1
       if(down_row <= nrow(heights)){
       down_col = as.numeric(visited_points$position_col[current_i])
       down_pos = paste(down_row, down_col, sep =  ", ")
       down_hei = heights[down_row, down_col]
       if(which(letters == down_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
           if(!(down_pos %in% visited_points$pos)){
               len = nrow(visited_points)
               visited_points[len + 1,] = c(down_hei, down_pos, down_row, down_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
           }
       }
       }
       
       ## right
       right_row = as.numeric(visited_points$position_row[current_i])
       right_col = as.numeric(visited_points$position_col[current_i]) + 1
       if(right_col<=ncol(heights)){
       right_pos = paste(right_row, right_col, sep = ", ")
       right_hei = heights[right_row, right_col]
       if(which(letters == right_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
           if(!(right_pos %in% visited_points$pos)){
               len = nrow(visited_points)
               visited_points[len + 1,] = c(right_hei, right_pos, right_row, right_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
           }
       }
       }
   }
    visited_points$children[current_i] = "Yes"
    current_i = current_i + 1
    current_pos = visited_points$pos[i]
}

## part 2
shortest_paths = c()

for(x in 1:nrow(heights)){
    for(y in 1:3){
        if(heights[x,y] == "a"){
            start_pos = paste(x,y, sep = ", ")
            start_row = x
            start_col = y
            current_pos = start_pos
            
            visited_points = data.frame(elevation = c(0), pos = c(0), position_row = c(0), position_col = c(0),distance = as.numeric(c(0)), children = c(0))
            
            visited_points[1,] = c("a", start_pos, start_row, start_col, 0, "No")
            
            current_i = 1
            while(!(end_pos %in% visited_points$pos)) {
                if(visited_points$children[current_i] == "No"){
                    ## up
                    up_row = as.numeric(visited_points$position_row[current_i]) - 1
                    if(up_row >= 1){
                        up_col = as.numeric(visited_points$position_col[current_i])
                        up_pos = paste(up_row, up_col, sep =  ", ")
                        up_hei = heights[up_row, up_col]
                        if(which(letters == up_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
                            if(!(up_pos %in% visited_points$pos)){
                                len = nrow(visited_points)
                                visited_points[len + 1,] = c(up_hei, up_pos, up_row, up_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
                            }
                        }
                    }
                    
                    ## left
                    left_row = as.numeric(visited_points$position_row[current_i])
                    left_col = as.numeric(visited_points$position_col[current_i]) - 1
                    if(left_col >= 1){
                        left_pos = paste(left_row, left_col, sep = ", ")
                        left_hei = heights[left_row, left_col]
                        if(which(letters == left_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
                            if(!(left_pos %in% visited_points$pos)){
                                len = nrow(visited_points)
                                visited_points[len + 1,] = c(left_hei, left_pos, left_row, left_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
                            }
                        }
                    }
                    
                    ## down
                    down_row = as.numeric(visited_points$position_row[current_i]) + 1
                    if(down_row <= nrow(heights)){
                        down_col = as.numeric(visited_points$position_col[current_i])
                        down_pos = paste(down_row, down_col, sep =  ", ")
                        down_hei = heights[down_row, down_col]
                        if(which(letters == down_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
                            if(!(down_pos %in% visited_points$pos)){
                                len = nrow(visited_points)
                                visited_points[len + 1,] = c(down_hei, down_pos, down_row, down_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
                            }
                        }
                    }
                    
                    ## right
                    right_row = as.numeric(visited_points$position_row[current_i])
                    right_col = as.numeric(visited_points$position_col[current_i]) + 1
                    if(right_col<=ncol(heights)){
                        right_pos = paste(right_row, right_col, sep = ", ")
                        right_hei = heights[right_row, right_col]
                        if(which(letters == right_hei) <= which(letters == visited_points$elevation[current_i]) + 1){
                            if(!(right_pos %in% visited_points$pos)){
                                len = nrow(visited_points)
                                visited_points[len + 1,] = c(right_hei, right_pos, right_row, right_col, as.numeric(visited_points$distance[current_i]) + 1, "No")
                            }
                        }
                    }
                } 
                visited_points$children[current_i] = "Yes"
                current_i = current_i + 1
                current_pos = visited_points$pos[i]
            }
            shortest_paths = c(shortest_paths,visited_points$distance[which(visited_points$pos == end_pos)])
        }

    }
}



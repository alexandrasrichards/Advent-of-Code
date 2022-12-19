library(here)
data <- read.delim(here("data","data 18.txt"), header = FALSE, sep = "\n")

find_sides <- function(coords){
    p = unlist(strsplit(coords, split = ","))
    x = as.numeric(p[1])
    y = as.numeric(p[2])
    z = as.numeric(p[3])
    
    blf = paste("(",x,",",y,",",z,")", sep = "")
    brf = paste("(",x + 1,",",y,",",z,")", sep = "")
    blb = paste("(",x,",",y + 1,",",z,")", sep = "")
    brb = paste("(",x + 1,",",y + 1,",",z,")", sep = "")
    tlf = paste("(",x,",",y,",",z + 1,")", sep = "")
    trf = paste("(",x + 1,",",y,",",z + 1,")", sep = "")
    tlb = paste("(",x,",",y + 1,",",z + 1,")", sep = "")
    trb = paste("(",x + 1,",",y + 1,",",z + 1,")", sep = "")
    
    side1 = paste(blf, brf, tlf, trf, sep = ",")
    side2 = paste(blb, brb, tlb, trb, sep = ",")
    side3 = paste(blf, blb, tlf, tlb, sep = ",")
    side4 = paste(brf, brb, trf, trb, sep = ",")
    side5 = paste(blf, brf, blb, brb, sep = ",")
    side6 = paste(tlf, trf, tlb, trb, sep = ",")
    
    return(c(side1, side2, side3, side4, side5, side6))
}

all_sides = data.frame(input = data[,1], side1 = 0, side2 = 0, side3 = 0, side4 = 0, side5 = 0, side6 = 0, sides = 6)

for(i in seq(1, nrow(all_sides),1)){
    all_sides[i,2:7] = find_sides(all_sides[i,1])
}

for(i in seq(1, nrow(all_sides),1)){
    if(all_sides[i,2] %in% all_sides[,3]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,3] %in% all_sides[,2]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,4] %in% all_sides[,5]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,5] %in% all_sides[,4]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,6] %in% all_sides[,7]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,7] %in% all_sides[,6]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
}

sum(all_sides$sides)



### part 2

find_points <- function(coords){
    p = unlist(strsplit(coords, split = ","))
    x = as.numeric(p[1])
    y = as.numeric(p[2])
    z = as.numeric(p[3])
    
    return(c(x,y,z))
}

points <- data.frame(input = data[,1], x = 0, y = 0, z = 0)

for(i in seq(1, nrow(points),1)){
    points[i,2:4] = find_points(points[i,1])
}

top = max(points[,2:4])
bottom = min(points[,2:4])

points[,2:4] = points[,2:4] + 3

edge_up = max(points[,2:4]) + 2
edge_down = 1

all_points = array(data = "-", dim = c(edge_up, edge_up, edge_up))
all_points[1,1,1]

for(i in 1:nrow(points)) {
    all_points[points$x[i], points$y[i], points$z[i]] = "L"
}




bfs_to_edge <- function(all_points, start){
    #browser()
    start.x = start[1]
    start.y = start[2]
    start.z = start[3]
    
    end.x = 1
    end.y = 1
    end.z = 1
    end = "1,1,1"
    
    visited_points = data.frame(x = start.x, y = start.y, z = start.z, checked = "N", points = paste(start.x, start.y, start.z, sep = ","))
    current = 0
    len = 1
    inner = TRUE
    while(!all(visited_points$checked == "Y")){
        current = current + 1
            
            new.x = as.numeric(visited_points[current,"x"]) - 1
            new.y = as.numeric(visited_points[current,"y"])
            new.z = as.numeric(visited_points[current,"z"])
            new = paste(new.x, new.y, new.z, sep = ",")
            if(new.x > 0){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }

            
            new.x = as.numeric(visited_points[current,"x"])
            new.y = as.numeric(visited_points[current,"y"]) - 1
            new.z = as.numeric(visited_points[current,"z"])
            new = paste(new.x, new.y, new.z, sep = ",")
            if(new.y > 0){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }
            
            
            new.x = as.numeric(visited_points[current,"x"])
            new.y = as.numeric(visited_points[current,"y"])
            new.z = as.numeric(visited_points[current,"z"]) - 1
            new = paste(new.x, new.y, new.z, sep = ",")
            if(new.z > 0){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }
            
            new.x = as.numeric(visited_points[current,"x"]) + 1
            new.y = as.numeric(visited_points[current,"y"])
            new.z = as.numeric(visited_points[current,"z"])
            new = paste(new.x, new.y, new.z, sep = ",")
            if(new.x < nrow(all_points)){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }
            
            
            new.x = as.numeric(visited_points[current,"x"])
            new.y = as.numeric(visited_points[current,"y"]) + 1
            new.z = as.numeric(visited_points[current,"z"])
            new = paste(new.x, new.y, new.z, sep = ",")
            if(new.y < nrow(all_points)){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }
            
            
            new.x = as.numeric(visited_points[current,"x"])
            new.y = as.numeric(visited_points[current,"y"])
            new.z = as.numeric(visited_points[current,"z"]) + 1
            new = paste(new.x, new.y, new.z, sep = ",")

            if(new.z < nrow(all_points)){
                if(all_points[new.x, new.y, new.z]!= "L"){
                    if(!(new %in% visited_points$points)){
                        print(new)
                        len = len + 1
                        visited_points[len,] = c(new.x, new.y, new.z, "N", new)
                    }
                }
            }
            
            visited_points[current,"checked"] = "Y"

        }

    return(visited_points)
}


visits = bfs_to_edge(all_points, c(1,1,1))

for(i in 1:nrow(visits)){
    all_points[as.numeric(visits$x[i]),as.numeric(visits$y[i]), as.numeric(visits$z[i])] = "X"
}

all_points[24,,] = "X"
all_points[,24,] = "X"
all_points[,,24] = "X"

inside = data.frame(x = 0,y = 0,z = 0)
count = 0
for(i in 1:24){
    for(j in 1:24){
        for(k in 1:24){
            if(all_points[i,j,k]!="L" && all_points[i,j,k]!="X"){
                count = count + 1
                inside[count,] = c(i,j,k)
            }
        }
    }
}

inside$points = paste(inside$x, inside$y, inside$z, sep = ",")

all_sides = data.frame(input = inside[,4], side1 = 0, side2 = 0, side3 = 0, side4 = 0, side5 = 0, side6 = 0, sides = 6)

for(i in seq(1, nrow(all_sides),1)){
    all_sides[i,2:7] = find_sides(all_sides[i,1])
}

for(i in seq(1, nrow(all_sides),1)){
    if(all_sides[i,2] %in% all_sides[,3]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,3] %in% all_sides[,2]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,4] %in% all_sides[,5]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,5] %in% all_sides[,4]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,6] %in% all_sides[,7]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
    if(all_sides[i,7] %in% all_sides[,6]){
        all_sides$sides[i] = as.numeric(all_sides$sides[i]) - 1 
    }
    
}

sum(all_sides$sides)

3466 - 1454

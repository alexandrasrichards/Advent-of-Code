library(here)
input = read.csv(here("data","data 16.csv"), sep = "\n", header = FALSE)

input$pos = apply(input, 1, function(x) strsplit(strsplit(x, "Valve ")[[1]][2]," ")[[1]][1])
input$release = apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "rate=")[[1]][2],"; ")[[1]][1]))
input$tunnels = apply(input, 1, function(x) strsplit(x, "valves ")[[1]][2])
for(i in 1:nrow(input)){
    if(is.na(input$tunnels[i])){
        print(i)
        input$tunnels[i] = strsplit(input[i,1], "valve ")[[1]][2]
    }
}
input$tunnels2 = strsplit(input$tunnels,", ")

n_pressured <- length(which(input$release > 0))
pos_pressured <- input$pos[which(input$release > 0)]
pressure <- input$release[which(input$release > 0)]

pressured_valves <- data.frame(start = c(rep("AA", n_pressured),
                                         rep(pos_pressured, each = n_pressured)),
                               end = c(rep(pos_pressured, times = n_pressured + 1)),
                               distance = 0,
                               pressure = c(rep(pressure, times = n_pressured + 1)))

find_distance <- function(start, end, input){
 
    visited_points = data.frame(valve = c(start),distance = c(0), children = c("No"))
    
    current_i = 1

    while(!(end %in% visited_points$valve)) {
        print(current_i)
        children = unlist(input$tunnels2[which(input$pos == visited_points$valve[current_i])])
        if(visited_points$children[current_i] == "No"){
            for(dirs in children){
                if(!(dirs %in% visited_points$valve)){
                len = nrow(visited_points)
                visited_points[len + 1,] = c(dirs,as.numeric(visited_points$distance[current_i]) + 1,"No")
            
                }
            }
            visited_points$children[current_i] = "Yes"
            current_i = current_i + 1
        }
    }
    
    return(visited_points$distance[which(visited_points$valve == end)])
    
}

for(start in unique(pressured_valves$start)){
    for(end in unique(pressured_valves$end)){
        row_num = intersect(which(pressured_valves$start == start), which(pressured_valves$end == end))
        print(row_num)
        pressured_valves$distance[row_num] = find_distance(start, end, input)
    }
    
}



find_paths <- function(time, points){
    browser()
    visited_points = data.frame(valve = c("AA"),time = c(time), children = c("No"), path = c("-"), pressure = c(0))
    
    current_i = 1
    while(!all(visited_points$children == "Yes")){
        print(current_i)
        children = points$end[which(points$start == visited_points$valve[current_i])]
        if(visited_points$children[current_i] == "No"){
            for(dirs in children){
                if(!(grepl(dirs, visited_points$path[current_i]))){
                    len = nrow(visited_points)
                    row_num = intersect(which(points$start == visited_points$valve[current_i]),
                                        which(points$end == dirs))
                    time_remaining = as.numeric(visited_points$time[current_i]) - as.numeric(points$distance[row_num]) - 1
                    pressure_released = time_remaining * as.numeric(points$pressure[row_num])
                    if(time_remaining > 0){
                        visited_points[len + 1,] = c(dirs, time_remaining, "No", 
                                                     paste(visited_points$path[current_i],dirs,", "),
                                                     as.numeric(visited_points$pressure[current_i]) + pressure_released)
                    }
                    
                    
                }
            }
            visited_points$children[current_i] = "Yes"
            current_i = current_i + 1
        }
    }
    

    
    return(visited_points)
}

paths = find_paths(30, pressured_valves)

paths_2 = find_paths(26, pressured_valves)
paths_2$path = sub("-","",paths_2$path)
paths_2$path = sub(" ","",paths_2$path)

for(i in 1:nrow(paths_2)){
    paths_2$unlisted[i] = list(strsplit(paths_2$path[i], ",")[[1]])
}

paths_2 = paths_2[order(as.numeric(paths_2$pressure), decreasing = TRUE),]

for(i in 1:nrow(paths_2)){
    paths_2$all_new[i] =  all(!(unlist(paths_2$unlisted[i]) %in% unlist(paths_2$unlisted[1])))
}

as.numeric(paths_2$pressure[1]) + as.numeric(paths_2$pressure[min(which(paths_2$all_new == TRUE))])


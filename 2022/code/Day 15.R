## Day 15

library(here)

input <- read.csv(here("data","data 15.csv"), header = FALSE, sep = c("\n"))

input$Sx <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "x=")[[1]][2],",")[[1]][1]))
input$Sy <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "y=")[[1]][2],":")[[1]][1]))
input$Bx <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "x=")[[1]][3],",")[[1]][1]))
input$By <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "y=")[[1]][3],":")[[1]][1]))

input$distance <- abs(input$Sx - input$Bx) + abs(input$Sy - input$By)

row = 2000000

input$ydisttorow <- abs(input$Sy - row)

input$xvaratrow <- ifelse(input$distance > input$ydisttorow, input$distance - input$ydisttorow, 0)

poss_x = c()
for(i in 1:nrow(input)) {
    if(input$xvaratrow[i] != 0){
        poss_x = c(poss_x, seq(input$Sx[i] - input$xvaratrow[i], input$Sx[i] + input$xvaratrow[i],1))
    }
}

if(row %in% input$Sy){
    poss_x = poss_x[poss_x != input$Sx[which(input$Sy == row)]]
}

if(row %in% input$By){
    not_x = unique(input$Bx[which(input$By == row)])
    for(i in length(not_x)){
        poss_x = poss_x[poss_x != not_x[i]]
    }
    
}

length(unique(poss_x))



## Part 2
edge = 4000000
 
input$possbroken = input$distance + 1

poss_broken_full = data.frame(x = c(), y = c(), pairs = c(), count = c())
count_broken = 0
allowed = seq(0,edge,1)

for(i in seq(1,nrow(input),1)){
    print(i)
    x = rep(seq(input$Sx[i] - input$possbroken[i],input$Sx[i] + input$possbroken[i],1),2)
    y = c(seq(input$Sy[i], input$Sy[i] + input$possbroken[i],1),seq(input$Sy[i] + input$possbroken[i]-1, input$Sy[i],-1),seq(input$Sy[i], input$Sy[i] - input$possbroken[i],-1),seq(input$Sy[i] - input$possbroken[i]+ 1, input$Sy[i],1))
    poss_broken = data.frame(x, y)
    if(min(poss_broken$x < 0)) {
        not_x = which(poss_broken$x < 0)
        poss_broken = poss_broken[-not_x,]
    }
    if(max(poss_broken$x > edge)) {
        not_x = which(poss_broken$x > edge)
        poss_broken = poss_broken[-not_x,]
    }
    if(min(poss_broken$y < 0)) {
        not_y = which(poss_broken$y < 0)
        poss_broken = poss_broken[-not_y,]
    }
    if(max(poss_broken$y > edge)) {
        not_y = which(poss_broken$y > edge)
        poss_broken = poss_broken[-not_y,]
    }
    
    poss_broken = poss_broken[!duplicated(poss_broken),]
    for(j in 1:nrow(poss_broken)){
        print(j)
        poss_broken$pairs[j] = paste(poss_broken$poss_x[j], poss_broken$poss_y[j], ", ")
        if (poss_broken$pairs[j] %in% poss_broken_full$pairs){
            poss_broken_full$count[which(poss_broken_full$pairs == poss_broken$pairs[j])] = poss_broken_full$count[which(poss_broken_full$pairs == poss_broken$pairs[j])]
        } else {
            poss_broken_full[nrow(poss_broken_full) + 1,] = c(poss_broken[j,],1)
        }
    }
    

    poss_broken_full = rbind(poss_broken_full, poss_broken)
}




### less smart method
edge = 4000000
allowed = seq(0,edge,1)


for(row in allowed){
    print(row)
    input$ydisttorow <- abs(input$Sy - row)
    
    input$xvaratrow <- ifelse(input$distance > input$ydisttorow, input$distance - input$ydisttorow, 0)
    
    poss_x = c()
    for(i in 1:nrow(input)) {
        if(input$xvaratrow[i] != 0){
            poss_x = c(poss_x, seq(input$Sx[i] - input$xvaratrow[i], input$Sx[i] + input$xvaratrow[i],1))
        }
    }
    poss_x = unique(poss_x)
    poss_x = poss_x[which(poss_x >=0)]
    poss_x = poss_x[which(poss_x <= max(allowed))]
    if (length(poss_x)!=length(allowed)){
        z = poss_x
        x = setdiff(allowed, poss_x)
        y = row
        break
    }
}

mandistance <- function(x1,y1,x2,y2){
    return(abs(x1-x2) + abs(y1-y2))
}

x * 4000000 + y

edges = data.frame(x = c(0),y = c(0))

for(sensor in 1:nrow(input)){
    print(sensor)
    edge = c(input$Sx[sensor] - (input$distance[sensor] + 1),input$Sy[sensor])
    edges[nrow(edges) + 1,] = edge
    while(mandistance(edge[1] + 1, edge[2] + 1, input$Sx[sensor], input$Sy[sensor]) == input$distance[sensor] + 1){
        edge  = edge + c(1,1)
        edges[nrow(edges) + 1,] = edge
    }
    while(mandistance(edge[1] + 1, edge[2] - 1, input$Sx[sensor], input$Sy[sensor]) == input$distance[sensor] + 1){
        edge  = edge + c(1,-1)
        edges[nrow(edges) + 1,] = edge
    }
    while(mandistance(edge[1] - 1, edge[2] - 1, input$Sx[sensor], input$Sy[sensor]) == input$distance[sensor] + 1){
        edge  = edge + c(-1,-1)
        edges[nrow(edges) + 1,] = edge
    }
    while(mandistance(edge[1] - 1, edge[2] + 1, input$Sx[sensor], input$Sy[sensor]) == input$distance[sensor] + 1){
        edge  = edge + c(-1,+1)
        edges[nrow(edges) + 1,] = edge
    }
        
}


data15 <- sapply(strsplit(gsub("\\D+", " ", readLines(here("data","data 15.txt"))), " "), as.integer)
dist <- abs(data15[2, ] - data15[4, ]) + abs(data15[3, ] - data15[5, ])



## intersects solution
input$top_left = input$Sy -input$Sx + input$distance 
input$bottom_right = input$Sy -input$Sx - input$distance 
input$top_right = input$Sx + input$Sy + input$distance 
input$bottom_left = input$Sx + input$Sy - input$distance 


count_test = 1
crossover = data.frame(x = c(0), y = c(0))
for(i in 1:nrow(input)){
    print(i)
    for(j in 1:nrow(input)){
        print(j)
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_left[i], input$top_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_left[i], input$top_right[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_left[i], input$bottom_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_left[i], input$bottom_right[j])
        
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_right[i], input$top_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_right[i], input$top_right[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_right[i], input$bottom_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$top_right[i], input$bottom_right[j])
        
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_left[i], input$top_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_left[i], input$top_right[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_left[i], input$bottom_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_left[i], input$bottom_right[j])
        
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_right[i], input$top_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_right[i], input$top_right[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_right[i], input$bottom_left[j])
        count_test = count_test + 1
        ## tl/tl
        crossover[count_test,] = return_intersect(input$bottom_right[i], input$bottom_right[j])

    }
}

return_intersect <- function(a,b){
    x = (as.numeric(b)-as.numeric(a))/2
    y = (as.numeric(a)+as.numeric(b))/2
    
    if(x >= 0 && y >= 0 && x <= 4000000 && y <= 400000){
        return(c(x, y))
    } else {
        return(c(0,0))
    }
}

intersect_x0 = which(crossover$x == 0)
intersect_y0 = which(crossover$y == 0)

equal0 = intersect(intersect_x0, intersect_y0)

crossover = crossover[-equal0,]

intersect_x.5= which(crossover$x %% 1 == 0.5)
intersect_y.5= which(crossover$y %% 1 == 0.5)
equal.5 = union(intersect_x.5, intersect_y.5)

crossover.5 = crossover[equal.5,]
crossover = crossover[-equal.5]

crossover.5fix = crossover.5
count_fix = 0
for(i in 1:nrow(crossover.5)){
    count_fix = count_fix + 1
    crossover.5fix[count_fix,] = c(crossover.5$x[i] + 0.5, crossover.5$y[i] + 0.5)
    
    count_fix = count_fix + 1
    crossover.5fix[count_fix,] = c(crossover.5$x[i] - 0.5, crossover.5$y[i] + 0.5)
    
    count_fix = count_fix + 1
    crossover.5fix[count_fix,] = c(crossover.5$x[i] + 0.5, crossover.5$y[i] - 0.5)
    
    count_fix = count_fix + 1
    crossover.5fix[count_fix,] = c(crossover.5$x[i] - 0.5, crossover.5$y[i] - 0.5)
    
}

crossover = rbind(crossover, crossover.5fix)

crossover.all = crossover
count_all = 0
for(i in 1:nrow(crossover)){
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] + 1, crossover$y[i] + 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] - 1, crossover$y[i] + 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] + 1, crossover$y[i] - 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] - 1, crossover$y[i] - 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] + 1, crossover$y[i] + 0)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] - 1, crossover$y[i] - 0)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] + 0, crossover$y[i] + 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] - 0, crossover$y[i] - 1)
    
    count_all = count_all + 1
    crossover.all[count_all,] = c(crossover$x[i] + 0, crossover$y[i] + 0)


}

for(i in 1:nrow(crossover.all)){
    count = 0
    for(j in 1:nrow(input)){
        if(mandistance(crossover.all$x[i], crossover.all$y[i], input$Sx[j], input$Sy[j])<= input$distance[j]){
            count = count + 1
        } 
    }
    if(count == 0){
        print(crossover.all$x[i] * 4000000 + crossover.all$y[i])
    }
}

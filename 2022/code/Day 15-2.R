library(here)

input <- read.csv(here("data","data 15.csv"), header = FALSE, sep = c("\n"))

input$Sx <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "x=")[[1]][2],",")[[1]][1]))
input$Sy <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "y=")[[1]][2],":")[[1]][1]))
input$Bx <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "x=")[[1]][3],",")[[1]][1]))
input$By <- apply(input, 1, function(x) as.numeric(strsplit(strsplit(x, "y=")[[1]][3],":")[[1]][1]))

input$distance <- abs(input$Sx - input$Bx) + abs(input$Sy - input$By)

mandistance <- function(x1,y1,x2,y2){
    return(abs(x1-x2) + abs(y1-y2))
}

find_cover <- function(cover){
    if (nrow(cover) < 2) { #
        return(cover)
    }
    else {
        min1 = cover[1,1]
        max1 = cover[1,2]
        min2 = cover[2,1]
        max2 = cover[2,2]
        if (max2 < max1){
            return(find_cover(cover[-2,]))
        } else if (min2 <= max1 + 1) {
            first = c(min1, max2)
            cover = rbind(first, cover[-c(1,2),])
            return(find_cover(cover))
        } else {
            cover2 = find_cover(cover[-1,])
            cover = rbind(cover[1,], cover2)
            return(cover)
        }
    }

}

max = 4000000
rowy = 0

for(row in seq(0,max,1)){
    print(row)
    covered = data.frame(a = 0,b = 0)
    for(i in seq(1, nrow(input))){
        xmin = input$Sx[i] - (input$distance[i] - abs(input$Sy[i] - row))
        xmax = input$Sx[i] + (input$distance[i] - abs(input$Sy[i] - row))
        if (xmin <= xmax){
            covered = rbind(covered, c(xmin, xmax))
        }
    }
    covered = covered[order(covered[,1], covered[,2]),]
    covered = find_cover(covered)
    if(covered[1,2]< max){
        print("hi")
        break
    }
}


input$ydisttorow <- abs(input$Sy - row)

input$xvaratrow <- ifelse(input$distance > input$ydisttorow, input$distance - input$ydisttorow, 0)

poss_x = c()
for(i in 1:nrow(input)) {
    if(input$xvaratrow[i] != 0){
        poss_x = c(poss_x, seq(input$Sx[i] - input$xvaratrow[i], input$Sx[i] + input$xvaratrow[i],1))
    }
}

poss_x = poss_x[which(poss_x >=0)]
poss_x = poss_x[which(poss_x <= max)]

x = setdiff(seq(0,4000000,1), poss_x)

sol = x*4000000 + row  ##13197439355220


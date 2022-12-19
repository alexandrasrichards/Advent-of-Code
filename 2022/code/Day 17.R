library(here)

wind <- read.delim(here("data","data 17.txt"), header = FALSE)
wind <- strsplit(as.character(wind), "")[[1]]

shapes <- data.frame(shape1.x = c(3,4,5,6,NA), shape1.y = c(0,0,0,0,NA),
                     shape2.x = c(4,3,4,5,4), shape2.y = c(0,1,1,1,2),
                     shape3.x = c(3,4,5,5,5), shape3.y = c(0,0,0,1,2),
                     shape4.x = c(3,3,3,3,NA), shape4.y = c(0,1,2,3,NA),
                     shape5.x = c(3,4,3,4,NA), shape5.y = c(0,0,1,1,NA))

tower = data.frame(x.coords = c(1,2,3,4,5,6,7),
                   y.coords = c(0,0,0,0,0,0,0))

tower$coords = paste("(",tower$x.coords,",",tower$y.coords,")", sep = "")

wind_count = 1
length_wind = length(wind)
shape_count = 1
windshape = c()


while(shape_count < 50000){
    print(shape_count)
    choose_shape = (shape_count-1) %% 5
    if(choose_shape == 0){
        shape.x = as.numeric(shapes$shape1.x[1:4])
        shape.y = as.numeric(shapes$shape1.y[1:4])
    } else if(choose_shape == 1){
        shape.x = as.numeric(shapes$shape2.x)
        shape.y = as.numeric(shapes$shape2.y)
    } else if(choose_shape == 2){
        shape.x = as.numeric(shapes$shape3.x)
        shape.y = as.numeric(shapes$shape3.y)
    } else if(choose_shape == 3){
        shape.x = as.numeric(shapes$shape4.x[1:4])
        shape.y = as.numeric(shapes$shape4.y[1:4])
    } else if(choose_shape == 4){
        shape.x = as.numeric(shapes$shape5.x[1:4])
        shape.y = as.numeric(shapes$shape5.y[1:4])
    }
    
    start_height = max(tower$y.coords) + 4
    shape.y = shape.y + start_height
    
    down_rock = 0
    local_count = 0
    windshape = c(windshape, paste("w:", wind_count,", s:", choose_shape, "h:", max(tower$y.coords), sep = ""))
    while(down_rock == 0){
        local_count = local_count + 1
        if (local_count %% 2 == 1){
            if(wind[wind_count] == ">"){
                shape.x.new = shape.x + 1
                if(8 %in% shape.x.new){
                    shape.x.new = shape.x
                }
            } else if(wind[wind_count] == "<"){
                shape.x.new = shape.x - 1
                if(0 %in% shape.x.new){
                    shape.x.new = shape.x
                }
            }
            shape.new = paste("(",shape.x.new,",",shape.y,")", sep = "")
            if(any(is.element(shape.new, tower$coords))){
                shape.x.new = shape.x
            }
            wind_count = wind_count + 1
            if(wind_count > length_wind){
                wind_count = 1
            }
            shape.x = shape.x.new
        } else {
            shape.y.new = shape.y - 1
            shape.new = paste("(",shape.x,",",shape.y.new,")", sep = "")
            if(any(is.element(shape.new, tower$coords))){
                down_rock = 1
                final.shape = data.frame(x.coords = shape.x,
                                         y.coords = shape.y,
                                         coords = paste("(",shape.x,",",shape.y,")", sep = ""))
                tower = rbind(tower, final.shape)
            } else {
                shape.y = shape.y.new
            }
        }
        
    }
    
    
    shape_count = shape_count + 1
}


max(tower$y.coords)
   
height_add = c()
for(i in seq(8,nrow(tower),22)){
    print(i)
    height = c(height, max(as.numeric(tower$y.coords[i:(i+21)])))
    if(i > 8){
        height_add[length(height)-1] = height[length(height)] - height[length(height) - 1]
    }
    
} 

which(grepl("w:2,",windshape))
which(grepl("w:8,",windshape))
which(grepl("w:13,",windshape))
which(grepl("w:17,",windshape))
which(grepl("w:26,",windshape))
which(grepl("w:34,",windshape))
which(grepl("w:5694,",windshape))
which(grepl("w:5657,",windshape))
which(grepl("w:2723,",windshape))
which(grepl("w:256,",windshape))

repeating_length  = 1715
repeat_start = 471

nrows_tower_5shapes = 4 + 4 + 5 + 5 + 4

max_repeat_start = (7 + (repeat_start-1)*nrows_tower_5shapes/5)
height_start = max(as.numeric(tower$y.coords[1:max_repeat_start]))    
shape_loops_repeating = repeating_length/5
shape_loops_nrows = shape_loops_repeating * nrows_tower_5shapes

height_loop = max(as.numeric(tower$y.coords[(max_repeat_start + 1 ):(max_repeat_start + 1 + shape_loops_nrows)]))- height_start
#height_1726 = max(as.numeric(tower$y.coords[1:(1725/5 * nrows_tower_5shapes + 7)]))
#height_5 = max(as.numeric(tower$y.coords[1:(1740/5 * nrows_tower_5shapes + 7)])) - height_start
#height_1726_2 = max(as.numeric(tower$y.coords[1:((1725 + 1715)/5 * nrows_tower_5shapes + 7)]))
#height_5_2 = max(as.numeric(tower$y.coords[1:((1740 + 1715)/5 * nrows_tower_5shapes + 7)])) - height_1726_2


max_shapes = 1000000000000
from_start = max_shapes - (repeat_start-1)
n_loops = (from_start %/% repeating_length)
n_left = from_start %% repeating_length
height_remainder = max(tower$y.coords[1:((n_left/5)*nrows_tower_5shapes + max_repeat_start)]) - height_start
total_height = height_start + n_loops * height_loop + height_remainder


tower$shape = 0
tower$shape[8:117] = rep(c(rep(1,4), rep(2,5), rep(3,5), rep(4,4), rep(5,4)),5)

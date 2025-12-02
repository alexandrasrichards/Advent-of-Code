def get_input(file):
    with open(file, "r") as f:
        contents = f.read().splitlines()
    return contents

def travgrid(inst, pos):
    dirs = {"U":[0,1],
            "D":[0,-1],
            "L":[-1,0],
            "R":[1,0]}
    for x in inst:
        pos = ingrid([pos[0] + dirs[x][0], pos[1] + dirs[x][1]])
    return(pos)


def ingrid(pos):
    if pos[0] > 1:
        pos[0] = 1
    elif pos[0] < -1:
        pos[0] = -1
    
    if pos[1] > 1:
        pos[1] = 1
    elif pos[1] < -1:
        pos[1] = -1
    
    return pos

def translatepos(pos):
    keypad = [[7,8,9],
            [4,5,6],
            [1,2,3]]
    out = keypad[pos[1] + 1][pos[0] + 1]

    return out


moves = get_input("2016 - lookback/day2input.txt") 
pos = [-2,0]

for steps in moves:
    pos = travgrid(steps, pos)
    print(pos)
    print(translatepos(pos))


def travbiggrid(inst, pos):
    dirs = {"U":[0,1],
            "D":[0,-1],
            "L":[-1,0],
            "R":[1,0]}
    for x in inst:
        pos = inbiggrid(pos, dirs[x])
    return(pos)

def inbiggrid(pos, dirs):
    outofbounds = [[-3,0],[-2,1],[-2,-1],[-1,2],[-1,-2],[0,3],[0,-3],[1,2],[1,-2],[2,1],[2,-1],[3,0]]
    newpos = [pos[0] + dirs[0], pos[1] + dirs[1]]
    if any(x == newpos for x in outofbounds):
        return pos
    else:
        return newpos

def translatebigpos(pos):
    keypad = [[0,0,'D',0,0],
            [0,'A','B','C',0],
            [5,6,7,8,9],
            [0,2,3,4,0],
            [0,0,1,0,0]]
    out = keypad[pos[1] + 2][pos[0] + 2]

    return out


moves = get_input("2016 - lookback/day2input.txt") 
pos = [0,0]

for steps in moves:
    pos = travbiggrid(steps, pos)
    print(translatebigpos(pos))
def get_input(file):
    with open(file, "r") as f:
        contents = f.read()
    contents = contents.split(", ")
    return contents

def head_tail(s):
    head = s[0]
    tail = s[1:]
    return head, tail

def trav_grid(dir, pos):
    compass = [(0,1),(1,0),(0,-1),(-1,0)]
    orient = pos[0]
    if dir[0] == "R":
        orient += 1 
    else:
        orient += -1

    orient = orient % 4
    dist_x = int(dir[1]) * compass[orient][0]
    dist_y = int(dir[1]) * compass[orient][1]

    return orient, pos[1] + dist_x, pos[2] + dist_y

def track_grid(dir, orient, pos):
    pos = list(pos)
    compass = [(0,1),(1,0),(0,-1),(-1,0)]
    if dir[0] == "R":
        orient += 1 
    else:
        orient += -1

    orient = orient % 4
    pos_list_int = []
    for i in range(1,int(dir[1])+1):
        pos[0] += compass[orient][0]
        pos[1] += compass[orient][1]
        pos_list_int.append((pos[0],pos[1]))

    return orient, pos_list_int, (pos[0],pos[1])


input = [head_tail(s) for s in get_input("day1input.txt")]

pos = [0,0,0]

for dirs in input:
    pos = trav_grid(dirs,pos)

print(pos)
print(abs(pos[1]) + abs(pos[2]))

pos_list = [(0,0)]

pos = [0,0]
orient = 0

for dirs in input:
    orient,pos_short_list,pos = track_grid(dirs,orient,pos)

    for steps in pos_short_list:
        if(steps in pos_list):
            print(steps)
            break
    else:
        for x in pos_short_list:
            pos_list.append(x)


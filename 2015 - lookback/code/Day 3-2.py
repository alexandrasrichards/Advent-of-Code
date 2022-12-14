data = open("/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2015 - lookback/data/data 3.csv", "r").read()

santa = (0,0)
robo = (0,0)
houses = [start]
count = 0
unique = 1
move = (0,0)
who = "s"
for let in data:
    print(let)
    if let == '^':
        move = (0,1)
    elif let == '>':
        move = (1,0)
    elif let == 'v':
        move = (0,-1)
    elif let == '<':
        move = (-1,0)
    if who == "s":
        who = "r"
        new = tuple(map(lambda i, j: i + j, santa, move))
        santa = new
    elif who == "r":
        who = "s"
        new = tuple(map(lambda i, j: i + j, robo, move))
        robo = new
    if new in houses:
        count += 1
    else:
        count += 1
        unique += 1
        
    houses.append(new)


print(unique)

def upstairs(data):
    b = 1
    f = 0
    while f >= 0:
        if data[b-1] == "(":
            f += 1
            b += 1
        elif data[b-1] == ")":
            f -= 1
            b += 1
    return b
    
    

data = open("/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2015 - lookback/data/data 1.csv", "r").read()
print(len(data))
print(upstairs(data))

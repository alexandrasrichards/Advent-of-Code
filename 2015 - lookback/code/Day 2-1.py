data = open("/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2015 - lookback/data/data 2.csv", "r").read()
data = data.splitlines()
total = 0
for rows in data:
    l = int(rows.split("x")[0])
    w = int(rows.split("x")[1])
    h = int(rows.split("x")[2])
    
    lw = l*w
    lh = l*h
    wh = w*h
    
    total += 2*lw + 2*lh + 2*wh + min(lw,lh,wh)
    


print(total)

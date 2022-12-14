data = open("/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2015 - lookback/data/data 1.csv", "r").read()
up = data.count("(")
down = data.count(")")
print(up - down)

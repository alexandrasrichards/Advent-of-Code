def get_input(filename):
    with open(filename, "r") as f:
        contents = f.readlines()
        contents = [line.strip() for line in contents]
    return contents

def get_indices(data, target):
    indices = []
    for num in range(0,len(data)):
        if data[num] == target:
            indices.append(num)
    return indices



def check_neighbours(grid):
    count = 0
    gaps = [get_indices(s,"@") for s in grid]
    newgrid = grid.copy()
    for row in range(0,len(grid)):
        for col in gaps[row]:
            rolls = ["."]
            if row > 0:
                if col > 0:
                    rolls.append(grid[row - 1][col - 1])
                rolls.append(grid[row - 1][col])
                if col < len(grid[row])-1:
                    rolls.append(grid[row - 1][col + 1])
            if col > 0:
                rolls.append(grid[row][col - 1])
            if col < len(grid[row])-1:
                rolls.append(grid[row][col + 1])
            if row < len(grid)-1:
                if col > 0:
                    rolls.append(grid[row + 1][col - 1])
                rolls.append(grid[row + 1][col])
                if col < len(grid[row])-1:
                    rolls.append(grid[row + 1][col + 1])

            if len(get_indices("".join(rolls),"@")) < 4:
                count += 1
                newgrid[row] = newgrid[row][:col] + "x" + newgrid[row][col+1:]
            
    return count, newgrid



grid = get_input("2025/day4input.txt")
print(check_neighbours(grid)[0])


totcount = 0
currcount = 1
newgrid = grid.copy()
while currcount > 0:
    currcount, newgrid = check_neighbours(newgrid)
    totcount += currcount

print(totcount)
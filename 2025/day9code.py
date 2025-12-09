def get_input(filename):
    with open(filename, "r") as f:
        contents = f.read().splitlines()
    
    contents = [row.split(",") for row in contents]
    return contents

def find_rectangles(tiles):
    for i in range(len(tiles)):
        area = []
        for j in range(len(tiles)):
            x = abs(int(tiles[i][0]) - int(tiles[j][0])) + 1
            y = abs(int(tiles[i][1]) - int(tiles[j][1])) + 1
            area.append(x * y)
        if i == 0:
            area_grid = [area]
        else:
            area_grid.append(area)
    return area_grid        


grid = get_input("2025/day9input.txt")
areas = find_rectangles(grid)
print(max(max(areas)))
def get_input(filename):
    with open(filename, "r") as f:
        contents = f.read().splitlines()
        contents = [[x for x in rows] for rows in contents]
    return contents

def manifold_process(data):
    lightbeamcols = set([data[0].index("S")])
    count = 0
    row = 1
    while row < len(data):
        newlightbeams = set()
        for light in lightbeamcols:
            if data[row][light] != ".":
                if light > 0:
                    newlightbeams.add(light - 1)
                if light < len(data) - 1:
                    newlightbeams.add(light + 1)
                count += 1
            else:
                newlightbeams.add(light)
        lightbeamcols = newlightbeams.copy()
        row = row + 1
    
    return count

def quantum_process(data):
    lightbeamcols = set([data[0].index("S")])
    data[0][data[0].index("S")] = 1
    row = 1
    while row < len(data):
        newlightbeams = set()
        for light in lightbeamcols:
            if data[row][light] == "^":
                if light > 0:
                    if data[row][light - 1] == ".":
                        data[row][light - 1] = int(data[row - 1][light])
                    else:
                        data[row][light - 1] = int(data[row][light - 1]) + int(data[row - 1][light])
                    newlightbeams.add(light - 1)
                if light < len(data) - 1:
                    if data[row][light + 1] == ".":
                        data[row][light + 1] = int(data[row - 1][light])
                    else:
                        data[row][light + 1] = int(data[row][light + 1]) + int(data[row - 1][light])
                    newlightbeams.add(light + 1)
            else:
                if data[row][light] == ".":
                    data[row][light] = int(data[row - 1][light])
                else: 
                    data[row][light] = int(data[row][light]) + int(data[row - 1][light])
                newlightbeams.add(light)
        lightbeamcols = newlightbeams.copy()
        row = row + 1
    
    last = [x for x in data[-1] if x != "."]

    return sum(last)


data = get_input("2025/day7input.txt")
print(manifold_process(data))
print(quantum_process(data))
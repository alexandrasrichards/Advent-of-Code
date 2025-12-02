def readfile(filename):
    with open(filename, "r") as f:
        contents = f.read()
    contents = contents.split(",")
    return contents

def findranges(input):
    idrange = input.split("-")
    return (idrange)

def checkinvalid(numbers):
    if len(numbers) % 2 == 0:
        half = len(numbers) // 2
        if numbers[0:half] == numbers[half:]:
            return(int(numbers))
        else:
            return(0)
    else:
        return(0)

def runranges(inp):
    invalid = 0
    start = int(inp[0])
    end = int(inp[1])
    for i in range(start,end):
        invalid += checkinvalid(str(i))
    return invalid

ranges = readfile("2025/day2input.txt")
ranges = [findranges(s) for s in ranges]
invalid = [runranges(id) for id in ranges]
print(sum(invalid))

def checkinvalid2(numbers):
    maxsplit = len(numbers) + 1
    minsplit = 2
    for i in range(minsplit, maxsplit):
        if len(numbers) % i == 0:
            size = len(numbers)//i
            parts = [numbers[x:x+size] for x in range(0, len(numbers),size)]
            if all(x == parts[0] for x in parts):
                return(int(numbers))
    return(0)

def runranges2(inp):
    invalid = 0
    start = int(inp[0])
    end = int(inp[1])
    for i in range(start,end):
        invalid += checkinvalid2(str(i))
    return invalid


ranges2 = readfile("2025/day2input.txt")
ranges2 = [findranges(s) for s in ranges2]
invalid2 = [runranges2(id) for id in ranges2]
print(sum(invalid2))
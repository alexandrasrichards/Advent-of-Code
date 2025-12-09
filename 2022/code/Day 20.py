fname = "/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2022/data/data 20.txt"

def get_stream():
    with open(fname) as f:
        stream = f.read()
    stream = stream.splitlines()
    return [int(x) for x in stream]


stream = get_stream()
p = 811589153
stream = [p * x for x in stream]
positions = list(range(len(stream)))
l = len(stream)

for k in range(10):
    print(k)
    for i in range(len(stream)):
        
        pos = positions.index(i)
        positions.pop(pos)

        val = stream.pop(pos)

        j = (pos + val) % (len(stream))
        positions.insert(j, i)
        
        if not(pos in positions):
            break
        stream.insert(j, val)
    print(37 in positions)

z = stream.index(0)
l = len(stream)
a = stream[(z + 1000) % l]
b = stream[(z + 2000) % l]
c = stream[(z + 3000) % l]

print(a + b + c)



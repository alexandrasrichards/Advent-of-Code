data = open("day1input.txt")

data = data.readlines()

def splitstring(s):
    head = s[0]
    tail = int(s[1:])
    return head, tail

data = [splitstring(s) for s in data]

def newpos(inst, oldpos):
    if inst[0] == 'R':
        new = (oldpos + inst[1]) % 100
    else:
        new = (oldpos - inst[1]) % 100
    return new

pos = 50
zeros = 0

for inst in data:
    pos = newpos(inst,pos)
    if pos == 0:
        zeros += 1


print(zeros)


def newposclicks(inst, oldpos):
    if inst[0] == 'R':
        new = (oldpos + inst[1]) % 100
        zeros = (oldpos + inst[1]) // 100
        if new == 0:
            zeros = zeros - 1
    else:
        new = (oldpos - inst[1]) % 100
        zeros = abs((oldpos - inst[1]) // 100)
        if oldpos == 0:
            zeros = zeros - 1

    print(inst, new, zeros)
    return new, zeros

pos = 50
zeros = 0
for inst in data:
    pos, click = newposclicks(inst, pos)
    zeros += click
    if pos == 0:
        zeros += 1

print(zeros)


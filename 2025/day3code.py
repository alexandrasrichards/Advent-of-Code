def get_input(filename):
    with open(filename, "r") as f:
        contents = f.read().splitlines()
    return contents

def findjoltage(bank):
    first = [0]
    for n in range(0, len(bank)-1):
        if all(x != int(bank[n]) for x in first):
            if all(x < int(bank[n]) for x in first):
                pairs = findpairs(bank[n:])
                first.append(int(bank[n]))
    return(pairs)


def findpairs(bank):
    high = 0
    for num in bank[1:]:
        if int(num) > high:
            high = int(num)
    out = [bank[0],str(high)]
    return int("".join(out))

def findlong(bank):
    bank = [int(s) for s in bank]
    testnum = []
    count = 0
    while len(testnum) < 12:
        lenleft = 11 - len(testnum)
        if lenleft > 0:
            next = max(bank[count:-lenleft])
        else:
            next = max(bank[count:])
        count += bank[count:].index(next) + 1
        testnum.append(next)
    return int("".join(map(str,testnum)))
   



nums = get_input("2025/day3input.txt")
jolt = [findjoltage(bank) for bank in nums]
print(sum(jolt))

joltlong = [findlong(bank) for bank in nums]
print(sum(joltlong))

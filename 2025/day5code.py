def get_input(filename):
    with open(filename, "r") as f:
        contents = f.read().splitlines()
    return(contents)


def parse_list(data):
    sep = data.index("")

    freshlistmin = [elem.split("-")[0] for elem in data[:sep]]
    freshlistmax = [elem.split("-")[1] for elem in data[:sep]]

    testlist = data[sep+1:]

    fresh = 0

    for foods in testlist:
        for ind in range(0,len(freshlistmin)):
            if int(foods) >= int(freshlistmin[ind]) and int(foods) <= int(freshlistmax[ind]):
                fresh += 1
                break

    return fresh


    
def find_freshlist(data):
    sep = data.index("")

    freshlist = sorted([list(map(int,elem.split("-"))) for elem in data[:sep]])

    rangecount = 0

    newlist = [freshlist[0]]
    
    for ind in range(1,len(freshlist)):
        if freshlist[ind][0] <= newlist[rangecount][1] + 1 and freshlist[ind][1] > newlist[rangecount][1]:
            newlist[rangecount][1] = freshlist[ind][1]
        elif freshlist[ind][0] > newlist[rangecount][1] + 1:
            rangecount += 1
            newlist.append(freshlist[ind])

    goodIDs = [len(range(x[0],x[1]+1)) for x in newlist]

    return sum(goodIDs)


foodlist = get_input("2025/day5input.txt")
print(parse_list(foodlist))
print(find_freshlist(foodlist))
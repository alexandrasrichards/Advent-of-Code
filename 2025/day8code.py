import math


def get_input(filename):
    with open(filename, "r") as f:
        contents = f.read().splitlines()
    
    contents = [row.split(",") for row in contents]
    return contents

def find_distance(grid):
    for i in range(len(grid)):
        dist = []
        for j in range(len(grid)):
            x = int(grid[i][0]) - int(grid[j][0])
            y = int(grid[i][1]) - int(grid[j][1])
            z = int(grid[i][2]) - int(grid[j][2])
            dist.append(math.sqrt(x**2 + y**2 + z**2))
        if i == 0:
            dist_grid = [dist]
        else:
            dist_grid.append(dist)
    return dist_grid

def find_chains(dist, distlist):
    chains = list(range(len(dist)))
    chains = [[x] for x in chains]
    for chain in distlist:
        terms = [(index, row.index(chain)) for index, row in enumerate(dist) if chain in row]
        terms = terms[0]
        term1 = []
        term2 = []
        for elem in chains:
            if len(elem) == 1:
                if terms[0] == elem[0]:
                    term1 = chains.index(elem)
                    break
            elif terms[0] in elem:
                term1 = chains.index(elem)
                break        
        for elem in chains:

            if len(elem) == 1:
                if terms[1] == elem[0]:
                    term2 = chains.index(elem)
                    break
            elif terms[1] in elem:
                term2 = chains.index(elem)
                break
        if term1 == [] and term2 == []:
            chains.append([list(terms)])
        elif term1 == []:
            chains[term2].append(terms[0])
        elif term2 == []:
            chains[term1].append(terms[1])
        else:
            chain1 = term1
            chain2 = term2

            tot_chains = set(range(len(chains)))
            tot_chains = list(tot_chains - set([chain1]) - set([chain2]))
            
            chain_join = chains[chain1] + chains[chain2]
            chain_join = list(set(chain_join))
            new_chains = [chains[x] for x in tot_chains]
            chains = new_chains[:]
            chains.append(chain_join)

    chains_lens = [len(x) for x in chains]

    chains_lens = sorted(chains_lens, reverse = True)

    out = chains_lens[0] * chains_lens[1] * chains_lens[2]
    return out


def find_loop(dist, distlist, grid):
    chains = list(range(len(dist)))
    chains = [[x] for x in chains]
    tick = -1
    while len(chains) > 1:
        tick += 1
        chain = distlist[tick]
        terms = [(index, row.index(chain)) for index, row in enumerate(dist) if chain in row]
        terms = terms[0]
        term1 = []
        term2 = []
        for elem in chains:
            if len(elem) == 1:
                if terms[0] == elem[0]:
                    term1 = chains.index(elem)
                    break
            elif terms[0] in elem:
                term1 = chains.index(elem)
                break        
        for elem in chains:

            if len(elem) == 1:
                if terms[1] == elem[0]:
                    term2 = chains.index(elem)
                    break
            elif terms[1] in elem:
                term2 = chains.index(elem)
                break
        if term1 == [] and term2 == []:
            chains.append([list(terms)])
        elif term1 == []:
            chains[term2].append(terms[0])
        elif term2 == []:
            chains[term1].append(terms[1])
        else:
            chain1 = term1
            chain2 = term2

            tot_chains = set(range(len(chains)))
            tot_chains = list(tot_chains - set([chain1]) - set([chain2]))
            
            chain_join = chains[chain1] + chains[chain2]
            chain_join = list(set(chain_join))
            new_chains = [chains[x] for x in tot_chains]
            chains = new_chains[:]
            chains.append(chain_join)

    terms = [(index, row.index(chain)) for index, row in enumerate(dist) if chain in row]
    out = int(grid[terms[0][0]][0]) * int(grid[terms[0][1]][0])
    
    return out

grid = get_input("2025/day8input.txt")
dist = find_distance(grid)
distlist = sorted(list(set([j for sub in dist for j in sub])))
print(find_chains(dist, distlist[1:1001]))
print(find_loop(dist, distlist[1:], grid))


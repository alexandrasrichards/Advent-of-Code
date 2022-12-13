import json
import functools

def sign(a):
    if a < 0:
        return -1
    elif a == 0:
        return 0
    elif a > 0:
        return 1


def comp(p):
    l,r = p
    i = 0
    
    ll = len(l)
    lr = len(r)
    
    if ll == 0 or lr == 0:
        return sign(lr-ll) >= 0
    
    # Init: i = 0, l[0..0) == [] == r[0..)
    while i < min(ll,lr) and compare(l[i],r[i]) == 0: # I: l[0..i) == r[0..i)
        i += 1
    
    # Post: l[0..i) == r[0..i) 
    # AND i == min(ll,lr) or compare(l[i],r[i]) != 0
    
    
    if i == min(ll,lr):
        return sign(lr-ll) >= 0
    elif compare(l[i],r[i]) in [1, -1]: # i < min(ll, lr)
        return compare(l[i],r[i]) == 1
    else:
        error
    

def compare(l,r):
    if l == r:
        return 0
    if isinstance(l, int) and isinstance(r, int):
        if l > r:
            return -1
        else:
            return 1
    elif isinstance(l, list) and isinstance(r, int):
        return compare(l,[r])
    elif isinstance(l, int) and isinstance(r, list):
        return compare([l],r)
    elif isinstance(l, list) and isinstance(r, list):
        if len(l) == 0 and len(r) > 0:
            return 1
        elif len(l) > 0 and len(r) == 0:
            return -1
        elif len(l) == 0 and len(r) == 0:
            return 0
        
        l_head = l[0]
        r_head = r[0]
        out = compare(l_head, r_head)
        if out == 0:
            return compare(l[1:],r[1:])
        else:
            return out


fname = "/Users/lsh1700322/Documents/GitHub/Advent-of-Code/2022/data/data 13.json"
f = open(fname)
data = json.load(f)
pairs = data
listed = []


for pair in pairs:
    listed = listed + pair 
   
listed = listed + [[[2]]] + [[[6]]]

listed.sort(key = functools.cmp_to_key(compare), reverse = True)
a = (listed.index([[2]]) + 1) * (listed.index([[6]]) + 1)

        
print(a)        

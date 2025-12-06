def get_input(filename):
    with open(filename, "r") as f:
        contents = f.readlines()
        contents = [line.strip() for line in contents]
        contents = [line.split(" ") for line in contents]
        contents = [[x for x in line if x != ''] for line in contents]
    return contents

def calc_sums(sums):
    maths = sums[len(sums)-1]
    tot = 0
    for ind in range(0, len(sums[0])):
        if maths[ind] == "*":
            temp = 1
            for nums in range(0, len(sums) - 1):
                temp *= int(sums[nums][ind])
        else:
            temp = 0
            for nums in range(0, len(sums) - 1):
                temp += int(sums[nums][ind])
        tot += temp
    return tot 

def get_input2(filename):
    with open(filename, "r") as f:
        contents = f.readlines()
        contents = [line[:-1] for line in contents]
    return contents

def get_indices(data, target):
    indices = []
    for num in range(0,len(data)):
        if data[num] == target:
            indices.append(num)
    return indices


def find_nums(sums):
    sums = [item[::-1] for item in sums]
    sums = [[x for x in rows] for rows in sums]
    maths = [x for x in sums[len(sums)-1] if x != " "]
    
    blanks = [get_indices(x, " ") for x in sums]
    num_gaps = blanks[0]
    for blank in blanks:
        num_gaps = sorted(list(set(num_gaps) & set(blank)))
    num_starts = [x+ 1 for x in num_gaps]
    num_starts = [0] + num_starts
    
    nums = sum_over_rows(sums[:-1])
    print(nums)
    tot = 0
    tick = 0
    tock = 0
    while tock < len(nums):
        if nums[tock] == 0:
            tick += 1
            tock += 1
        else:
            calc = maths[tick]
            if calc == "*":
                temp = 1
            elif calc == "+":
                temp = 0
            while tock < len(nums) and nums[tock] != 0:
                if calc == "*":
                    temp *= nums[tock]
                elif calc == "+":
                    temp += nums[tock]
                tock += 1

            tot += temp
                
    return tot
                  

def sum_over_rows(data):
    nums = [[data[j][i] for j in range(0,len(data))] for i in range(0,len(data[0]))]
    
    for ind in range(0,len(nums)):
        if all(x == " " for x in nums[ind]):
            nums[ind] = 0
        else:
            nums[ind] = int("".join(map(str,nums[ind])))

    return nums


def calc_sums2(sums):
    maths = sums[len(sums)-1]
    tot = 0
    for ind in range(0, len(sums[0])):
        if maths[ind] == "*":
            temp = 1
            for nums in range(0, len(sums) - 1):
                temp *= int(sums[nums][ind])
        else:
            temp = 0
            for nums in range(0, len(sums) - 1):
                temp += int(sums[nums][ind])
        tot += temp
    return tot 


sums = get_input("2025/day6input.txt")
print(calc_sums(sums))

sums2 = get_input2("2025/day6input.txt")
print(find_nums(sums2))
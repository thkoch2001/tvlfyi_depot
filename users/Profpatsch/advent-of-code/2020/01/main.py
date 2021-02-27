import sys

l = []
with open('./input', 'r') as f:
    for line in f:
        l.append(int(line))

s = set(l)

res=None
for el in s:
    for el2 in s:
        if (2020-(el+el2)) in s:
            res=(el, el2, 2020-(el+el2))
            break

if res == None:
    sys.exit("could not find a number that adds to 2020")

print(res)

print(res[0] * res[1] * res[2])

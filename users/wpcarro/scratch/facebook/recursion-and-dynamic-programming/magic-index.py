from math import floor

def find_magic_index_brute(xs):
    for i in range(len(xs)):
        if xs[i] == i:
            return i
    return -1

def mid(lo, hi):
    return lo + floor((hi - lo) / 2)

def find_magic_index(xs):
    lo, hi = 0, len(xs) - 1
    return do_find_magic_index(xs, 0, len(xs) - 1)

def do_find_magic_index(xs, lo, hi):
    pass

xss = [
    [],
    [-1,0,2,4,5,6],
    [1,1,1,1,1,5],
    [-2,-2,-2,-2,4],
    [1,2,3,4,5],
]

for xs in xss:
    print(xs)
    a = find_magic_index_brute(xs)
    b = find_magic_index(xs)
    print(a, b)
    assert a == b
    print("Success!")

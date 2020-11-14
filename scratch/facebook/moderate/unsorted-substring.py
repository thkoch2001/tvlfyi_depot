# Write a function that accepts an array of integers and returns the indices for
# the starting and ending integers that, if their elements were sorted, the
# entire array would be sorted.

################################################################################
# First Attempt
################################################################################

def unsorted_substring(xs):
    ys = xs[:]; ys.sort()
    m = 0
    while xs[m] == ys[m]:
        m += 1
        if m >= len(xs):
            return -1, -1
    n = len(xs) - 1
    while xs[n] == ys[n]:
        n -= 1
    return m, n

################################################################################
# Second Attempt
################################################################################

def unsorted_substring_2(xs):
    beg = 1
    while xs[beg - 1] <= xs[beg]:
        beg += 1
        if beg >= len(xs):
            return -1, -1
    end = len(xs) - 2
    while xs[end + 1] >= xs[end]:
        end -= 1

    min_mid = xs[beg]
    max_mid = xs[beg]
    i = beg + 1
    while i <= end:
        min_mid = min(min_mid, xs[i])
        max_mid = max(max_mid, xs[i])
        i += 1

    # beg -= 1 until max(lhs) <= min(mid)
    while beg - 1 >= 0 and xs[beg - 1] >= min_mid:
        beg -= 1

    # end += 1 while max(mid) <= min(rhs)
    while end + 1 < len(xs) and max_mid >= xs[end + 1]:
        end += 1
    return beg, end

################################################################################
# Tests
################################################################################

xs = [
    [1,2,4,7,10,11,7,12,6,7,16,18,19],
    [1,2,3,4],
    [4,3,2,1],
    [1,3,2,4],
    [2,1,3,4],
]

for x in xs:
    print("Testing: {}".format(x))
    print("1) {}".format(unsorted_substring(x)))
    print("2) {}".format(unsorted_substring_2(x)))

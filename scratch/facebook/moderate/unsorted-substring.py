# Write a function that accepts an array of integers and returns the indices for
# the starting and ending integers that, if their elements were sorted, the
# entire array would be sorted.

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

print(unsorted_substring([1,2,4,7,10,11,7,12,6,7,16,18,19]))
print(unsorted_substring([1,2,3,4]))
print(unsorted_substring([4,3,2,1]))
print(unsorted_substring([1,3,2,4]))
print(unsorted_substring([2,1,3,4]))

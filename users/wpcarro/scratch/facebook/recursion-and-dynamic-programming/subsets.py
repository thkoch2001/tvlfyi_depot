# take-aways:
#   - Use integers as lists of boolean values
#   - Use 1 << n to compute 2^n where n = len(xs)

def set_from_int(xs, n):
    result = []
    for i in range(len(xs)):
        if n & (1 << i) != 0:
            result.append(xs[i])
    return result

# subsets :: Set a -> List (Set a)
def subsets(xs):
    n = len(xs)
    return [set_from_int(xs, i) for i in range(1 << n)]

#   0 1 2
# 0 N Y Y
# 1 _ N Y
# 2 _ _ N

# For my interview, be able to compute *permutations* and *combinations*

# This differs from permutations because this is about finding combinations...
#
# bottom-up
# 0 =>        { }
# 1 =>  {3}   {4}   {3}
# 2 => {5,4} {5,3} {4,3}

xs = [
    ([], [[]]),
    ([5], [[], [5]]),
    ([5,4], [[],[5],[4],[5,4]]),
]

for x, expected in xs:
    result = subsets(x)
    print("subsets({}) => {} == {}".format(x, result, expected))
    assert result == expected
    print("Success!")

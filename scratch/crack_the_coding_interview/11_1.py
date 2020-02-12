# Implementation for a problem from "Crack the Coding Interview".
#
# Dependencies:
# - python 2.7.16
# - entr 4.1
#
# To run the tests, run: `python 11_1.py`
# For a tight development loop, run: `echo 11_1.py | entr python /_`
#
# Author: William Carroll <wpcarro@gmail.com>

################################################################################
# Implementation
################################################################################
def insert_sorted(xs, ys):
    """
    Merges `ys` into `xs` and ensures that the result is sorted.

    Assumptions:
    - `xs` and `ys` are both sorted.
    - `xs` has enough unused space to accommodate each element in `ys`.
    """
    for y in ys:
        xi = xs.index(None) - 1
        yi = xs.index(None)
        xs[yi] = y
        while xi != -1 and y < xs[xi]:
            xs[xi], xs[yi] = xs[yi], xs[xi]
            xi, yi = xi - 1, yi - 1
    return xs

################################################################################
# Tests
################################################################################
assert insert_sorted([1, 3, 5, None, None], [2, 4]) == [1, 2, 3, 4, 5]
assert insert_sorted([None, None], [2, 4]) == [2, 4]
assert insert_sorted([None, None], [2, 4]) == [2, 4]
assert insert_sorted([1, 1, None, None], [0, 0]) == [0, 0, 1, 1]
assert insert_sorted([1, 1, None, None], [1, 1]) == [1, 1, 1, 1]
print('All tests pass!')

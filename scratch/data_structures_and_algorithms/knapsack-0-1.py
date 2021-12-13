import unittest
from math import floor


def knapify(xs, capacity=None):
    assert capacity is not None
    n = len(xs)
    # For 0/1 Knapsack, we must use a table, since this will encode which values
    # work for which items. This is cleaner than including a separate data
    # structure to capture it.
    maxes = [[0 for x in range(capacity + 1)] for x in range(n + 1)]

    # Build table maxes[][] in bottom up manner
    for row in range(n + 1):
        for col in range(capacity + 1):
            if row == 0 or col == 0:
                maxes[row][col] = 0
            elif xs[row - 1][0] <= col:
                maxes[row][col] = max(
                    xs[row - 1][1] + maxes[row - 1][col - xs[row - 1][0]],
                    maxes[row - 1][col])
            else:
                maxes[row][col] = maxes[row - 1][col]

    return maxes[-1][capacity]


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_one_cake(self):
        actual = knapify([(3, 10), (2, 15), (7, 2), (12, 20)], capacity=12)
        expected = None
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

from math import floor
import unittest


################################################################################
# Solution
################################################################################
def bounds(r):
    ct = len(r)
    if ct % 2 == 0:
        h = int(ct / 2)
        return ct, h
    else:
        h = floor(ct / 2)
        return ct, h


def find_repeat(xs):
    ct, h = bounds(xs)
    rl = range(1, h + 1)
    rr = range(h + 1, ct)
    while True:
        nl = len([None for x in xs if x in rl])
        nr = len([None for x in xs if x in rr])
        branch = rl if nl > nr else rr
        if len(branch) == 1:
            return branch[0]
        ct, h = bounds(branch)
        rl = range(branch[0], branch[0])
        rr = range(branch[0] + h, branch[-1] + 1)
    raise Exception(
        'We could not find any duplicates in xs. Perhaps xs did not adhere to the usage contract.'
    )


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_just_the_repeated_number(self):
        actual = find_repeat([1, 1])
        expected = 1
        self.assertEqual(actual, expected)

    def test_short_list(self):
        actual = find_repeat([1, 2, 3, 2])
        expected = 2
        self.assertEqual(actual, expected)

    def test_medium_list(self):
        actual = find_repeat([1, 2, 5, 5, 5, 5])
        expected = 5
        self.assertEqual(actual, expected)

    def test_long_list(self):
        actual = find_repeat([4, 1, 4, 8, 3, 2, 7, 6, 5])
        expected = 4
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

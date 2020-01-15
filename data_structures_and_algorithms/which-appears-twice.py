import unittest


################################################################################
# Solution
################################################################################
# find_repeat :: [Int] -> Int
def find_repeat(xs):
    n = len(xs) - 1
    return sum(xs) - ((n**2 + n) / 2)


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_short_list(self):
        actual = find_repeat([1, 2, 1])
        expected = 1
        self.assertEqual(actual, expected)

    def test_medium_list(self):
        actual = find_repeat([4, 1, 3, 4, 2])
        expected = 4
        self.assertEqual(actual, expected)

    def test_long_list(self):
        actual = find_repeat([1, 5, 9, 7, 2, 6, 3, 8, 2, 4])
        expected = 2
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

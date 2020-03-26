import unittest


################################################################################
# InterviewCake's solution
################################################################################
def cycle_len(xs, i):
    """
    Returns the length of a cycle that contains no duplicate items.
    """
    result = 1
    checkpt = i
    current = xs[checkpt - 1]

    while current != checkpt:
        current = xs[current - 1]
        result += 1

    return result


def theirs(xs):
    """
    This is InterviewCake's solution.
    """
    i = xs[-1]
    for _ in range(len(xs) - 1):
        i = xs[i - 1]

    cycle_length = cycle_len(xs, i)

    p0 = xs[-1]
    p1 = xs[-1]
    for _ in range(cycle_length):
        p1 = xs[p1 - 1]

    while p0 != p1:
        p0 = xs[p0 - 1]
        p1 = xs[p1 - 1]

    print(p0, p1)

    return p0


################################################################################
# My solution
################################################################################
def mine(xs):
    """
    This is the solution that I came up with, which differs from InterviewCake's
    solution.
    """
    i = xs[-1]
    offset = 1 if len(xs) % 2 == 0 else 2

    for _ in range(len(xs) - offset):
        i = xs[i - 1]

    return i


use_mine = True
find_duplicate = mine if use_mine else theirs


# Tests
class Test(unittest.TestCase):
    def test_just_the_repeated_number(self):
        # len(xs) even
        actual = find_duplicate([1, 1])
        expected = 1
        self.assertEqual(actual, expected)

    def test_short_list(self):
        # len(xs) even
        actual = find_duplicate([1, 2, 3, 2])
        expected = 2
        self.assertEqual(actual, expected)

    def test_medium_list(self):
        # len(xs) even
        actual = find_duplicate([1, 2, 5, 5, 5, 5])
        expected = 5
        self.assertEqual(actual, expected)

    def test_long_list(self):
        # len(xs) odd
        actual = find_duplicate([4, 1, 4, 8, 3, 2, 7, 6, 5])
        expected = 4
        self.assertEqual(actual, expected)

    ############################################################################
    # Additional examples from InterviewCake.com
    ############################################################################
    def test_example_a(self):
        # len(xs) even
        actual = find_duplicate([3, 4, 2, 3, 1, 5])
        expected = 3
        self.assertTrue(actual, expected)

    def test_example_b(self):
        # len(xs) even
        actual = find_duplicate([3, 1, 2, 2])
        expected = 2
        self.assertEqual(actual, expected)

    def test_example_c(self):
        # len(xs) odd BUT multiple duplicates
        actual = find_duplicate([4, 3, 1, 1, 4])
        self.assertTrue(actual in {1, 4})


unittest.main(verbosity=2)

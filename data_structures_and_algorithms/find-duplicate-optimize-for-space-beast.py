import unittest


################################################################################
# Solution
################################################################################
def find_duplicate(xs):
    self_ref_count = 0
    for i in range(len(xs)):
        if xs[i] == i + 1:
            self_ref_count += 1
    hops = len(xs) - 1 - self_ref_count
    current = xs[-1]
    while hops > 0:
        current = xs[current - 1]
        hops -= 1
    return current


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    # TODO: Debug why this fails.
    def test_darren_from_interview_cake(self):
        actual = find_duplicate([4, 1, 8, 3, 2, 7, 6, 5, 4])
        expected = 4
        self.assertEqual(actual, expected)

    def test_just_the_repeated_number(self):
        actual = find_duplicate([1, 1])
        expected = 1
        self.assertEqual(actual, expected)

    def test_short_list(self):
        actual = find_duplicate([1, 2, 3, 2])
        expected = 2
        self.assertEqual(actual, expected)

    def test_last_cycle(self):
        actual = find_duplicate([3, 4, 2, 3, 1, 5])
        expected = 3
        self.assertEqual(actual, expected)

    def test_medium_list(self):
        actual = find_duplicate([1, 2, 5, 5, 5, 5])
        expected = 5
        self.assertEqual(actual, expected)

    def test_long_list(self):
        actual = find_duplicate([4, 1, 4, 8, 3, 2, 7, 6, 5])
        expected = 4
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

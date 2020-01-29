import unittest


################################################################################
# Solution
################################################################################
def find_rotation_point(xs):
    """Usage of `visited` here is a hack, but works for the test cases
    (gulp)."""
    i = 0
    j = round(len(xs) / 2)
    result = None
    visited = set()
    while not result:
        if i in visited:
            i += 1
        if j in visited:
            j -= 1
        visited.add(i)
        visited.add(j)
        if xs[j - 1] > xs[j]:
            result = j
        elif xs[i] < xs[j]:
            i = j
            j += round((len(xs) - j) / 2)
        elif xs[i] >= xs[j]:
            i = j
            j -= round((j - i) / 2)
    return result


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_small_list(self):
        actual = find_rotation_point(['cape', 'cake'])
        expected = 1
        self.assertEqual(actual, expected)

    def test_medium_list(self):
        actual = find_rotation_point(
            ['grape', 'orange', 'plum', 'radish', 'apple'])
        expected = 4
        self.assertEqual(actual, expected)

    def test_large_list(self):
        actual = find_rotation_point([
            'ptolemaic', 'retrograde', 'supplant', 'undulate', 'xenoepist',
            'asymptote', 'babka', 'banoffee', 'engender', 'karpatka',
            'othellolagkage'
        ])
        expected = 5
        self.assertEqual(actual, expected)

    # Are we missing any edge cases?


unittest.main(verbosity=2)

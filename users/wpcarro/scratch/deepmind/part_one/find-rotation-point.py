import unittest
from math import floor


def midpoint(a, b):
    return a + floor((b - a) / 2)


def do_find_rotation_point(a, b, xs):
    i = midpoint(a, b)
    count = b - a + 1

    if count == 2:
        if xs[a] > xs[b]:
            return b
        else:
            return -1

    if i in {a, b}:
        return i

    if xs[a] < xs[i]:
        return do_find_rotation_point(i, b, xs)
    else:
        return do_find_rotation_point(a, i, xs)


def find_rotation_point(xs):
    return do_find_rotation_point(0, len(xs) - 1, xs)


# Tests
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


unittest.main(verbosity=2)

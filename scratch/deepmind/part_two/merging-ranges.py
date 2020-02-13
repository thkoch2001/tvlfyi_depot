import unittest
import timeit


# Solution that uses O(n) space to store the result.
def not_in_place(xs):
    xs.sort()
    result = [xs[0]]
    for ca, cb in xs[1:]:
        pa, pb = result[-1]
        if ca <= pb:
            result[-1] = (pa, max(pb, cb))
        else:
            result.append((ca, cb))
    return result


# Solution that uses O(1) space to store the result.
def in_place(xs):
    xs.sort()
    i = 0
    j = i + 1
    while j < len(xs):
        pa, pb = xs[i]
        ca, cb = xs[j]
        if ca <= pb:
            xs[i] = (pa, max(pb, cb))
            del xs[j]
        else:
            i = j
            j += 1
    return xs


def test_nip():
    inputs = [
        [(1, 3), (2, 4)],
        [(5, 6), (6, 8)],
        [(1, 8), (2, 5)],
        [(1, 3), (4, 8)],
        [(1, 4), (2, 5), (5, 8)],
        [(5, 8), (1, 4), (6, 8)],
        [(1, 10), (2, 5), (6, 8), (9, 10), (10, 12)],
        [(0, 1), (3, 5), (4, 8), (10, 12), (9, 10)],
    ]
    for x in inputs:
        not_in_place(x)


def test_ip():
    inputs = [
        [(1, 3), (2, 4)],
        [(5, 6), (6, 8)],
        [(1, 8), (2, 5)],
        [(1, 3), (4, 8)],
        [(1, 4), (2, 5), (5, 8)],
        [(5, 8), (1, 4), (6, 8)],
        [(1, 10), (2, 5), (6, 8), (9, 10), (10, 12)],
        [(0, 1), (3, 5), (4, 8), (10, 12), (9, 10)],
    ]
    for x in inputs:
        in_place(x)


merge_ranges = in_place

setup = 'from __main__ import test_nip, test_ip'
print(timeit.timeit('test_nip()', number=10000, setup=setup))
print(timeit.timeit('test_ip()', number=10000, setup=setup))


# Tests
class Test(unittest.TestCase):
    def test_meetings_overlap(self):
        actual = merge_ranges([(1, 3), (2, 4)])
        expected = [(1, 4)]
        self.assertEqual(actual, expected)

    def test_meetings_touch(self):
        actual = merge_ranges([(5, 6), (6, 8)])
        expected = [(5, 8)]
        self.assertEqual(actual, expected)

    def test_meeting_contains_other_meeting(self):
        actual = merge_ranges([(1, 8), (2, 5)])
        expected = [(1, 8)]
        self.assertEqual(actual, expected)

    def test_meetings_stay_separate(self):
        actual = merge_ranges([(1, 3), (4, 8)])
        expected = [(1, 3), (4, 8)]
        self.assertEqual(actual, expected)

    def test_multiple_merged_meetings(self):
        actual = merge_ranges([(1, 4), (2, 5), (5, 8)])
        expected = [(1, 8)]
        self.assertEqual(actual, expected)

    def test_meetings_not_sorted(self):
        actual = merge_ranges([(5, 8), (1, 4), (6, 8)])
        expected = [(1, 4), (5, 8)]
        self.assertEqual(actual, expected)

    def test_one_long_meeting_contains_smaller_meetings(self):
        actual = merge_ranges([(1, 10), (2, 5), (6, 8), (9, 10), (10, 12)])
        expected = [(1, 12)]
        self.assertEqual(actual, expected)

    def test_sample_input(self):
        actual = merge_ranges([(0, 1), (3, 5), (4, 8), (10, 12), (9, 10)])
        expected = [(0, 1), (3, 8), (9, 12)]
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

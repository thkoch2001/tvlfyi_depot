import unittest


################################################################################
# Solution
################################################################################
# do_merge_ranges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
def do_merge_ranges(prev, xs):
    if len(xs) == 0:
        return prev
    elif len(xs) == 1:
        return prev + xs
    else:
        a1, a2 = xs[0]
        b1, b2 = xs[1]
        rest = xs[2:]
        if b1 <= a2:
            return do_merge_ranges(prev, [(a1, max(a2, b2))] + rest)
        else:
            return do_merge_ranges(prev + [(a1, a2)], [(b1, b2)] + rest)


# merge_ranges :: [(Int, Int)] -> [(Int, Int)]
def merge_ranges(xs):
    xs = xs[:]
    xs.sort()
    return do_merge_ranges([], xs)


# merge_ranges_b :: [(Int, Int)] -> [(Int, Int)]
def merge_ranges_b(xs):
    fi = 0
    ci = 1
    result = []
    xs = xs[:]
    xs.sort()
    while ci < len(xs):
        while ci < len(xs) and xs[ci][0] <= xs[fi][1]:
            xs[fi] = xs[fi][0], max(xs[ci][1], xs[fi][1])
            ci += 1
        result.append(xs[fi])
        fi = ci
        ci += 1
    if fi < len(xs):
        result.append(xs[fi])
    return result


################################################################################
# Tests
################################################################################
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

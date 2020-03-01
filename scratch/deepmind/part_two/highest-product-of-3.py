import unittest
import sys
import trace


def highest_product_of_3(xs):
    if len(xs) < 3:
        raise Exception("List needs to contain at least three elements.")
    hp3 = xs[0] * xs[1] * xs[2]
    hp2 = xs[0] * xs[1]
    lp2 = xs[0] * xs[1]
    hn = max(xs[0], xs[1])
    ln = min(xs[0], xs[1])
    for x in xs[2:]:
        hp3 = max(hp3, hp2 * x, lp2 * x)
        hp2 = max(hp2, hn * x, ln * x)
        lp2 = min(lp2, hn * x, ln * x)
        hn = max(hn, x)
        ln = min(ln, x)
    return hp3


# Tests
class Test(unittest.TestCase):
    def test_short_list(self):
        actual = highest_product_of_3([1, 2, 3, 4])
        expected = 24
        self.assertEqual(actual, expected)

    def test_longer_list(self):
        actual = highest_product_of_3([6, 1, 3, 5, 7, 8, 2])
        expected = 336
        self.assertEqual(actual, expected)

    def test_list_has_one_negative(self):
        actual = highest_product_of_3([-5, 4, 8, 2, 3])
        expected = 96
        self.assertEqual(actual, expected)

    def test_list_has_two_negatives(self):
        actual = highest_product_of_3([-10, 1, 3, 2, -10])
        expected = 300
        self.assertEqual(actual, expected)

    def test_list_is_all_negatives(self):
        actual = highest_product_of_3([-5, -1, -3, -2])
        expected = -6
        self.assertEqual(actual, expected)

    def test_error_with_empty_list(self):
        with self.assertRaises(Exception):
            highest_product_of_3([])

    def test_error_with_one_number(self):
        with self.assertRaises(Exception):
            highest_product_of_3([1])

    def test_error_with_two_numbers(self):
        with self.assertRaises(Exception):
            highest_product_of_3([1, 1])

    def test_custom(self):
        actual = highest_product_of_3([9, 5, 2, 1, 7, 3])
        expected = 9 * 7 * 5
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)


def main():
    highest_product_of_3([-5, -1, -3, -2])


tracer = trace.Trace(ignoredirs=[sys.prefix, sys.exec_prefix],
                     trace=0,
                     count=1)

tracer.run('main()')
r = tracer.results()
r.write_results(show_missing=True, coverdir=".")

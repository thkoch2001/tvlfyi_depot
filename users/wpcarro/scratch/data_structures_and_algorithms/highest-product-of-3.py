import unittest


################################################################################
# Solution
################################################################################
# f :: [Int] -> Int
def highest_product_of_3(xs):
    """Here we're greedily storing:
    - current max
    - largest product of two
    - largest positive number
    - second largest positive number
    - largest negative number
    """
    if len(xs) < 3:
        raise Exception

    cm = None
    ld = xs[0] * xs[1]
    l2 = min(xs[0], xs[1])
    if xs[0] < 0 or xs[1] < 0:
        ln = min(xs[0], xs[1])
    else:
        ln = 1
    l = max(xs[0], xs[1])

    for x in xs[2:]:
        if not cm:
            cm = max(x * ln * l, ld * x, x * l * l2)  # beware
            ld = max(ld, x * ln, x * l)
            ln = min(ln, x)
            l = max(l, x)
            if x < l:
                l2 = max(l2, x)
        else:
            cm = max(cm, x * ln * l, x * ld, x * l * l2)
            ld = max(ld, x * ln, x * l)
            ln = min(ln, x)
            l = max(l, x)
            if x < l:
                l2 = max(l2, x)

    return cm


################################################################################
# Tests
################################################################################
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


unittest.main(verbosity=2)

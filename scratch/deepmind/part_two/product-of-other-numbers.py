import unittest


# get_products_of_all_ints_except_at_index :: [Int] -> [Int]
def get_products_of_all_ints_except_at_index(xs):
    n = len(xs)
    if n < 2:
        raise Exception("Cannot computer without 2 or elements")
    # lhs
    befores = [None] * n
    befores[0] = 1
    for i in range(1, n):
        befores[i] = befores[i - 1] * xs[i - 1]

    # rhs
    afters = [None] * n
    afters[-1] = 1
    for i in range(n - 2, -1, -1):
        afters[i] = afters[i + 1] * xs[i + 1]

    result = [None] * n
    for i in range(n):
        result[i] = befores[i] * afters[i]
    return result


# Tests
class Test(unittest.TestCase):
    def test_small_list(self):
        actual = get_products_of_all_ints_except_at_index([1, 2, 3])
        expected = [6, 3, 2]
        self.assertEqual(actual, expected)

    def test_longer_list(self):
        actual = get_products_of_all_ints_except_at_index([8, 2, 4, 3, 1, 5])
        expected = [120, 480, 240, 320, 960, 192]
        self.assertEqual(actual, expected)

    def test_list_has_one_zero(self):
        actual = get_products_of_all_ints_except_at_index([6, 2, 0, 3])
        expected = [0, 0, 36, 0]
        self.assertEqual(actual, expected)

    def test_list_has_two_zeros(self):
        actual = get_products_of_all_ints_except_at_index([4, 0, 9, 1, 0])
        expected = [0, 0, 0, 0, 0]
        self.assertEqual(actual, expected)

    def test_one_negative_number(self):
        actual = get_products_of_all_ints_except_at_index([-3, 8, 4])
        expected = [32, -12, -24]
        self.assertEqual(actual, expected)

    def test_all_negative_numbers(self):
        actual = get_products_of_all_ints_except_at_index([-7, -1, -4, -2])
        expected = [-8, -56, -14, -28]
        self.assertEqual(actual, expected)

    def test_error_with_empty_list(self):
        with self.assertRaises(Exception):
            get_products_of_all_ints_except_at_index([])

    def test_error_with_one_number(self):
        with self.assertRaises(Exception):
            get_products_of_all_ints_except_at_index([1])


unittest.main(verbosity=2)

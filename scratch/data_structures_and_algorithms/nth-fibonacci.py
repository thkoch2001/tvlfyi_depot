import unittest


################################################################################
# Solution
################################################################################
def fib(n):
    """This should be accomplishable in O(1) space."""
    if n in {0, 1}:
        return n
    a = 0  # i = 0
    b = 1  # i = 1
    for x in range(2, n + 1):
        result = a + b
        a = b
        b = result
    return result


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_zeroth_fibonacci(self):
        actual = fib(0)
        expected = 0
        self.assertEqual(actual, expected)

    def test_first_fibonacci(self):
        actual = fib(1)
        expected = 1
        self.assertEqual(actual, expected)

    def test_second_fibonacci(self):
        actual = fib(2)
        expected = 1
        self.assertEqual(actual, expected)

    def test_third_fibonacci(self):
        actual = fib(3)
        expected = 2
        self.assertEqual(actual, expected)

    def test_fifth_fibonacci(self):
        actual = fib(5)
        expected = 5
        self.assertEqual(actual, expected)

    def test_tenth_fibonacci(self):
        actual = fib(10)
        expected = 55
        self.assertEqual(actual, expected)

    def test_negative_fibonacci(self):
        with self.assertRaises(Exception):
            fib(-1)


unittest.main(verbosity=2)

import unittest


# Compute the fibonacci using a bottom-up algorithm.
def fib(n):
    if n < 0:
        raise Error('Cannot call fibonacci with negative values')
    cache = [0, 1]
    for i in range(n):
        cache[0], cache[1] = cache[1], cache[0] + cache[1]
    return cache[0]


# Compute the fibonacci using memoization.
def fib_memoized(n):
    cache = {
        0: 0,
        1: 1,
    }

    def do_fib(n):
        if n < 0:
            raise Error('The fib function does not support negative inputs')

        if n in cache:
            return cache[n]

        cache[n - 1] = do_fib(n - 1)
        cache[n - 2] = do_fib(n - 2)
        return cache[n - 1] + cache[n - 2]

    return do_fib(n)


# Tests
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

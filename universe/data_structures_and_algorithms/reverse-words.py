from collections import deque
import unittest

################################################################################
# Solution
################################################################################


def rev(xs, i, j):
    """Reverse xs in place from [i, j]"""
    while i < j:
        xs[i], xs[j] = xs[j], xs[i]
        i += 1
        j -= 1


def rotate(xs, n, i=None, j=None):
    """Mutably rotates list, xs, n times. Positive n values rotate right while
    negative n values rotate left. Rotate within window [i, j]."""
    i = i or 0
    j = j or len(xs) - 1
    ct = j - i

    if n < 0:
        n = abs(n)
        p = i + n - 1
        rev(xs, i, p)
        rev(xs, p + 1, j)
        rev(xs, i, j)
    else:
        p = j - (n - 1)
        rev(xs, p, j)
        rev(xs, i, p - 1)
        rev(xs, i, j)
    return xs


def rev_words(xs, i, j):
    if j + 1 == len(xs):
        return 0

    while j + 1 < len(xs):
        while j + 1 < len(xs) and xs[j + 1] != ' ':
            j += 1

        rev(xs, i, j)
        j += 2
        i = j

    return 0


def reverse_words(xs):
    # first reverse everything
    rev(xs, 0, len(xs) - 1)
    return rev_words(xs, 0, 0)


def reverse_words_bak(xs, i=None, j=None):
    i = i or 0
    j = j or len(xs) - 1
    w0, w1 = [], []

    if i >= j:
        return 0

    pi = i
    while pi < len(xs) and xs[pi] != ' ':
        w0.append(xs[pi])
        pi += 1

    if pi == len(xs):
        return 0

    pj = j
    while xs[pj] != ' ':
        w1.append(xs[pj])
        pj -= 1

    d = len(w0) - len(w1)

    rotate(xs, -1 * d, i, j)

    for k in range(len(w1)):
        xs[i + k] = w1[len(w1) - 1 - k]

    for k in range(len(w0)):
        xs[j - k] = w0[len(w0) - 1 - k]

    while i != j and xs[i] != ' ' and xs[j] != ' ':
        i += 1
        j -= 1

    if i == j:
        return 0

    elif xs[i] == ' ':
        while j > 0 and xs[j] != ' ':
            j -= 1
        if j == 0:
            return 0
    elif xs[j] == ' ':
        while i < len(xs) and xs[i] != ' ':
            i += 1
        if i == len(xs):
            return 0
    return reverse_words(xs, i + 1, j - 1)


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_rev(self):
        xs = [1, 2, 3, 4, 5]
        rev(xs, 0, len(xs) - 1)
        self.assertEqual(xs, [5, 4, 3, 2, 1])

    def test_rotate(self):
        ys = [1, 2, 3, 4, 5]
        xs = ys[:]
        self.assertEqual(rotate(xs, 1, 1, 3), [1, 4, 2, 3, 5])
        xs = ys[:]
        self.assertEqual(rotate(xs, -1, 1, 3), [1, 3, 4, 2, 5])
        xs = ys[:]
        self.assertEqual(rotate(xs, 1), [5, 1, 2, 3, 4])
        xs = ys[:]
        self.assertEqual(rotate(xs, -1), [2, 3, 4, 5, 1])
        xs = ys[:]
        self.assertEqual(rotate(xs, -2), [3, 4, 5, 1, 2])
        xs = ys[:]
        self.assertEqual(rotate(xs, -5), [1, 2, 3, 4, 5])
        xs = ys[:]
        self.assertEqual(rotate(xs, 5), [1, 2, 3, 4, 5])
        xs = ys[:]
        self.assertEqual(rotate(xs, 3), [3, 4, 5, 1, 2])

    def test_one_word(self):
        message = list('vault')
        reverse_words(message)
        expected = list('vault')
        self.assertEqual(message, expected)

    def test_two_words(self):
        message = list('thief cake')
        reverse_words(message)
        expected = list('cake thief')
        self.assertEqual(message, expected)

    def test_three_words(self):
        message = list('one another get')
        reverse_words(message)
        expected = list('get another one')
        self.assertEqual(message, expected)

    def test_multiple_words_same_length(self):
        message = list('rat the ate cat the')
        reverse_words(message)
        expected = list('the cat ate the rat')
        self.assertEqual(message, expected)

    def test_multiple_words_different_lengths(self):
        message = list('at rat house')
        reverse_words(message)
        expected = list('house rat at')
        self.assertEqual(message, expected)

    def test_multiple_words_different_lengths(self):
        message = list('yummy is cake bundt chocolate')
        reverse_words(message)
        expected = list('chocolate bundt cake is yummy')
        self.assertEqual(message, expected)

    def test_empty_string(self):
        message = list('')
        reverse_words(message)
        expected = list('')
        self.assertEqual(message, expected)


unittest.main(verbosity=2)

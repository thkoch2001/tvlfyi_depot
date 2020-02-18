import unittest


def reverse(xs, i, j):
    """Reverse array of characters, xs, in-place."""
    while i < j:
        xs[i], xs[j] = xs[j], xs[i]
        i += 1
        j -= 1


def reverse_words(xs):
    reverse(xs, 0, len(xs) - 1)
    i = 0
    j = i
    while j < len(xs):
        while j < len(xs) and xs[j] != ' ':
            j += 1
        reverse(xs, i, j - 1)
        j += 1
        i = j


# Tests
class Test(unittest.TestCase):
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

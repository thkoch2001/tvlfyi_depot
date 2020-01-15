from collections import Counter
import unittest


################################################################################
# Impl
################################################################################
# palindromifiable :: String -> Boolean
def has_palindrome_permutation(x):
    bag = Counter(x)
    odd_entries_ct = 0

    for _, y in bag.items():
        if y % 2 != 0:
            odd_entries_ct += 1

    return odd_entries_ct in {0, 1}


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_permutation_with_odd_number_of_chars(self):
        result = has_palindrome_permutation('aabcbcd')
        self.assertTrue(result)

    def test_permutation_with_even_number_of_chars(self):
        result = has_palindrome_permutation('aabccbdd')
        self.assertTrue(result)

    def test_no_permutation_with_odd_number_of_chars(self):
        result = has_palindrome_permutation('aabcd')
        self.assertFalse(result)

    def test_no_permutation_with_even_number_of_chars(self):
        result = has_palindrome_permutation('aabbcd')
        self.assertFalse(result)

    def test_empty_string(self):
        result = has_palindrome_permutation('')
        self.assertTrue(result)

    def test_one_character_string(self):
        result = has_palindrome_permutation('a')
        self.assertTrue(result)


unittest.main(verbosity=2)

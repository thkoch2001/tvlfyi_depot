import unittest


################################################################################
# Solution
################################################################################
# is_valid :: String -> Boolean
def is_valid(xs):
    s = []
    seeking = {
        '}': '{',
        ']': '[',
        ')': '(',
    }
    openers = seeking.values()
    closers = seeking.keys()
    for c in xs:
        if c in openers:
            s.append(c)
        elif c in closers:
            if not s:
                return False
            elif s[-1] != seeking.get(c):
                return False
            else:
                s.pop()
    return len(s) == 0


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_valid_short_code(self):
        result = is_valid('()')
        self.assertTrue(result)

    def test_valid_longer_code(self):
        result = is_valid('([]{[]})[]{{}()}')
        self.assertTrue(result)

    def test_interleaved_openers_and_closers(self):
        result = is_valid('([)]')
        self.assertFalse(result)

    def test_mismatched_opener_and_closer(self):
        result = is_valid('([][]}')
        self.assertFalse(result)

    def test_missing_closer(self):
        result = is_valid('[[]()')
        self.assertFalse(result)

    def test_extra_closer(self):
        result = is_valid('[[]]())')
        self.assertFalse(result)

    def test_empty_string(self):
        result = is_valid('')
        self.assertTrue(result)


unittest.main(verbosity=2)

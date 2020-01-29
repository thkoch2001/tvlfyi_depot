import unittest


################################################################################
# Answer
################################################################################
class Node(object):
    def __init__(self, value, children=set()):
        self.value = value
        self.children = children


# treeify :: Char -> Set(Char) -> Node(Char)
def treeify(x, xs):
    return Node(x, [treeify(c, xs - {c}) for c in xs])


# dft :: Node(Char) -> [String]
def dft(node):
    result = []
    s = []

    s.append(('', node))

    while s:
        p, n = s.pop()
        p += str(n.value)

        if not n.children:
            result.append(p)
        else:
            for c in n.children:
                s.append((p, c))

    return result


# main :: String -> Set(String)
def get_permutations(xs):
    if xs == '':
        return set([''])

    ys = set(xs)
    trees = []

    for y in ys:
        trees.append(treeify(y, ys - {y}))

    result = set()

    for t in trees:
        for d in dft(t):
            result.add(d)

    return result


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_empty_string(self):
        actual = get_permutations('')
        expected = set([''])
        self.assertEqual(actual, expected)

    def test_one_character_string(self):
        actual = get_permutations('a')
        expected = set(['a'])
        self.assertEqual(actual, expected)

    def test_two_character_string(self):
        actual = get_permutations('ab')
        expected = set(['ab', 'ba'])
        self.assertEqual(actual, expected)

    def test_three_character_string(self):
        actual = get_permutations('abc')
        expected = set(['abc', 'acb', 'bac', 'bca', 'cab', 'cba'])
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)

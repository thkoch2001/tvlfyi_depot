import unittest
from itertools import permutations


class Node(object):
    def __init__(self, x):
        self.value = x
        self.children = []


def make_tree(c, xs):
    root = Node(c)
    for x in xs:
        root.children.append(make_tree(x, xs - {x}))
    return root


def get_permutations(xs):
    xs = set(xs)
    root = make_tree("", xs)
    q, perms = [], set()
    q.append(("", root))
    while q:
        c, node = q.pop()
        if not node.children:
            perms.add(c)
        else:
            for child in node.children:
                q.append((c + child.value, child))
    return perms


# Tests
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

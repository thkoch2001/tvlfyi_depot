import unittest
from collections import deque


################################################################################
# Implementation
################################################################################
def is_leaf(node):
    return node.left is None and node.right is None


def find_largest(node):
    current = node
    while current.right is not None:
        current = current.right
    return current.value


def find_second_largest(node):
    history = deque()
    current = node

    while current.right:
        history.append(current)
        current = current.right

    if current.left:
        return find_largest(current.left)
    elif history:
        return history.pop().value
    else:
        raise TypeError


def find_second_largest_backup(node):
    history = deque()
    current = node

    # traverse -> largest
    while current.right:
        history.append(current)
        current = current.right

    if current.left:
        return find_largest(current.left)
    elif history:
        return history.pop().value
    else:
        raise ArgumentError


# Write a iterative version to avoid consuming memory with the call stack.
# Commenting out the recursive code for now.
def find_second_largest_backup(node):
    if node.left is None and node.right is None:
        raise ArgumentError

    elif node.right is None and is_leaf(node.left):
        return node.left.value

    # recursion
    # elif node.right is None:
    #     return find_largest(node.left)

    # iterative version
    elif node.right is None:
        current = node.left
        while current.right is not None:
            current = current.right
        return current.value

    # recursion
    # TODO: Remove recursion from here.
    elif not is_leaf(node.right):
        return find_second_largest(node.right)

    # could do an else here, but let's be more assertive.
    elif is_leaf(node.right):
        return node.value

    else:
        raise ArgumentError


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    class BinaryTreeNode(object):
        def __init__(self, value):
            self.value = value
            self.left = None
            self.right = None

        def insert_left(self, value):
            self.left = Test.BinaryTreeNode(value)
            return self.left

        def insert_right(self, value):
            self.right = Test.BinaryTreeNode(value)
            return self.right

    def test_full_tree(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        right = tree.insert_right(70)
        left.insert_left(10)
        left.insert_right(40)
        right.insert_left(60)
        right.insert_right(80)
        actual = find_second_largest(tree)
        expected = 70
        self.assertEqual(actual, expected)

    def test_largest_has_a_left_child(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        right = tree.insert_right(70)
        left.insert_left(10)
        left.insert_right(40)
        right.insert_left(60)
        actual = find_second_largest(tree)
        expected = 60
        self.assertEqual(actual, expected)

    def test_largest_has_a_left_subtree(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        right = tree.insert_right(70)
        left.insert_left(10)
        left.insert_right(40)
        right_left = right.insert_left(60)
        right_left_left = right_left.insert_left(55)
        right_left.insert_right(65)
        right_left_left.insert_right(58)
        actual = find_second_largest(tree)
        expected = 65
        self.assertEqual(actual, expected)

    def test_second_largest_is_root_node(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        tree.insert_right(70)
        left.insert_left(10)
        left.insert_right(40)
        actual = find_second_largest(tree)
        expected = 50
        self.assertEqual(actual, expected)

    def test_descending_linked_list(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(40)
        left_left = left.insert_left(30)
        left_left_left = left_left.insert_left(20)
        left_left_left.insert_left(10)
        actual = find_second_largest(tree)
        expected = 40
        self.assertEqual(actual, expected)

    def test_ascending_linked_list(self):
        tree = Test.BinaryTreeNode(50)
        right = tree.insert_right(60)
        right_right = right.insert_right(70)
        right_right.insert_right(80)
        actual = find_second_largest(tree)
        expected = 70
        self.assertEqual(actual, expected)

    def test_error_when_tree_has_one_node(self):
        tree = Test.BinaryTreeNode(50)
        with self.assertRaises(Exception):
            find_second_largest(tree)

    def test_error_when_tree_is_empty(self):
        with self.assertRaises(Exception):
            find_second_largest(None)


unittest.main(verbosity=2)

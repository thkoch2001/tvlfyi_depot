import unittest
from collections import deque


# While this function solves the problem, it uses O(n) space since we're storing
# all of the less-thans and greater-thans.
def is_binary_search_tree_first_attempt(root):
    q = deque()
    q.append((set(), set(), root))

    while q:
        lts, gts, node = q.popleft()

        if not all([node.value < lt for lt in lts]):
            return False
        if not all([node.value > gt for gt in gts]):
            return False

        if node.left:
            q.append((lts | {node.value}, gts, node.left))
        if node.right:
            q.append((lts, gts | {node.value}, node.right))

    return True


# While I did not originally solve this problem this way, when I learned that I
# could condense the space of my solution's runtime, I wrote this.
def is_binary_search_tree(root):
    q = deque()
    q.append((None, None, root))

    while q:
        lt, gt, node = q.popleft()

        if not lt is None and node.value >= lt:
            return False
        if not gt is None and node.value <= gt:
            return False

        if node.left:
            q.append((node.value, gt, node.left))
        if node.right:
            q.append((lt, node.value, node.right))

    return True


# Tests
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

    def test_valid_full_tree(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        right = tree.insert_right(70)
        left.insert_left(10)
        left.insert_right(40)
        right.insert_left(60)
        right.insert_right(80)
        result = is_binary_search_tree(tree)
        self.assertTrue(result)

    def test_both_subtrees_valid(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(30)
        right = tree.insert_right(80)
        left.insert_left(20)
        left.insert_right(60)
        right.insert_left(70)
        right.insert_right(90)
        result = is_binary_search_tree(tree)
        self.assertFalse(result)

    def test_descending_linked_list(self):
        tree = Test.BinaryTreeNode(50)
        left = tree.insert_left(40)
        left_left = left.insert_left(30)
        left_left_left = left_left.insert_left(20)
        left_left_left.insert_left(10)
        result = is_binary_search_tree(tree)
        self.assertTrue(result)

    def test_out_of_order_linked_list(self):
        tree = Test.BinaryTreeNode(50)
        right = tree.insert_right(70)
        right_right = right.insert_right(60)
        right_right.insert_right(80)
        result = is_binary_search_tree(tree)
        self.assertFalse(result)

    def test_one_node_tree(self):
        tree = Test.BinaryTreeNode(50)
        result = is_binary_search_tree(tree)
        self.assertTrue(result)


unittest.main(verbosity=2)

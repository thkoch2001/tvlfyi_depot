import unittest


################################################################################
# Implementation
################################################################################
# is_leaf :: Node(a) -> Boolean
def is_leaf(node):
    return not node.left and not node.right


# is_binary_search_tree :: Node(Integer) -> Set(Int) -> Set(Int) -> Boolean
def is_binary_search_tree_a(node, la=set(), ra=set()):
    """My first solution for this problem."""
    for x in la:
        if not node.value < x:
            return False
    for x in ra:
        if not node.value > x:
            return False
    if is_leaf(node):
        return True
    elif not node.left:
        return is_binary_search_tree(
            node.right,
            la=la,
            ra=ra ^ {node.value},
        )
    elif not node.right:
        return is_binary_search_tree(node.left, la=la ^ {node.value}, ra=ra)
    else:
        return all([
            is_binary_search_tree(node.left, la=la ^ {node.value}, ra=ra),
            is_binary_search_tree(node.right, la=la, ra=ra ^ {node.value})
        ])


# is_binary_search_tree :: Node(Int) -> Maybe(Int) -> Maybe(Int) -> Boolean
def is_binary_search_tree(node, lb=None, ub=None):
    if lb:
        if node.value < lb:
            return False
    if ub:
        if node.value > ub:
            return False
    if is_leaf(node):
        return True
    elif not node.right:
        return is_binary_search_tree(node.left, lb=lb, ub=node.value)
    elif not node.left:
        return is_binary_search_tree(node.right, lb=node.value, ub=ub)
    else:
        return is_binary_search_tree(
            node.left, lb=lb, ub=node.value) and is_binary_search_tree(
                node.right, lb=node.value, ub=ub)


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

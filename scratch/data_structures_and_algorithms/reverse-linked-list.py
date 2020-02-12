import unittest


################################################################################
# Solution
################################################################################
# reverse :: List(a) -> List(a)
def reverse(node):
    curr = node
    prev = None
    while curr:
        nxt = curr.next
        curr.next = prev
        prev = curr
        curr = nxt
    # Make sure to understand the spec! Debugging takes time. Rewriting takes
    # time.
    return prev


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    class LinkedListNode(object):
        def __init__(self, value, next=None):
            self.value = value
            self.next = next

        def get_values(self):
            node = self
            values = []
            while node is not None:
                values.append(node.value)
                node = node.next
            return values

    def test_short_linked_list(self):
        second = Test.LinkedListNode(2)
        first = Test.LinkedListNode(1, second)

        result = reverse(first)
        self.assertIsNotNone(result)

        actual = result.get_values()
        expected = [2, 1]
        self.assertEqual(actual, expected)

    def test_long_linked_list(self):
        sixth = Test.LinkedListNode(6)
        fifth = Test.LinkedListNode(5, sixth)
        fourth = Test.LinkedListNode(4, fifth)
        third = Test.LinkedListNode(3, fourth)
        second = Test.LinkedListNode(2, third)
        first = Test.LinkedListNode(1, second)

        result = reverse(first)
        self.assertIsNotNone(result)

        actual = result.get_values()
        expected = [6, 5, 4, 3, 2, 1]
        self.assertEqual(actual, expected)

    def test_one_element_linked_list(self):
        first = Test.LinkedListNode(1)

        result = reverse(first)
        self.assertIsNotNone(result)

        actual = result.get_values()
        expected = [1]
        self.assertEqual(actual, expected)

    def test_empty_linked_list(self):
        result = reverse(None)
        self.assertIsNone(result)


unittest.main(verbosity=2)

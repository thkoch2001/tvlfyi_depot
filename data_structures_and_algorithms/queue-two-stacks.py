import unittest


################################################################################
# Solution
################################################################################
class QueueTwoStacks(object):
    def __init__(self):
        self.a = []
        self.b = []

    def enqueue(self, x):
        self.a.append(x)

    def dequeue(self):
        if self.b:
            return self.b.pop()
        else:
            while self.a:
                self.b.append(self.a.pop())
            return self.dequeue()


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_basic_queue_operations(self):
        queue = QueueTwoStacks()
        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)
        actual = queue.dequeue()
        expected = 1
        self.assertEqual(actual, expected)
        actual = queue.dequeue()
        expected = 2
        self.assertEqual(actual, expected)
        queue.enqueue(4)
        actual = queue.dequeue()
        expected = 3
        self.assertEqual(actual, expected)
        actual = queue.dequeue()
        expected = 4
        self.assertEqual(actual, expected)

    def test_error_when_dequeue_from_new_queue(self):
        queue = QueueTwoStacks()
        with self.assertRaises(Exception):
            queue.dequeue()

    def test_error_when_dequeue_from_empty_queue(self):
        queue = QueueTwoStacks()
        queue.enqueue(1)
        queue.enqueue(2)
        actual = queue.dequeue()
        expected = 1
        self.assertEqual(actual, expected)
        actual = queue.dequeue()
        expected = 2
        self.assertEqual(actual, expected)
        with self.assertRaises(Exception):
            queue.dequeue()


unittest.main(verbosity=2)

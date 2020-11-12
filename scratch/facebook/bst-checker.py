from collections import deque

class Node(object):
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def insert_left(self, value):
        self.left = Node(value)
        return self.left

    def insert_right(self, value):
        self.right = Node(value)
        return self.right

    def min(self):
        xs = deque()
        result = float('inf')
        xs.append(self)
        while xs:
            node = xs.popleft()
            result = min(result, node.value)
            if node.left:
                xs.append(node.left)
            if node.right:
                xs.append(node.right)
        return result

    def max(self):
        xs = deque()
        result = float('-inf')
        xs.append(self)
        while xs:
            node = xs.popleft()
            result = max(result, node.value)
            if node.left:
                xs.append(node.left)
            if node.right:
                xs.append(node.right)
        return result

    def is_bst(self):
        result = True
        if self.left:
            result = result and self.left.max() < self.value
        if self.right:
            result = result and self.right.min() > self.value
        return result


x = Node(
    50,
    Node(
        17,
        Node(
            12,
            Node(9),
            Node(14),
        ),
        Node(
            23,
            Node(19),
        ),
    ),
    Node(
        72,
        Node(
            54,
            None,
            Node(67)
        ),
        Node(76),
    ),
)


assert x.is_bst()
print("Success!")

from collections import deque

class Node(object):
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def is_bst(self):
        s = []
        s.append((float('-inf'), self, float('inf')))
        while s:
            lo, node, hi = s.pop()
            if lo <= node.value <= hi:
                node.left and s.append((lo, node.left, node.value))
                node.right and s.append((node.value, node.right, hi))
            else:
                return False
        return True


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

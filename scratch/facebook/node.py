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

tree = Node(
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

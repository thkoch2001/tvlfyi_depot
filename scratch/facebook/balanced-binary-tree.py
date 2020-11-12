from collections import deque

class Node(object):
    # __init__ :: T(A)
    def __init__(self, value=None, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    # insert_left :: T(A) -> A -> T(A)
    def insert_left(self, value):
        self.left = Node(value)
        return self.left

    # insert_right :: T(A) -> A -> T(A)
    def insert_right(self, value):
        self.right = Node(value)
        return self.right

    # is_superbalanced :: T(A) -> Bool
    def is_superbalanced(self):
        xs = deque()
        min_depth, max_depth = float('inf'), float('-inf')
        xs.append((self, 0))
        while xs:
            x, d = xs.popleft()
            # Only redefine the depths at leaf nodes
            if not x.left and not x.right:
                min_depth, max_depth = min(min_depth, d), max(max_depth, d)
            if x.left:
                xs.append((x.left, d + 1))
            if x.right:
                xs.append((x.right, d + 1))
        return max_depth - min_depth <= 1

    # __repr__ :: T(A) -> String
    def __repr__(self):
        result = ''
        xs = deque()
        xs.append((self, 0))
        while xs:
            node, indent = xs.popleft()
            result += '{i}{x}\n'.format(i=' ' * indent, x=node.value)
            if node.left:
                xs.append((node.left, indent + 2))
            if node.right:
                xs.append((node.right, indent + 2))
        return result

# from_array :: List(A) -> T(A)
def from_array(values):
    xs = deque()
    root = Node()
    xs.append(root)
    for value in values:
        node = xs.popleft()
        node.value = value
        node.left = Node()
        xs.append(node.left)
        node.right = Node()
        xs.append(node.right)
    return root

x = from_array([1, 1, 1, 1, 1, 1, 1])
print(x)
print(x.is_superbalanced())

x = Node(1, Node(2), Node(3))
print(x)
print(x.is_superbalanced())

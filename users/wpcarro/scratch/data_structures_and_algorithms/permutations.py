class Node(object):
    # ctor :: a -> [a] -> Node(a)
    def __init__(self, value, children=[]):
        self.value = value
        self.children = children


# is_leaf :: Node(a) -> Boolean
def is_leaf(node):
    return len(node.children) == 0


# enumerate :: Node(a) -> Set(List(a))
def enumerate(node):
    current = []
    result = []
    q = []

    q.append(node)

    while q:
        x = q.pop()
        print(x.value)

        for c in x.children:
            q.append(c)

        current.append(x.value)
        print(current)

        if is_leaf(x):
            result.append(current)
            print("Reseting current")
            current = []

    return result


node = Node('root', [
    Node('a', [
        Node('b', [Node('c')]),
        Node('c', [Node('b')]),
    ]),
    Node('b', [
        Node('a', [Node('c')]),
        Node('c', [Node('a')]),
    ]),
    Node('c', [
        Node('a', [Node('b')]),
        Node('b', [Node('a')]),
    ])
])

print('----------')
print(enumerate(node))

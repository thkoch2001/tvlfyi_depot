class Node(object):
    def __init__(self, value, children=[]):
        self.value = value
        self.children = children


################################################################################
# Solution
################################################################################
def trip_time(node):
    s = []
    result = 0
    s.append((node.value, node))
    while s:
        p, node = s.pop()
        if not node.children:
            result = max(result, p)
        for x in node.children:
            s.append((p + x.value, x))
    return result


################################################################################
# Tests
################################################################################
tree = Node(
    0,
    children=[
        Node(5, children=[Node(6)]),
        Node(2, children=[
            Node(6),
            Node(10),
        ]),
        Node(3, children=[Node(2, children=[Node(11)])]),
    ])

assert trip_time(tree) == 16
print("Tests pass!")

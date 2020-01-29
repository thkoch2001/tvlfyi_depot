from random import choice


class Node(object):
    def __init__(self, value=None, left=None, right=None):
        self.value = value
        self.left = left
        self.right = left


def p(node, indent=0):
    print(indent * ' ' + '|-' + str(node.value))
    if node.left is not None:
        p(node.left, indent=indent + 2)
    if node.right is not None:
        p(node.right, indent=indent + 2)


# read trees (i.e. traversing, parsing)
# write trees (i.e. generating, printing)
def random(d=0):
    left = None
    right = None

    if choice([True, False]):
        left = random(d + 1)

    if choice([True, False]):
        right = random(d + 1)

    return Node(
        value=d,
        left=left,
        right=right,
    )


################################################################################
# DFTs can be:
# - imperative (mutable)
# - functional (immutable)
# - iterative
# - recursive
################################################################################


# Iterative
def traverse(node, f):
    stack = [(node, 0)]

    while len(stack):
        node, depth = stack.pop()
        f(node, depth)
        print(depth)

        if node.left is not None:
            stack.append((node.left, depth + 1))
        if node.right is not None:
            stack.append((node.right, depth + 1))


print('----------------------------------------------------------------------')
for _ in range(10):
    traverse(random(), lambda _, d: print(d))
print()

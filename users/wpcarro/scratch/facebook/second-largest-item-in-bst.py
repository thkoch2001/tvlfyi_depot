from collections import deque
from node import Node, tree

def find_largest(node):
    while node.right:
        node = node.right
    return node.value

def find_second_largest(node):
    # parent of the rightmost, when rightmost is leaf
    # max(rightmost.left)
    prev = None
    while node.right:
        prev = node
        node = node.right
    if node.left:
        return find_largest(node.left)
    else:
        return prev.value

assert find_second_largest(tree) == 72
print("Success!")

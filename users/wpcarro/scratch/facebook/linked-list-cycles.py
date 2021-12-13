import random

from linked_list import Node

def contains_cycle(node):
    one = node
    two = node
    while two.next and two.next.next:
        one = one.next
        two = two.next.next
        if one == two:
            return True
    return False

xs = Node(1, Node(2, Node(3)))
assert not contains_cycle(xs)
print("Success!")

a = Node(1)
b = Node(2)
c = Node(3)
a.next = b
b.next = c
c.next = random.choice([a, b, c])
assert contains_cycle(a)
print("Success!")

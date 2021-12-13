def contains_cycle(node):
    """
    Return True if the linked-list, `node`, contains a cycle.
    """
    if not node:
        return False
    a = node
    b = node.next
    while a != b:
        a = a.next
        if b and b.next and b.next.next:
            b = b.next.next
        else:
            return False
    return True

################################################################################
# Bonus
################################################################################

def first_node_in_cycle(node):
    """
    Given that the linked-list, `node`, contains a cycle, return the first
    element of that cycle.
    """
    # enter the cycle
    a = node
    b = node.next
    while a != b:
        a = a.next
        b = b.next.next

    # get the length of the cycle
    beg = a
    a = a.next
    n = 1
    while a != beg:
        a = a.next
        n += 1

    # run b n-steps ahead of a
    a = node
    b = node
    for _ in range(n):
        b = b.next

    # where they intersect is the answer
    while a != b:
        a = a.next
        b = b.next
    return a

################################################################################
# Tests
################################################################################

class Node(object):
    def __init__(self, value, next=None):
        self.value = value
        self.next = next
    def __repr__(self):
        return "Node({}) -> ...".format(self.value)

d = Node('d')
c = Node('c', d)
b = Node('b', c)
a = Node('a', b)
d.next = b

print(first_node_in_cycle(a))

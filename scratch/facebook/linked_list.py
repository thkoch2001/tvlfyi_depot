class Node(object):
    def __init__(self, value=None, next=None):
        self.value = value
        self.next = next

    def __repr__(self):
        result = []
        node = self
        while node:
            result.append(str(node.value))
            node = node.next
        return 'LinkedList({xs})'.format(xs=', '.join(result))

def from_list(xs):
    head = Node(xs[0])
    node = head
    for x in xs[1:]:
        node.next = Node(x)
        node = node.next
    return head

list = from_list(['A', 'B', 'C'])

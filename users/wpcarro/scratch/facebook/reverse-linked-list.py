from linked_list import Node

def reverse(node):
    prev, curr, next = None, node, node.next

    while curr:
        curr.next = prev
        prev = curr
        curr = next
        next = curr.next if curr else None
    return prev

one = Node(1)
two = Node(2)
three = Node(3)
one.next = two
two.next = three

print(one)
result = reverse(one)
print(result)
assert all([result == three,
            three.next == two,
            two.next == one])
print("Success!")

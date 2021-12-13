from linked_list import Node, from_list

def delete(node):
    if not node.next:
        node.value = None
    else:
        node.value = node.next.value
        node.next = node.next.next

one = Node(1)
two = Node(2)
three = Node(3)

one.next = two
two.next = three

print(one)
delete(two)
print(one)

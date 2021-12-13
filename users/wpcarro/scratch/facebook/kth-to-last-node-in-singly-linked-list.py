from linked_list import Node, from_list

def kth_to_last_node(k, node):
    one = node
    two = node
    for _ in range(k - 1):
        if not one:
            return None
        one = one.next
    while one.next:
        one = one.next
        two = two.next
    return two.value


xs = from_list(["Angel Food", "Bundt", "Cheese", "Devil's Food", "Eccles"])
result = kth_to_last_node(2, xs)
print(result)
assert result == "Devil's Food"
print("Success!")

xs = from_list(["Angel Food", "Bundt"])
result = kth_to_last_node(30, xs)
print(result)
assert result is None
print("Success!")

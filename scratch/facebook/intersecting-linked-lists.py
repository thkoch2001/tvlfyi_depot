class LinkedList(object):
    def __init__(self, x):
        self.val = x
        self.next = None

    def __repr__(self):
        if self.next:
            return "{} -> {}".format(self.val, self.next)
        return "{}".format(self.val)

def find_intersection(a, b):
    init_a, init_b = a, b

    while a != b:
        a = a.next if a.next else init_b
        b = b.next if b.next else init_a

    return a

# make A...
e1 = LinkedList(5)
d1 = LinkedList(2); d1.next = e1
c1 = LinkedList(3); c1.next = d1 # shared
b1 = LinkedList(1); b1.next = c1 # shared
a1 = LinkedList(4); a1.next = b1 # shared

# make B...
c2 = LinkedList(1); c2.next = c1
b2 = LinkedList(5); b2.next = c2
a2 = LinkedList(6); a2.next = b2

print(a1)
print(a2)
print(find_intersection(a1, a2).val)

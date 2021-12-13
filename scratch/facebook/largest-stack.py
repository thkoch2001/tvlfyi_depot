from stack import Stack, from_list
from heapq import heapify, heappush, heappop
from random import shuffle

class MaxStack(Stack):
    def __init__(self):
        self.max = Stack()
        super().__init__()

    def __repr__(self):
        return super().__repr__()

    def push(self, x):
        super().push(x)
        max = self.get_max()
        if not max:
            self.max.push(x)
        else:
            self.max.push(max if x < max else x)

    def pop(self):
        self.max.pop()
        return super().pop()

    def get_max(self):
        return self.max.peek()

xs = list(range(1, 11))
shuffle(xs)
stack = MaxStack()
for x in xs:
    stack.push(x)

print(stack)
result = stack.get_max()
print(result)
assert result == 10

popped = stack.pop()
print("Popped: {}".format(popped))
print(stack)
while popped != 10:
    assert stack.get_max() == 10
    popped = stack.pop()
    print("Popped: {}".format(popped))
    print(stack)

assert stack.get_max() != 10
print("Success!")

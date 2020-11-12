class Stack(object):
    def __init__(self):
        self.items = []

    def __repr__(self):
        return self.items.__repr__()

    def push(self, x):
        self.items.append(x)

    def pop(self):
        if not self.items:
            return None
        return self.items.pop()

    def peek(self):
        if not self.items:
            return None
        return self.items[-1]

def from_list(xs):
    result = Stack()
    for x in xs:
        result.push(x)
    return result

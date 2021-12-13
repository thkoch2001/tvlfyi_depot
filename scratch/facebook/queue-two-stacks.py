from stack import Stack

class Queue(object):
    def __init__(self):
        self.lhs = Stack()
        self.rhs = Stack()

    def enqueue(self, x):
        self.rhs.push(x)

    def dequeue(self, x):
        y = self.rhs.pop()
        while y:
            self.lhs.push(y)
            y = self.rhs.pop()
        result = self.lhs.pop()
        y = self.lhs.pop()
        while y:
            self.rhs.push(y)
        return result

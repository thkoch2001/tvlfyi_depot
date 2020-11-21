class Queue(object):
    def __init__(self):
        self.lhs = []
        self.rhs = []

    def enqueue(self, x):
        self.lhs.append(x)

    def dequeue(self):
        if self.rhs:
            return self.rhs.pop()
        while self.lhs:
            self.rhs.append(self.lhs.pop())
        if self.rhs:
            return self.rhs.pop()
        else:
            raise Exception("Attempting to remove an item from an empty queue")

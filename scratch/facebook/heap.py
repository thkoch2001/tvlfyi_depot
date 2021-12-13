from math import floor

class Heap(object):
    def __init__(self):
        self.xs = [None]
        self.i = 1

    def __repr__(self):
        return "[{}]".format(", ".join(str(x) for x in self.xs[1:]))

    def insert(self, x):
        if len(self.xs) == 1:
            self.xs.append(x)
            self.i += 1
            return
        self.xs.append(x)
        i = self.i
        while i != 1 and self.xs[floor(i / 2)] > self.xs[i]:
            self.xs[floor(i / 2)], self.xs[i] = self.xs[i], self.xs[floor(i / 2)]
            i = floor(i / 2)
        self.i += 1

    def root(self):
        return self.xs[1]

xs = Heap()
print(xs)
for x in [12, 15, 14, 21, 1, 10]:
    xs.insert(x)
    print(xs)

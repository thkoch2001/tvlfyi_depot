from random import randint

class Box(object):
    def __init__(self, w, h, d):
        self.width  = w
        self.depth  = d
        self.height = h

    def __repr__(self):
        return "{}x{}x{}".format(self.width, self.depth, self.height)

    def lt(self, b):
        return all([
            self.width  < b.width,
            self.height < b.height,
            self.depth  < b.depth,
        ])

    def gt(self, b):
        return all([
            self.width  > b.width,
            self.height > b.height,
            self.depth  > b.depth,
        ])

def random_box():
    return Box(
        randint(1, 10),
        randint(1, 10),
        randint(1, 10),
    )

xs = [random_box() for _ in range(5)]

def highest_stack(xs, cache={}):
    if not xs:
        return 0
    heights = []
    for i in range(len(xs)):
        x, rest = xs[i], xs[0:i] + xs[i+1:]
        if cache and x in cache:
            height = cache[x]
        else:
            height = x.height + highest_stack([b for b in rest if x.gt(b)], cache)
            cache[x] = height
        heights += [height]
    return max(heights)

print(xs)
print(highest_stack(xs))

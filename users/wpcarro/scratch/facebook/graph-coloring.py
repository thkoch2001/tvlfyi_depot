from collections import deque

class Palette(object):
    def __init__(self, n):
        self.i = 0
        self.colors = list(range(n))

    def get(self):
        return self.colors[self.i]

    def advance(self):
        self.i += 1 % len(self.colors)

class GraphNode(object):
    def __init__(self, label):
        self.label = label
        self.neighbors = set()
        self.color = None

    def __repr__(self):
        result = []
        xs = deque()
        xs.append(self)
        seen = set()
        while xs:
            node = xs.popleft()
            result.append('{} ({})'.format(node.label, str(node.color)))
            for c in node.neighbors:
                if c.label not in seen:
                    xs.append(c)
                    seen.add(node.label)
        return ', '.join(result)

def color_graph(graph, d):
    seen = set()
    start = graph
    xs = deque()
    palette = Palette(d + 1)
    xs.append((start, palette.get()))
    while xs:
        x, color = xs.popleft()
        x.color = color
        for c in x.neighbors:
            if c.label not in seen:
                palette.advance()
                xs.append((c, palette.get()))
                seen.add(x.label)

a = GraphNode('a')
b = GraphNode('b')
c = GraphNode('c')

a.neighbors.add(b)
b.neighbors.add(a)
b.neighbors.add(c)
c.neighbors.add(b)

print(a)
color_graph(a, 3)
print(a)

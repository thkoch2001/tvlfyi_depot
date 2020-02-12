from collections import deque
from heapq import heappush, heappop
from fixtures import weighted_graph


def put(t, x, xs):
    if t == 'stack':
        return xs.append(x)
    if t == 'queue':
        return xs.append(x)
    if t == 'priority':
        return heappush(xs, x)


def pop(t, xs):
    if t == 'stack':
        return xs.pop()
    if t == 'queue':
        return xs.popleft()
    if t == 'priority':
        return heappop(xs)


# shortest_path :: Vertex -> Vertex -> Graph -> [Vertex]
def shortest_path(a, b, g):
    """Returns the shortest path from vertex a to vertex b in graph g."""
    t = 'priority'
    xs = []
    seen = set()
    # Map(Weight, [Vertex])
    m = {}

    put(t, (0, [a], a), xs)

    while xs:
        w0, path, v = pop(t, xs)

        seen.add(v)
        if v == b:
            m[w0] = path
        for w1, x in g.get(v):
            if x not in seen:
                put(t, (w0 + w1, path + [x], x), xs)

    return m


print(shortest_path('a', 'f', graph_a))

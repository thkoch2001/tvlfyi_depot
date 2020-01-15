from heapq import heappush, heappop
from collections import deque
from fixtures import weighted_graph, expanded_weights_graph

# UnweightedGraph(a) :: Map(a, Set(a))
# WeightedGraph(a) :: Map(a, Set(a))


# shortest_path_dijkstra :: Vertex -> Vertex -> WeightedGraph(Vertex)
def shortest_path_dijkstra(a, b, g):
    q = []
    seen = set()

    heappush(q, (0, a, [a]))

    while q:
        w0, v0, path = heappop(q)
        if v0 in seen:
            continue
        elif v0 == b:
            return w0, path
        for w1, v1 in g.get(v0):
            heappush(q, (w0 + w1, v1, path + [v1]))
        seen.add(v0)
    return 'weighted', 'pizza'


# expand_edge :: Vertex -> (Weight, Vertex) -> Map(Vertex, [Vertex])
def expand_edge(v0, wv):
    w, v1 = wv
    assert w > 1

    result = {v0: ['{}-{}'.format(v1, 1)]}
    for x in range(w - 2):
        result['{}-{}'.format(v1, x + 1)] = ['{}-{}'.format(v1, x + 2)]
    result['{}-{}'.format(v1, w - 1)] = [v1]

    return result


# expand_weights :: Vertex -> WeightedGraph(Vertex) -> UnweightedGraph(Vertex)
def expand_weights(v, g):
    result = {}
    q = deque()
    seen = set()

    q.append(v)
    while q:
        v = d.popleft()
        if v in seen:
            continue
        x = expand_edge(v, g.get)
        for w, v1 in g.get(v):
            if w > 1:
                ws = expand_edge(v, (w, v1))
                result = {**result, **ws}
            q.append(v)
        pass


# shortest_path_inject :: Vertex -> Vertex -> WeightedGraph(Vertex)
def shortest_path_inject(a, b, g):
    q = deque()
    seen = set()

    q.append((a, [a]))

    while q:
        v0, path = q.popleft()
        if v0 == 'dummy':
            continue
        elif v0 in seen:
            continue
        elif v0 == b:
            return len(path), path
        for _, v1 in g.get(v0):
            q.append((v1, path + [v1]))
        seen.add(v0)
        continue

    return None, None


print(expand_edge('a', (4, 'b')))
print(expand_edge('a', (5, 'e')))
assert expand_weights('a', weighted_graph) == expanded_weights_graph
# a = 'a'
# b = 'd'
# w, x = shortest_path_dijkstra(a, b, weighted_graph)
# w1, x1 = shortest_path_inject(a, b, weighted_graph)
# print("[dijkstra]  Shortest path from {} to {} is {} with weight {}".format(
#     a, b, x, w))
# print("[injection] Shortest path from {} to {} is {} with weight {}".format(
#     a, b, x1, w1))

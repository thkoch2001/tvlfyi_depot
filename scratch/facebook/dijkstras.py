from heapq import heappush, heappop
import random

# Dijkstra's algorithm will traverse a directed graph with weighted edges. If
# the edges aren't weighted, we can pretend that each edges weighs 1. The
# algorithm will find the shortest path between points A and B.

def dijkstra(a, b, graph):
    h = []
    seen = set()
    heappush(h, (0, a, [a], []))
    while h:
        km, x, path, steps = heappop(h)

        if x == b:
            for a, b, d in steps:
                print("{} -> {} => {}".format(a, b, d))
            return path, km

        seen.add(x)
        for c, dist in graph[x]:
            if c not in seen:
                heappush(h, (km + dist, c, path + [c], steps + [(x, c, dist)]))
    return [], float('inf')

graph = {
    1: [(3, 9), (2, 7), (6, 14)],
    2: [(1, 7), (3, 10), (4, 15)],
    3: [(1, 9), (6, 2), (4, 11), (2, 10)],
    4: [(5, 6), (2, 15), (3, 11)],
    5: [(4, 6), (6, 9)],
    6: [(5, 9), (3, 2), (1, 14)],
}

beg = random.choice(list(graph.keys()))
end = random.choice(list(graph.keys()))
print("Searching for the shortest path from {} -> {}".format(beg, end))
print(dijkstra(beg, end, graph))

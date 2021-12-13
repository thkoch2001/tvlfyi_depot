from heapq import heappush, heappop
import random

def to_vertex_list(graph):
    result = {}
    for a, b, kg in graph:
        if a in result:
            result[a].append((b, kg))
        else:
            result[a] = [(b, kg)]
        if b in result:
            result[b].append((a, kg))
        else:
            result[b] = [(a, kg)]
    return result

def mst(graph):
    graph = to_vertex_list(graph)
    beg = random.choice(list(graph.keys()))
    h = []
    result = []
    seen = set()
    for c, kg in graph[beg]:
        heappush(h, (kg, beg, c))
    while h:
        kg, beg, end = heappop(h)
        # detect cycles
        if end in seen:
            continue
        # use the edge
        seen.add(beg)
        seen.add(end)
        result.append((beg, end))
        for c, kg in graph[end]:
            heappush(h, (kg, end, c))
    return result

graphs = [
    [
        ('A', 'B', 7),
        ('A', 'D', 5),
        ('B', 'D', 9),
        ('E', 'D', 15),
        ('F', 'D', 6),
        ('F', 'G', 11),
        ('F', 'E', 8),
        ('G', 'E', 9),
        ('C', 'E', 5),
        ('B', 'E', 7),
        ('B', 'C', 8),
    ],
    [
        ('A', 'B', 4),
        ('A', 'C', 8),
        ('B', 'C', 11),
        ('B', 'E', 8),
        ('C', 'D', 7),
        ('C', 'F', 1),
        ('D', 'E', 2),
        ('D', 'F', 6),
        ('E', 'G', 7),
        ('E', 'H', 4),
        ('F', 'H', 2),
        ('G', 'H', 14),
        ('G', 'I', 9),
        ('H', 'I', 10),
    ],
]

for graph in graphs:
    print(mst(graph))

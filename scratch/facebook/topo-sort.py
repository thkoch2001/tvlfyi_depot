import random
from heapq import heappush, heappop
from collections import deque

# A topological sort returns the vertices of a graph sorted in an ascending
# order by the number of incoming edges each vertex has.
#
# A few algorithms for solving this exist, and at the time of this writing, I
# know none. I'm going to focus on two:
#   1. Kahn's
#   2. DFS (TODO)

def count_in_edges(graph):
    result = {k: 0 for k in graph.keys()}
    for xs in graph.values():
        for x in xs:
            result[x] += 1
    return result

# Kahn's algorithm for returning a topological sorting of the vertices in
# `graph`.
def kahns_sort(graph):
    result = []
    q = deque()
    in_edges = count_in_edges(graph)
    for x in [k for k, v in in_edges.items() if v == 0]:
        q.append(x)
    while q:
        x = q.popleft()
        result.append(x)
        for c in graph[x]:
            in_edges[c] -= 1
            if in_edges[c] == 0:
                q.append(c)
    return result

graphs = [
    {
        0: [],
        1: [],
        2: [3],
        3: [1],
        4: [0, 1],
        5: [0, 2],
    },
    {
        'A': ['C', 'D'],
        'B': ['D', 'E'],
        'C': [],
        'D': ['F', 'G'],
        'E': [],
        'F': [],
        'G': ['I'],
        'H': ['I'],
        'I': [],
    }
]

print("--- Kahn's --- ")
for graph in graphs:
    print(kahns_sort(graph))

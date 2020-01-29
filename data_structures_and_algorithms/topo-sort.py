from fixtures import unweighted_digraph
from collections import deque

# vertices_no_in_edges :: UnweightedDigraph -> Set(Vertex)
def vertices_no_in_edges(g):
    """Return the vertices in graph `g` with no in-edges."""
    result = set()
    vertices = set(g.keys())
    for neighbors in g.values():
        result = result.union(neighbors)
    return vertices ^ result

# topo_sort :: UnweightedDigraph -> List(Vertex)
def topo_sort(g):
    q = deque()
    seen = set()
    result = []
    for x in vertices_no_in_edges(g):
        q.append(x)
    while q:
        vertex = q.popleft()
        if vertex in seen:
            continue
        result.append(vertex)
        neighbors = g.get(vertex)
        for x in g.get(vertex):
            q.append(x)
        seen.add(vertex)
    return result

print(topo_sort(unweighted_digraph))

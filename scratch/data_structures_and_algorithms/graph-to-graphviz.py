from graphviz import Digraph
from collections import deque
from fixtures import weighted_graph

# There are three ways to model a graph:
# 1. Edge list: [(Vertex, Vertex)]
# 2. Neighbors table: Map(Vertex, [Vertex])
# 3. Adjacency matrix: [[Boolean]]
#
# The following graph is a neighbors table.


# to_graphviz :: Vertex -> Map(Vertex, [(Vertex, Weight)]) -> String
def to_graphviz(start, g):
    """Compiles the graph into GraphViz."""
    d = Digraph()
    q = deque()
    seen = set()

    q.append(start)

    while q:
        v = q.popleft()
        if v in seen:
            continue
        d.node(v, label=v)

        for w, x in g[v]:
            d.edge(v, x, label=str(w))
            q.append(x)
        seen.add(v)

    return d.source


with open('/tmp/test.gv', 'w') as f:
    src = to_graphviz('a', weighted_graph)
    f.write(src)
    print('/tmp/test.gv created!')

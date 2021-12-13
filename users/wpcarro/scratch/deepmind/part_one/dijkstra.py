# Doing a practice implementation of Dijkstra's algorithm: a priority-first
# search.
from heapq import heappush, heappop


class Node(object):
    def __init__(self, value, children):
        self.value = value
        self.children = children


def shortest_path(a, b):
    """Return the shortest path from `a` to `b`."""
    q = []
    seen = set()
    heappush((a.value, a, [a]), q)

    while q:
        d, node, path = heappop(q)
        if node == b:
            return path
        seen.add(node)
        for child in node.children:
            if child not in seen:
                heappush((d + child.value, child, path + [child]), q)
    raise Exception("Path between nodes A and B does not exist.")

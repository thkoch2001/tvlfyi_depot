from heapq import heappush, heappop
import random

def shortest_path(a, b, graph):
    seen = set()
    h = []
    heappush(h, (0, a, [a]))
    while h:
        km, x, path = heappop(h)
        if x == b:
            return path
        for c in graph[x]:
            if c not in seen:
                heappush(h, (km + 1, c, path + [c]))
    raise Exception("We were unable to find a path from {} to {}".format(a, b))

graph = {
    'Min'     : ['William', 'Jayden', 'Omar'],
    'William' : ['Min', 'Noam'],
    'Jayden'  : ['Min', 'Amelia', 'Ren', 'Noam'],
    'Ren'     : ['Jayden', 'Omar'],
    'Amelia'  : ['Jayden', 'Adam', 'Miguel'],
    'Adam'    : ['Amelia', 'Miguel', 'Sofia', 'Lucas'],
    'Miguel'  : ['Amelia', 'Adam', 'Liam', 'Nathan'],
    'Noam'    : ['Nathan', 'Jayden', 'William'],
    'Omar'    : ['Ren', 'Min', 'Scott'],
    'Liam'    : ['Ren'],
    'Nathan'  : ['Noam'],
    'Scott'   : [],
}

result = shortest_path('Jayden', 'Adam', graph)
print(result)
assert result == ['Jayden', 'Amelia', 'Adam']
print('Success!')

beg = random.choice(list(graph.keys()))
end = random.choice(list(graph.keys()))
print("Attempting to find the shortest path between {} and {}".format(beg, end))
print(shortest_path(beg, end, graph))

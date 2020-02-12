# Using this module to store commonly used, but annoying to create, data
# structures for my test inputs.
#
# Use like:
# from fixtures import graph_a

################################################################################
# Constants
################################################################################

edge_list = [
    ('a', 'b'),
    ('a', 'c'),
    ('a', 'e'),
    ('b', 'c'),
    ('b', 'd'),
    ('c', 'e'),
    ('d', 'f'),
    ('e', 'd'),
    ('e', 'f'),
]

unweighted_graph = {
    'a': {'b', 'c', 'e'},
    'b': {'c', 'd'},
    'c': {'e'},
    'd': {'f'},
    'e': {'d', 'f'},
    'f': set(),
}

adjacencies = {
    'a': {
        'a': False,
        'b': False
    },
    'a': [],
    'a': [],
    'a': [],
    'a': [],
    'a': [],
    'a': [],
}

weighted_graph = {
    'a': {(4, 'b'), (2, 'c'), (4, 'e')},
    'b': {(5, 'c'), (10, 'd')},
    'c': {(3, 'e')},
    'd': {(11, 'f')},
    'e': {(4, 'd'), (5, 'f')},
    'f': set(),
}

# This is `weighted_graph` with each of its weighted edges "expanded".
expanded_weights_graph = {
    'a': ['b-1', 'c-1', 'e-1'],
    'b-1': ['b-2'],
    'b-2': ['b-3'],
    'b-3': ['b'],
    'c-1': ['c'],
    'e-1': ['e-2'],
    'e-2': ['e-3'],
    'e-3': ['e'],
    # and so on...
}

unweighted_digraph = {
    '5': {'2', '0'},
    '4': {'0', '1'},
    '3': {'1'},
    '2': {'3'},
    '1': set(),
    '0': set(),
}

################################################################################
# Functions
################################################################################


def vertices(xs):
    result = set()
    for a, b in xs:
        result.add(a)
        result.add(b)
    return result


def edges_to_neighbors(xs):
    result = {v: set() for v in vertices(xs)}
    for a, b in xs:
        result[a].add(b)
    return result


def neighbors_to_edges(xs):
    result = []
    for k, ys in xs.items():
        for y in ys:
            result.append((k, y))
    return result


def edges_to_adjacencies(xs):
    return xs


# Skipping handling adjacencies because I cannot think of a reasonable use-case
# for it when the vertex labels are items other than integers. I can think of
# ways of handling this, but none excite me.

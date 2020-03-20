import unittest
from collections import deque
from heapq import heappush, heappop


################################################################################
# InterviewCake.com
################################################################################
# construct_path :: Map String String -> String -> String -> [String]
def construct_path(paths, beg, end):
    """
    Reconstruct the path from `beg` to `end`.
    """
    result = []
    current = end

    print(paths)
    print(beg, end)
    print('-----')
    while current:
        result.append(current)
        current = paths[current]

    result.reverse()
    return result


def get_path_ic(graph, beg, end):
    """
    InterviewCake uses a dictionary and back-tracking to store and reconstruct
    the path instead of storing the path as state on each node.
    This reduces the memory costs. See get_path_bft for an example of this less
    optimal solution.
    """
    if beg not in graph:
        raise Exception('Origin node absent from graph.')

    if end not in graph:
        raise Exception('Destination node absent from graph.')

    q = deque()
    q.append(beg)
    paths = {beg: None}

    while q:
        node = q.popleft()

        if node == end:
            print(graph)
            return construct_path(paths, beg, end)

        for x in graph[node]:
            if x not in paths:
                paths[x] = node
                q.append(x)

    return None


################################################################################
# Per-node state
################################################################################
def get_path_bft(graph, beg, end):
    """
    Here we find the shortest path from `beg` to `end` in `graph` by doing a BFT
    from beg to end and storing the path state alongside each node in the queue.
    """
    if beg not in graph:
        raise Exception('Origin node absent from graph.')

    if end not in graph:
        raise Exception('Destination node absent from graph.')

    q = deque()
    seen = set()
    q.append([beg])

    while q:
        path = q.popleft()
        node = path[-1]
        seen.add(node)

        if node == end:
            return path

        for x in graph[node]:
            if x not in seen:
                q.append(path + [x])


################################################################################
# Dijkstra's Algorithm
################################################################################
def get_path(graph, beg, end):
    """
    Here we find the shortest path using Dijkstra's algorithm, which is my
    favorite solution.
    """
    if beg not in graph:
        raise Exception(
            'The origin node, {}, is not present in the graph'.format(beg))

    if end not in graph:
        raise Exception(
            'The origin node, {}, is not present in the graph'.format(end))

    q = []
    seen = set()
    heappush(q, (1, [beg]))

    while q:
        weight, path = heappop(q)
        node = path[-1]
        seen.add(node)

        if node == end:
            return path

        for x in graph[node]:
            if x not in seen:
                heappush(q, (weight + 1, path + [x]))

    return None


# Tests
class Test(unittest.TestCase):
    def setUp(self):
        self.graph = {
            'a': ['b', 'c', 'd'],
            'b': ['a', 'd'],
            'c': ['a', 'e'],
            'd': ['b', 'a'],
            'e': ['c'],
            'f': ['g'],
            'g': ['f'],
        }

    def test_two_hop_path_1(self):
        actual = get_path(self.graph, 'a', 'e')
        expected = ['a', 'c', 'e']
        self.assertEqual(actual, expected)

    def test_two_hop_path_2(self):
        actual = get_path(self.graph, 'd', 'c')
        expected = ['d', 'a', 'c']
        self.assertEqual(actual, expected)

    def test_one_hop_path_1(self):
        actual = get_path(self.graph, 'a', 'c')
        expected = ['a', 'c']
        self.assertEqual(actual, expected)

    def test_one_hop_path_2(self):
        actual = get_path(self.graph, 'f', 'g')
        expected = ['f', 'g']
        self.assertEqual(actual, expected)

    def test_one_hop_path_3(self):
        actual = get_path(self.graph, 'g', 'f')
        expected = ['g', 'f']
        self.assertEqual(actual, expected)

    def test_zero_hop_path(self):
        actual = get_path(self.graph, 'a', 'a')
        expected = ['a']
        self.assertEqual(actual, expected)

    def test_no_path(self):
        actual = get_path(self.graph, 'a', 'f')
        expected = None
        self.assertEqual(actual, expected)

    def test_start_node_not_present(self):
        with self.assertRaises(Exception):
            get_path(self.graph, 'h', 'a')

    def test_end_node_not_present(self):
        with self.assertRaises(Exception):
            get_path(self.graph, 'a', 'h')


unittest.main(verbosity=2)

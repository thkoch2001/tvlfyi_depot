import random
from collections import deque

def exists(pattern, tree):
    """
    Return true if `pattern` exists in `tree`.
    """
    if len(pattern) == 0:
        return True
    if len(pattern) == 1:
        for branch in tree:
            if branch[0] == pattern[0]:
                return True
        return False
    for branch in tree:
        if branch[0] == pattern[0]:
            return exists(pattern[1:], branch[1])
    return False

# Branch :: (Char, [Branch])
# SuffixTree :: [Branch]

def suffix_tree(xs):
    """
    Create a suffix tree from the input string, `xs`.
    """
    root = []
    for i in range(len(xs)):
        curr = xs[i:]
        parent = root
        for c1 in curr:
            grafted = False
            for c2, children in parent:
                if c1 == c2:
                    grafted = True
                    parent = children
            if grafted:
                continue
            else:
                children = []
                child = (c1, children)
                parent.append(child)
                parent = children
    return root

def suffix_tree(x):
    """
    Creates a suffix from the input string, `x`. This implementation uses a
    stack.
    """
    result = [None, []]
    q = deque()
    for i in range(len(x)):
        q.append((result, x[i:]))
    while q:
        parent, x = q.popleft()
        s = []
        s.append((parent, x))
        while s:
            parent, x = s.pop()
            if not x:
                continue
            c, rest = x[0], x[1:]
            grafted = False
            for child in parent[1]:
                if c == child[0]:
                    s.append((child, rest))
                    grafted = True
            if not grafted:
                child = [c, []]
                parent[1].append(child)
                s.append((child, rest))
    return result[1]

################################################################################
# Tests
################################################################################

x = random.choice(["burrito", "pizza", "guacamole"])
tree = suffix_tree(x)
for branch in tree:
    print(branch)

for _ in range(3):
    n = len(x)
    i, j = random.randint(0, n), random.randint(0, n)
    pattern = x[min(i, j):max(i, j)]
    print("Checking \"{}\" for \"{}\" ...".format(x, pattern))
    print("Result: {}".format(exists(pattern, tree)))
    pattern = random.choice(["foo", "bar", "baz"])
    print("Checking \"{}\" for \"{}\" ...".format(x, pattern))
    print("Result: {}".format(exists(pattern, tree)))
    print()

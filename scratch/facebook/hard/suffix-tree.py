import random

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

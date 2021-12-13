import random

def find_pairs(xs, n):
    """
    Return all pairs of integers in `xs` that sum to `n`.
    """
    seeking = set()
    result = set()
    for x in xs:
        if x in seeking:
            result.add((n - x, x))
        else:
            seeking.add(n - x)
    return result

xs = [random.randint(1, 10) for _ in range(10)]
n = random.randint(1, 10) + random.randint(1, 10)
print("Seeking all pairs in {} for {}...".format(xs, n))
print(find_pairs(xs, n))

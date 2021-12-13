import random

def find_duplicate(xs):
    print(xs)
    # entry point in our cycle is the duplicate
    i = xs[0]
    j = xs[xs[0]]
    while i != j:
        print(i, xs[i], j, xs[j])
        i = xs[i]
        j = xs[xs[j]]
    # detect cycle
    j = 0
    while i != j:
        i = xs[i]
        j = xs[j]
    return xs[i]

n = random.randint(5, 10)
xs = [random.randint(0, n - 1) for _ in range(n)]
result = find_duplicate(xs)
print(xs, result)

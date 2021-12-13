import random

def shuffle(xs):
    n = len(xs)
    for i in range(n):
        j = random.randint(i, n - 1)
        xs[i], xs[j] = xs[j], xs[i]

import random

def find_duplicate(xs):
    mini, maxi, acc = xs[0], xs[0], xs[0]
    for i in range(1, len(xs)):
        mini = min(mini, xs[i])
        maxi = max(maxi, xs[i])
        acc = acc ^ xs[i]
    mask = mini
    for i in range(mini + 1, maxi + 1):
        mask = mask ^ i
    return mask ^ acc

xs = [5, 3, 4, 1, 5, 2]
print(xs)
result = find_duplicate(xs)
print(result)

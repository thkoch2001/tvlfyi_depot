from random import randint

def get_random(i, j):
    return randint(i, j)

def shuffle(xs):
    for i in range(len(xs)):
        j = get_random(i, len(xs) - 1)
        xs[i], xs[j] = xs[j], xs[i]

xs = list(range(1, 53))
print(xs)
assert len(set(xs)) == 52
shuffle(xs)
assert len(set(xs)) == 52
print(xs)
print("Success!")

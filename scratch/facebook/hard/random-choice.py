import random

# This class of problems is known as "resevoir sampling".
def choose_a(m, xs):
    """
    Randomly choose `m` elements from `xs`.
    This algorithm runs in linear time with respect to the size of `xs`.
    """
    result = xs[:m]
    for i in range(m, len(xs)):
        j = random.randint(0, i)
        if j < m:
            result[j] = xs[i]
    return result

def choose_b(m, xs):
    """
    This algorithm, which copies `xs`, which runs in linear time, and then
    shuffles the copies, which also runs in linear time, achieves the same
    result as `choose_a` and both run in linear time.

    `choose_a` is still preferable since it has a coefficient of one, while this
    version has a coefficient of two because it copies + shuffles.
    """
    ys = xs[:]
    random.shuffle(ys)
    return ys[:m]

# ROYGBIV
xs = [
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'indigo',
    'violet',
]
print(choose_b(3, xs))

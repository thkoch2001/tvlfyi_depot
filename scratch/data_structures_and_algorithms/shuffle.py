import random


def get_random(floor, ceiling):
    return random.randrange(floor, ceiling + 1)


# shuffle_in_place :: [a] -> IO ()
def shuffle_in_place(xs):
    """Fisher-Yates algorithm. Notice that shuffling here is the same as
    selecting a random permutation of the input set, `xs`."""
    n = len(xs) - 1
    for i in range(len(xs)):
        r = get_random(i, n)
        xs[i], xs[r] = xs[r], xs[i]
    return xs


# shuffle :: [a] -> [a]
def shuffle_not_in_place(xs):
    result = []

    while xs:
        i = get_random(0, len(xs) - 1)
        x = xs.pop(i)
        result.append(x)

    return result


xs = [x for x in range(9)]
print(xs)
# print(shuffle_not_in_place(xs))
print(shuffle_in_place(xs[:]))

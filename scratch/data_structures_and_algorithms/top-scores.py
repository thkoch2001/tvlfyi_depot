from collections import deque

# list:
# array:
# vector:
# bit-{array,vector}:


def sort(xs, highest):
    v = [0] * (highest + 1)
    result = deque()

    for x in xs:
        v[x] += 1

    for i, x in enumerate(v):
        if x > 0:
            result.appendleft(i)

    return list(result)


assert sort([37, 89, 41, 100, 65, 91, 53],
            100) == [100, 91, 89, 65, 53, 41, 37]
print("Tests pass!")

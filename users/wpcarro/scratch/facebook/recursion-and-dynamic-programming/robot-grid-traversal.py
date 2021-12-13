import random

def factorial(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result

def travel(a, b):
    if a == b:
        return 1

    ax, ay = a
    bx, by = b
    if ax > bx or ay > by:
        return 0

    return sum([travel((ax + 1, ay), b), travel((ax, ay + 1), b)])

def travel_compute(a, b):
    bx, by = b
    return int(factorial(bx + by) / (factorial(bx) * factorial(by)))

a = (0, 0)
b = (random.randint(1, 10), random.randint(1, 10))
print("Travelling to {}, {}".format(b[0], b[1]))
print(travel(a, b))
print(travel_compute(a, b))

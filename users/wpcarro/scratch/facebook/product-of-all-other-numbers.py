from random import randint
from math import floor

# loop {forwards, backwards, up, down}
# through a table of values, [[a]].

def product(xs):
    n = len(xs)
    lhs = [1] * (n + 1)
    for i in range(1, n):
        lhs[i] = lhs[i - 1] * xs[i - 1]
    rhs = [1] * (n + 1)
    for i in range(n - 1, 0, -1):
        rhs[i] = rhs[i + 1] * xs[i]
    result = []
    for i in range(n):
        result.append(lhs[i] * rhs[i + 1])
    return result

def computed_expected(xs):
    product = 1
    for x in xs:
        product *= x
    return [floor(product / x) for x in xs]

xs = [randint(1, 10) for _ in range(5)]
expected = computed_expected(xs)
result = product(xs)
print(xs, result, expected)
assert result == expected
print("Success!")

print(product([2, 4, 3, 10, 5]))

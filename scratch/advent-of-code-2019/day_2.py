from itertools import product

x = [
    1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 10, 19, 1, 6, 19, 23,
    2, 23, 6, 27, 2, 6, 27, 31, 2, 13, 31, 35, 1, 10, 35, 39, 2, 39, 13, 43, 1,
    43, 13, 47, 1, 6, 47, 51, 1, 10, 51, 55, 2, 55, 6, 59, 1, 5, 59, 63, 2, 9,
    63, 67, 1, 6, 67, 71, 2, 9, 71, 75, 1, 6, 75, 79, 2, 79, 13, 83, 1, 83, 10,
    87, 1, 13, 87, 91, 1, 91, 10, 95, 2, 9, 95, 99, 1, 5, 99, 103, 2, 10, 103,
    107, 1, 107, 2, 111, 1, 111, 5, 0, 99, 2, 14, 0, 0
]


def interpret(i, x):
    op, a, b, out = x[i + 0], x[i + 1], x[i + 2], x[i + 3]
    if op == 1:
        x[out] = x[a] + x[b]
        return interpret(i + 4, x)
    elif op == 2:
        x[out] = x[a] * x[b]
        return interpret(i + 4, x)
    elif op == 99:
        return x
    else:
        raise Exception('Unsupported opcode: {}. {}, {}'.format(op, a, b))


for a, b in product(range(100), range(100)):
    y = x[:]
    y[1] = a
    y[2] = b
    if interpret(0, y)[0] == 19690720:
        print(100 * a + b)

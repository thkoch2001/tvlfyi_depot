def test(x, i):
    return x & (1 << i) != 0


def set(x, i):
    return x | (1 << i)


def clear(x, i):
    return x & ~(1 << i)


def toggle(x, i):
    if test(x, i):
        return clear(x, i)
    else:
        return set(x, i)


def test_single(x):
    if x == 0:
        return False
    else:
        return x & (x - 1) == 0


print(test(0b1010, 3))
print('{0:b}'.format(set(0b1010, 1)))
print('{0:b}'.format(clear(0b1010, 1)))
print('{0:b}'.format(toggle(0b1010, 2)))
print(test_single(0b1010))
print(test_single(0b1000))

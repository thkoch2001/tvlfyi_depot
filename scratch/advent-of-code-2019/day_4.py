import re

start = 134792
end = 675810


def satisfies(x):
    x = str(x)
    result = False
    double, not_decreasing = False, False

    # double and *only* double exists
    for i in range(len(x) - 1):
        # double and left-of-a  is BOL or !x
        #        and right-of-b is EOL or !x
        a, b = x[i], x[i + 1]
        bol = i - 1 < 0
        eol = i + 2 >= len(x)
        if a == b and (bol or x[i - 1] != a) and (eol or x[i + 2] != a):
            double = True
            break

    # not_decreasing
    prev = int(x[0])
    for a in x[1:]:
        a = int(a)
        if prev > a:
            return False
        prev = a
    not_decreasing = True

    return double and not_decreasing


print(len([x for x in range(start, end + 1) if satisfies(x)]))

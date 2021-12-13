def advance(position, xs):
    """
    Return the next element in `xs` pointed to by the current `position`.
    """
    return xs[position - 1]

def find_duplicate(xs):
    """
    Find the duplicate integer in the list, `xs`.
    """
    beg = xs[-1]
    a = beg
    b = advance(a, xs)
    # Find the first element of the cycle
    cycle_beg = None
    while a != b:
        cycle_beg = a
        a = advance(a, xs)
        b = advance(b, xs)
        b = advance(b, xs)
    # The duplicate element is the element before the `cycle_beg`
    a = beg
    result = None
    while a != cycle_beg:
        result = a
        a = advance(a, xs)
    return result

def find_duplicate(xs):
    """
    This is the solution that InterviewCake.com suggests.
    """
    # find length of the cycle
    beg = xs[-1]
    a = beg
    for _ in range(len(xs)):
        a = advance(a, xs)
    element = a
    a = advance(a, xs)
    n = 1
    while a != element:
        a = advance(a, xs)
        n += 1
    # find the first element in the cycle
    a, b = beg, beg
    for _ in range(n):
        b = advance(b, xs)
    while a != b:
        a = advance(a, xs)
        b = advance(b, xs)
    return a

xs = [2, 3, 1, 3]
result = find_duplicate(xs)
print(result)
assert result == 3
print("Success!")

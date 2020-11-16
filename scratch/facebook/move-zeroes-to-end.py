from collections import deque

def move_zeroes_to_end_quadratic(xs):
    """
    This solution is suboptimal. It runs in quadratic time, and it uses constant
    space.
    """
    i = 0
    while i < len(xs) - 1:
        if xs[i] == 0:
            j = i + 1
            while j < len(xs) and xs[j] == 0:
                j += 1
            if j >= len(xs):
                break
            xs[i], xs[j] = xs[j], xs[i]
        i += 1

def move_zeroes_to_end_linear(xs):
    """
    This solution is clever. It runs in linear time proportionate to the number
    of elements in `xs`, and has linear space proportionate to the number of
    consecutive zeroes in `xs`.
    """
    q = deque()
    for i in range(len(xs)):
        if xs[i] == 0:
            q.append(i)
        else:
            if q:
                j = q.popleft()
                xs[i], xs[j] = xs[j], xs[i]
                q.append(i)

def move_zeroes_to_end_linear_constant_space(xs):
    """
    This is the optimal solution. It runs in linear time and uses constant
    space.
    """
    i = 0
    for j in range(len(xs)):
        if xs[j] != 0:
            xs[i], xs[j] = xs[j], xs[i]
            i += 1


################################################################################
# Tests
################################################################################

xss = [
    [1, 2, 0, 3, 4, 0, 0, 5, 0],
    [0, 1, 2, 0, 3, 4],
    [0, 0],
]

f = move_zeroes_to_end_linear_constant_space

for xs in xss:
    print(xs)
    f(xs)
    print(xs)

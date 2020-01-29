


# Write a function with the following type signature:L
# equal? :: String -> String -> Bool
#
# Determine equality between two inputs with backspace characters encoded as
# "<".

################################################################################
# Solution 1
################################################################################

# from collections import deque

# def equal(a, b):
#     sa = deque()
#     sb = deque()

#     for c in a:
#         if c == '<':
#             sa.pop()
#         else:
#             sa.append(c)

#     for c in b:
#         if c == '<':
#             sb.pop()
#         else:
#             sb.append(c)

#     return sa == sb

################################################################################
# Solution 2
################################################################################

def handle_dels(num_dels, i, xs):
    if i < 0:
        return -1

    while xs[i] == '<':
        num_dels += 1
        i -= 1

    while num_dels > 0 and xs[i] != '<':
        num_dels -= 1
        i -= 1

    if xs[i] == '<':
        return handle_dels(num_dels, i, xs)
    else:
        return i

def update_index(i, xs):
    # TODO: Indexing into non-available parts of a string.
    if xs[i] != '<' and xs[i - 1] != '<':
        return i - 1

    elif xs[i - 1] == '<':
        return handle_dels(0, i - 1, xs)

def equal(a, b):
    ia = len(a) - 1
    ib = len(b) - 1

    while ia >= 0 and ib >= 0:
        if a[ia] != b[ib]:
            return False
        ia = update_index(ia, a)
        ib = update_index(ib, b)

    if ia != 0:
        return update_index(ia, a) <= -1
    if ib != 0:
        return update_index(ib, b) <= -1

    return True

# 0, 1, 1, 2, 3, 5
def fib(n):
    if n < 0:
        raise Exception("Need to supply an index that's >= 0. Not: {}".format(n))
    elif n in {0, 1}:
        return n
    state = [0, 1]
    for i in range(1, n):
        state[0], state[1] = state[1], state[0] + state[1]
    return state[-1]

for i in range(10):
    print("fib({}) => {}".format(i, fib(i)))

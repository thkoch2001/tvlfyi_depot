def move_zeroes_to_end(xs):
    n_zeroes = 0
    for x in xs:
        if x == 0:
            n_zeroes += 1

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
    # add zeroes to the end
    for i in range(n_zeroes):
        xs[len(xs) - 1 - i] = 0

xs = [1, 2, 0, 3, 4, 0, 0, 5, 0]
print(xs)
move_zeroes_to_end(xs)
assert xs == [1, 2, 3, 4, 5, 0, 0, 0, 0]
print(xs)
print("Success!")

def hi_product(xs):
    lowest_one, highest_one = min(xs[0], xs[1]), max(xs[0], xs[1])
    lowest_two, highest_two = xs[0] * xs[1], xs[0] * xs[1]
    highest = float('-inf')
    for x in xs[2:]:
        highest = max(highest, highest_two * x, lowest_two * x)
        lowest_one = min(lowest_one, x)
        highest_one = max(highest_one, x)
        lowest_two = min(lowest_two, highest_one * x, lowest_one * x)
        highest_two = max(highest_two, highest_one * x, lowest_one * x)
    return highest

xs = [([-10,-10,1,3,2], 300),
      ([1,10,-5,1,-100], 5000)]

for x, expected in xs:
    result = hi_product(x)
    print(x, result)
    assert result == expected
    print("Success!")

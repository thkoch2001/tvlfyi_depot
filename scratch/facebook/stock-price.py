def max_profit(xs):
    buy = xs[0]
    profit = xs[1] - xs[0]
    for price in xs[1:]:
        profit = max(profit, price - buy)
        buy = min(buy, price)
    return profit

xs = [([10,7,5,8,11,9], 6),
      ([10,8,7,6,5], -1)]

for x, expected in xs:
    result = max_profit(x)
    print(x, result)
    assert result == expected
    print("Success!")

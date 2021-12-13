def find_sum(xs):
    result = float('-inf')
    streak = 0
    for x in xs:
        result = max(result, streak, x)
        if streak + x <= 0:
            streak = x
        else:
            streak += x
    return result


x = [2,-8,3,-2,4,-10]
assert find_sum(x) == 5
print("Success!")

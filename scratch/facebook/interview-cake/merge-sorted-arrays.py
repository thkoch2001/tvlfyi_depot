def merge_sorted(xs, ys):
    result = []
    i = 0
    j = 0
    while i < len(xs) and j < len(ys):
        if xs[i] <= ys[j]:
            result.append(xs[i])
            i += 1
        else:
            result.append(ys[j])
            j += 1
    while i < len(xs):
        result.append(xs[i])
        i += 1
    while j < len(xs):
        result.append(ys[j])
        j += 1
    return result

################################################################################
# Tests
################################################################################

xs = [3, 4, 6, 10, 11, 15]
ys = [1, 5, 8, 12, 14, 19]
result = merge_sorted(xs, ys)
print(result)
assert len(result) == len(xs) + len(ys)
assert result == [1, 3, 4, 5, 6, 8, 10, 11, 12, 14, 15, 19]
print("Success!")

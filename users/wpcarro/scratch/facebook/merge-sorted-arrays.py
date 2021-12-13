def merge_sorted(xs, ys):
    result = []
    i, j = 0, 0

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

    while j < len(ys):
        result.append(ys[j])
        j += 1

    return result

# A
result = merge_sorted([3, 4, 6, 10, 11, 15], [1, 5, 8, 12, 14, 19])
print(result)
assert result == [1, 3, 4, 5, 6, 8, 10, 11, 12, 14, 15, 19]

# B
result = merge_sorted([], [1,2,3])
print(result)
assert result == [1,2,3]

# C
result = merge_sorted([1,2,3], [])
print(result)
assert result == [1,2,3]

# D
result = merge_sorted([], [])
print(result)
assert result == []

# Wahoo!
print("Success!")

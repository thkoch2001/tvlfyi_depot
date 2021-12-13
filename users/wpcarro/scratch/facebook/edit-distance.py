def print_grid(grid):
    result = []
    for row in grid:
        result.append(" ".join(str(c) for c in row))
    return print("\n".join(result))

def edit_distance(a, b):
    """
    Compute the "edit distance" to transform string `a` into string `b`.
    """
    grid = []
    for row in range(len(a) + 1):
        r = []
        for col in range(len(b) + 1):
            r.append(0)
        grid.append(r)

    # left-to-right
    # populate grid[0][i]
    for col in range(len(grid[0])):
        grid[0][col] = col

    # top-to-bottom
    # populate grid[i][0]
    for row in range(len(grid)):
        grid[row][0] = row

    for row in range(1, len(grid)):
        for col in range(1, len(grid[row])):
            # last characters are the same
            if a[0:row][-1] == b[0:col][-1]:
                grid[row][col] = grid[row - 1][col - 1]
            else:
                # substitution
                s = 1 + grid[row - 1][col - 1]
                # deletion
                d = 1 + grid[row - 1][col]
                # insertion
                i = 1 + grid[row][col - 1]
                grid[row][col] = min(s, d, i)
    print_grid(grid)
    return grid[-1][-1]

result = edit_distance("pizza", "pisa")
print(result)
assert result == 2
print("Success!")

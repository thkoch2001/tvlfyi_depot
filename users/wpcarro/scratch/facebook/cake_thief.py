from math import floor

def print_table(table):
    print('\n-- TABLE --')
    for row in range(len(table)):
        x = ''
        for col in range(len(table[row])):
            x += ' ' + str(table[row][col])
        print(x)

def leftover(capacity, kg):
    n = floor(capacity / kg)
    return n, capacity - (n * kg)

def init_table(num_rows, num_cols):
    table = []
    for _ in range(num_rows):
        row = []
        for _ in range(num_cols):
            row.append(0)
        table.append(row)
    return table

def get(table, row, col):
    if row < 0 or col < 0:
        return 0
    return table[row][col]

def max_haul(items, capacity):
    table = init_table(len(items), capacity)

    for row in range(len(table)):
        for col in range(len(table[row])):
            curr_capacity = col + 1
            kg, val = items[row]
            # A
            a = get(table, row - 1, col)
            # B
            n, lo = leftover(curr_capacity, kg)
            b = (val * n) + get(table, row - 1, lo - 1)
            # commit
            if kg > curr_capacity:
                table[row][col] = a
            else:
                print(n, lo)
                table[row][col] = max([a, b])
            print_table(table)
    return table[-1][-1]

# There are multiple variants of this problem:
#   1. We're allowed to take multiple of each item.
#   2. We can only take one of each item.
#   3. We can only take a fixed amount of each item.

items = [(7,160), (3,90), (2,15)]
capacity = 20
result = max_haul(items, capacity)
expected = None
print("Result: {} == Expected: {}".format(result, expected))
assert result == expected
print("Success!")

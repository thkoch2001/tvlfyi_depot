from utils import get, init_table, print_table

# This problem has a few variants:
#   - limited supply of each item
#   - unlimited supply of each item
#   - fractional amounts of each item (e.g. rice)

def max_haul(capacity, items):
    min_kg = min([kg for _, kg in items])
    max_kg = max([kg for _, kg in items])

    cols = int(max_kg / min_kg)
    fr_col_index = lambda index: min_kg * index + min_kg
    to_col_index = lambda capacity: int((capacity - min_kg) * cols / max_kg)

    table = init_table(rows=len(items), cols=cols, default=0)
    for row in range(len(table)):
        for col in range(len(table[row])):
            curr_capacity = fr_col_index(col)
            value, kg = items[row]

            if kg > curr_capacity:
                a = 0
            else:
                a = value + get(table, row - 1, to_col_index(curr_capacity - kg))

            b = get(table, row - 1, col)
            table[row][col] = max([a, b])
        print_table(table)
    return table[-1][-1]

guitar = (1500, 1)
stereo = (3000, 4)
laptop = (2000, 3)
necklace = (2000, 0.5)
items = [necklace, guitar, stereo, laptop]
capacity = 4
result = max_haul(capacity, items)
expected = 4000
print(result, expected)
assert result == expected
print("Success!")

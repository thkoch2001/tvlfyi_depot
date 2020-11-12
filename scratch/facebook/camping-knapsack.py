from utils import get, init_table, print_table

def max_haul(capacity, items, names):
    table = init_table(rows=len(items), cols=capacity, default=0)
    items_table = init_table(rows=len(items), cols=capacity, default=[])
    for row in range(len(table)):
        for col in range(len(table[row])):
            kg, value = items[row]
            curr_capacity = col + 1

            if kg > curr_capacity:
                a = 0
            else:
                a = value + get(table, row - 1, curr_capacity - kg - 1)
            b = get(table, row - 1, col)

            if a > b:
                rest = get(items_table, row - 1, curr_capacity - kg - 1)
                knapsack = [names.get(items[row])]
                if rest:
                    knapsack += rest
            else:
                knapsack = get(items_table, row - 1, col)

            table[row][col] = max([a, b])
            items_table[row][col] = knapsack
        print_table(table)
    return items_table[-1][-1]

water = (3, 10)
book = (1, 3)
food = (2, 9)
jacket = (2, 5)
camera = (1, 6)
items = [water, book, food, jacket, camera]
result = max_haul(6, items, {
    water: 'water',
    book: 'book',
    food: 'food',
    jacket: 'jacket',
    camera: 'camera',
})
expected = ['camera', 'food', 'water']
print(result, expected)
assert result == expected
print("Success!")

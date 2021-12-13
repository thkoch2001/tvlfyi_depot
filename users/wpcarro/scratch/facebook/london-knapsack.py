from utils import get, init_table, print_table

def optimal_itinerary(duration, items):
    min_duration = min([duration for duration, _ in items])
    max_duration = max([duration for duration, _ in items])
    table = init_table(rows=len(items), cols=int(max_duration / min_duration), default=0)
    to_index = lambda duration: int(duration / min_duration) - 1
    to_duration = lambda i: i * min_duration + min_duration

    for row in range(len(table)):
        for col in range(len(table[row])):
            curr_duration = to_duration(col)
            duration, value = items[row]
            if duration > curr_duration:
                a = 0
            else:
                a = value + get(table, row - 1, to_index(curr_duration - duration))
            b = get(table, row - 1, col)
            table[row][col] = max([a, b])

        print_table(table)
    return table[-1][-1]

# You're in London for two days, and you'd like to see the following
# attractions. How can you maximize your time spent in London?
westminster = (0.5, 7)
globe_theater = (0.5, 6)
national_gallery = (1, 9)
british_museum = (2, 9)
st_pauls_cathedral = (0.5, 8)
items = [
    westminster,
    globe_theater,
    national_gallery,
    british_museum,
    st_pauls_cathedral,
]
result = optimal_itinerary(2, items)
expected = 24
print(result, expected)
assert result == expected
print("Success!")

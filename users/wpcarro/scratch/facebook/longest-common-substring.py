from utils import get, init_table, print_table

def longest_common_substring(a, b):
    """
    Computes the length of the longest string that's present in both `a` and
    `b`.
    """
    table = init_table(rows=len(b), cols=len(a), default=0)
    for row in range(len(table)):
        for col in range(len(table[row])):
            if b[row] == a[col]:
                table[row][col] = 1 + get(table, row - 1, col - 1)
    return max([max(row) for row in table])

dictionary = ["fish", "vista"]
result = [longest_common_substring("hish", x) for x in dictionary]
expected = [3, 2]
print(result, expected)
assert result == expected
print("Success!")

# Given an infinite supply of:
#   - quarters
#   - dimes
#   - nickels
#   - pennies
# Write a function to count the number of ways to make change of n.

def get(table, row, col):
    """
    Defensively get cell `row`, `col` from `table`.
    """
    if row < 0 or row >= len(table):
        return 0
    if col < 0 or col >= len(table[0]):
        return 0
    return table[row][col]

def print_table(table):
    print('\n'.join([
        ','.join([str(col) for col in table[row]])
        for row in range(len(table))]))

def init_table(rows=0, cols=0, default=0):
    result = []
    for row in range(rows):
        r = []
        for col in range(cols):
            r.append(default)
        result.append(r)
    return result

def make_change(n):
    coins = [1, 5, 10, 25]
    table = init_table(rows=len(coins), cols=n)

    for row in range(len(table)):
        for col in range(len(table[row])):
            curr_coin = coins[row]
            curr_n = col + 1
            # a
            a = get(table, row - 1, col)
            # b
            b = get(table, row, curr_n - curr_coin - 1)
            # c
            c = 1 if curr_coin <= curr_n else 0
            # commit
            if curr_coin == curr_n:
                table[row][col] = a + c
            else:
                table[row][col] = a + b * c
            # debug
            print_table(table)
            print()
    return table[-1][-1]

print(make_change(7))

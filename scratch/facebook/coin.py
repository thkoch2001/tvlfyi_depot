def init_table(rows=0, cols=0, default=None):
    table = []
    for _ in range(rows):
        row = []
        for _ in range(cols):
            row.append(default)
        table.append(row)
    return table

def print_table(table):
    result = ''
    for row in range(len(table)):
        x = ''
        for col in range(len(table[row])):
            x += str(table[row][col]) + ' '
        result += x + '\n'
    print(result)

def get(table, row, col):
    if row < 0 or col < 0:
        return 0
    else:
        return table[row][col]

def make_change(coins, amt):
    table = init_table(rows=len(coins), cols=amt, default=0)
    for row in range(len(table)):
        for col in range(len(table[row])):
            coin = coins[row]
            curr_amt = col + 1
            pull_down = get(table, row - 1, col)

            if curr_amt < coin:
                table[row][col] = pull_down
            elif curr_amt == coin:
                table[row][col] = pull_down + 1
            else:
                leftover = get(table, row, curr_amt - coin - 1)
                table[row][col] = pull_down + leftover

    print_table(table)
    return table[-1][-1]

#   1 2 3 4
# 1 1 1 1 1
# 2 1 1 2 2
# 3 1 1 3 4

result = make_change([3,2,1], 4)
print(result)

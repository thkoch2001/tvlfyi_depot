def init_table(rows=0, cols=0, default=None):
    table = []
    for row in range(rows):
        x = []
        for col in range(cols):
            x.append(default)
        table.append(x)
    return table

def get(table, row, col, default=0):
    if row < 0 or col < 0:
        return default
    return table[row][col]

def print_table(table):
    result = []
    for row in range(len(table)):
        result.append(' '.join([str(cell) for cell in table[row]]))
    print('\n'.join(result))

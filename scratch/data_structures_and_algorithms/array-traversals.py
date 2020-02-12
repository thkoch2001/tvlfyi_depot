# This is practice for various types of list traversals that turn up.

xs = range(10)
n = len(xs)

print('---')
# pythonic left-to-right traversal
result = ''
for x in xs:
    result += str(x)
print(result)

print('---')
# left-to-right traversal
result = ''
for i in range(n):
    result += str(xs[i])
print(result)

print('---')
# right-to-left traversal
result = ''
for i in range(n):
    result += str(xs[n - 1 - i])
print(result)

print('---')
# 2x left-to-right traversal
result = ''
for i in range(2 * n):
    result += str(xs[i % n])
print(result)

print('---')
# 2x right-to-left traversal
result = ''
for i in range(2 * n):
    result += str(xs[(n - 1 - i) % n])
print(result)

################################################################################
# Table traversals
################################################################################

table = [[row * 10 + i for i in range(10)] for row in range(3)]
row_ct = len(table)
col_ct = len(table[0])

print('---')
# 3x10 table traversal
result = ''
for row in table:
    r = ''
    for col in row:
        r += '{:3d}'.format(col)
    result += r + '\n'
print(result[0:-1])

print('---')
# 3x10 table traversal
result = ''
for row in range(row_ct):
    r = ''
    for col in range(col_ct):
        r += '{:3d}'.format(table[row][col])
    result += r + '\n'
print(result[0:-1])

print('---')
# 3x10 table traversal (reverse)
result = ''
for row in range(row_ct):
    r = ''
    for col in range(col_ct):
        r += '{:3d}'.format(table[row_ct - 1 - row][col_ct - 1 - col])
    result += r + '\n'
print(result)

print('---')
# 3x10 column-row traversal
result = ''
for col in range(col_ct):
    r = ''
    for row in range(row_ct):
        r += '{:3d}'.format(table[row][col])
    result += r + '\n'
print(result)

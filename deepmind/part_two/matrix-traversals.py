# Herein I'm practicing two-dimensional matrix traversals in all directions of
# which I can conceive:
# 0. T -> B; L -> R
# 1. T -> B; R -> L
# 2. B -> T; L -> R
# 3. B -> T; R -> L
#
# Commentary:
# When I think of matrices, I'm reminded of cartesian planes. I think of the
# cells as (X,Y) coordinates. This has been a pitfall for me because matrices
# are usually encoded in the opposite way. That is, to access a cell at the
# coordinates (X,Y) given a matrix M, you index M like this: M[Y][X]. To attempt
# to avoid this confusion, instead of saying X and Y, I will prefer saying
# "column" and "row".
#
# When traversing a matrix, you typically traverse vertically and then
# horizontally; in other words, the rows come first followed by the columns. As
# such, I'd like to refer to traversal orders as "top-to-bottom, left-to-right"
# rather than "left-to-right, top-to-bottom".
#
# These practices are all in an attempt to rewire my thinking.

# This is a list of matrices where the index of a matrix corresponds to the
# order in which it should be traversed to produce the sequence:
# [1,2,3,4,5,6,7,8,9].
boards = [[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[3, 2, 1], [6, 5, 4], [9, 8, 7]],
          [[7, 8, 9], [4, 5, 6], [1, 2, 3]], [[9, 8, 7], [6, 5, 4], [3, 2, 1]]]

# T -> B; L -> R
board = boards[0]
result = []
for row in board:
    for col in row:
        result.append(col)
print(result)

# T -> B; R -> L
board = boards[1]
result = []
for row in board:
    for col in reversed(row):
        result.append(col)
print(result)

# B -> T; L -> R
board = boards[2]
result = []
for row in reversed(board):
    for col in row:
        result.append(col)
print(result)

# B -> T; R -> L
board = boards[3]
result = []
for row in reversed(board):
    for col in reversed(row):
        result.append(col)
print(result)

################################################################################
# Neighbors
################################################################################

import random


# Generate a matrix of size `rows` x `cols` where each cell contains an item
# randomly selected from `xs`.
def generate_board(rows, cols, xs):
    result = []
    for _ in range(rows):
        row = []
        for _ in range(cols):
            row.append(random.choice(xs))
        result.append(row)
    return result


# Print the `board` to the screen.
def print_board(board):
    print('\n'.join([' '.join(row) for row in board]))


board = generate_board(4, 5, ['R', 'G', 'B'])
print_board(board)


# Return all of the cells horizontally and vertically accessible from a starting
# cell at `row`, `col` in `board`.
def neighbors(row, col, board):
    result = {'top': [], 'bottom': [], 'left': [], 'right': []}
    for i in range(row - 1, -1, -1):
        result['top'].append(board[i][col])
    for i in range(row + 1, len(board)):
        result['bottom'].append(board[i][col])
    for i in range(col - 1, -1, -1):
        result['left'].append(board[row][i])
    for i in range(col + 1, len(board[0])):
        result['right'].append(board[row][i])
    return result


print(neighbors(1, 2, board))

def print_board(board):
    result = []
    for row in range(8):
        r = []
        for col in range(8):
            r.append("X" if col == board[row] else "-")
        result.append(" ".join(r))
    print("\n".join(result))
    print()

def can_place(board, row, col):
    column_occupied = not any([board[i] == col for i in range(row)])

    diagonals_clear = True
    for r in range(row):
        w = abs(col - board[r])
        h = abs(r - row)
        if w == h:
            diagonals_clear = False
            break

    return all([column_occupied, diagonals_clear])

def init_board():
    board = []
    for row in range(8):
        board.append(None)
    return board

def copy_board(board):
    return board[:]

def n_queens():
    do_n_queens(init_board(), 0, 0)

def do_n_queens(board, row, col):
    if row == 8:
        print_board(board)
        return
    for i in range(col, 8):
        if can_place(board, row, i):
            copy = copy_board(board)
            copy[row] = i
            do_n_queens(copy, row + 1, 0)

n_queens()

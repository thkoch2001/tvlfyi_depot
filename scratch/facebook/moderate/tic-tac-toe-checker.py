import random

def print_board(board):
    result = []
    for row in range(len(board)):
        r = []
        for col in range(len(board[row])):
            cell = board[row][col]
            if not cell:
                r.append("-")
            else:
                r.append(cell)
        result.append(" | ".join(r))
    print("\n---------\n".join(result))

def init_board():
    result = []
    for row in range(3):
        r = []
        for col in range(3):
            r.append(None)
        result.append(r)
    return result

def check(board, player):
    print_board(board)
    print()
    if player not in "XO":
        raise Exception("Only checking the board for Xs or Os. You supplied {}".format(player))
    dn, ax, ddg, udg = "DOWN", "ACROSS", "DOWN_DIAGONAL", "UP_DIAGONAL"
    ways = [
        [[dn, ax, ddg], [dn], [dn, udg]],
        [[ax], [], []],
        [[ax], [], []],
    ]
    for row in range(len(board)):
        for col in range(len(board[row])):
            if board[row][col] == player:
                xs = ways[row][col]
                for x in xs:
                    if x == dn:
                        if {player} == {board[row+1][col], board[row+2][col]}:
                            return True
                    if x == ax:
                        if {player} == {board[row][col+1], board[row][col+2]}:
                            return True
                    if x == ddg:
                        if {player} == {board[row+1][col+1], board[row+2][col+2]}:
                            return True
                    if x == udg:
                        if {player} == {board[row+1][col-1], board[row+2][col-2]}:
                            return True
    return False

def op(player):
    return "X" if player == "O" else "O"

dn_win = lambda p: [
    [op(p), p, None],
    [op(p), p, None],
    [None,  p, None],
]

ax_win = lambda p: [
    [p, p, p],
    [op(p), op(p), None],
    [None, None, None],
]

ddg_win = lambda p: [
    [p, None, None],
    [op(p), p, None],
    [op(p), None, p],
]

udg_win = lambda p: [
    [op(p), None, p],
    [op(p), p, None],
    [p, None, None],
]

# Down
p = random.choice(["X", "O"])
assert check(dn_win(p), p) == True
assert check(dn_win(p), op(p)) == False
# Across
p = random.choice(["X", "O"])
assert check(ax_win(p), p) == True
assert check(ax_win(p), op(p)) == False
# Down Diagonally
p = random.choice(["X", "O"])
assert check(ddg_win(p), p) == True
assert check(ddg_win(p), op(p)) == False
# Down Diagonally
p = random.choice(["X", "O"])
assert check(udg_win(p), p) == True
assert check(udg_win(p), op(p)) == False
# Success
print("Tests pass!")

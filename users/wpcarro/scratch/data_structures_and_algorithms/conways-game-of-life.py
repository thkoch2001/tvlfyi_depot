from itertools import product
from random import choice
from time import sleep
from os import system
from math import floor
from colorama import Back, Fore, Style

################################################################################
# Simulation of Conway's Game of Life. The goal here was to write this with a
# small amount of code as a proof-of-concept that could be run in the terminal.
#
# If you'd like to tinker with the rules, see the conditionals defined in the
# `advance/1` function. For other parameters, like the board size and refresh
# rate, refer to the while-loop defined at the bottom of this file.
################################################################################


def init_board(n, init_alive_percentage):
    """Initialize a board of size `n` by `n`. Supply a percentage,
    `init_alive_percentage`, representing the number of cells in the board that
    should be alive from the start."""
    alive_count = floor(n * init_alive_percentage)
    distribution = [True] * alive_count + [False] * (n - alive_count)
    return [[choice(distribution) for _ in range(n)] for _ in range(n)]


def neighbors(coord, board):
    """Return the neighbors for a given `coord` on a `board`."""
    n = len(board)
    row, col = coord
    return [
        board[(row + row_d) % n][(col + col_d) % n]
        for row_d, col_d in product([-1, 0, 1], [-1, 0, 1])
        if (row_d, col_d) != (0, 0)
    ]


def advance(board):
    """Advance the state of the `board` from T[n] to T[n+1]."""
    n = len(board)
    new_board = [[False for _ in range(n)] for _ in range(n)]
    for row in range(n):
        for col in range(n):
            alive_count = len([x for x in neighbors((row, col), board) if x])
            # Loneliness
            if alive_count == 0:
                new_board[row][col] = False
            # Status Quo
            elif alive_count == 1:
                new_board[row][col] = board[row][col]
            # Cooperation
            elif alive_count == 2:
                new_board[row][col] = True
            # Resource starvation
            elif alive_count >= 3:
                new_board[row][col] = False
    return new_board


def print_board(board):
    """Print the game `board` in a human-readable way."""
    result = ''
    for row in board:
        for col in row:
            if col:
                result += Back.GREEN + '1 ' + Style.RESET_ALL
            else:
                result += Back.RED + '0 ' + Style.RESET_ALL
        result += '\n'
    print(result)


board = init_board(100, 0.50)
while True:
    system('clear')
    print_board(board)
    sleep(0.15)
    board = advance(board)

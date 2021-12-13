from collections import deque

def maybe_queue(row, col, game, q, seen):
    """
    Add coordinate, (`row`, `col`), to the queue, `q`, as long as it exists in
    the map, `game`, and it is not already present in `seen`.
    """
    if row >= 0 and row < len(game) and col >= 0 and col < len(game[0]):
        if game[row][col] == 'L' and (row, col) not in seen:
            q.append((row, col))
            seen.add((row, col))

def visit_island(row, col, game, seen):
    """
    Starting at the coordinate, (`row`, `col`), in the map, `game`, visit all
    surrounding tiles marked as land by adding them to the `seen` set.
    """
    q = deque()
    q.append((row, col))
    while q:
        row, col = q.popleft()
        maybe_queue(row - 1, col, game, q, seen) # UP
        maybe_queue(row + 1, col, game, q, seen) # DOWN
        maybe_queue(row, col - 1, game, q, seen) # LEFT
        maybe_queue(row, col + 1, game, q, seen) # RIGHT

def count_islands(game):
    """
    Return the number of contiguous land tiles in the map, `game`.
    """
    result = 0
    seen = set()
    for row in range(len(game)):
        for col in range(len(game[row])):
            if game[row][col] == 'L' and (row, col) not in seen:
                visit_island(row, col, game, seen)
                result += 1
    return result

################################################################################
# Tests
################################################################################

game = [
    "LWLWWW",
    "LLLWWW",
    "WWWLLW",
]

result = count_islands(game)
print(result)
assert result == 2
print("Success!")

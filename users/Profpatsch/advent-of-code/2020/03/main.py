import itertools
import math

def tree_line(init):
    return {
        "init-len": len(init),
        "known": '',
        "rest": itertools.repeat(init)
    }

def tree_line_at(pos, tree_line):
    needed = (pos + 1) - len(tree_line["known"])
    # internally advance the tree line to the position requested
    if needed > 0:
        tree_line["known"] = tree_line["known"] \
          + ''.join(
            itertools.islice(
                tree_line["rest"],
                1+math.floor(needed / tree_line["init-len"])))
    # print(tree_line)
    return tree_line["known"][pos] == '#'

def tree_at(linepos, pos, trees):
    return tree_line_at(pos, trees[linepos])

def slope_positions(trees, right, down):
    line = 0
    pos = 0
    while line < len(trees):
        yield (line, pos)
        line = line + down
        pos = pos + right

trees = []
with open("./input", 'r') as f:
    for line in f:
        line = line.rstrip()
        trees.append(tree_line(line))

# print(list(itertools.islice(trees[0], 5)))
# print(list(map(
#     lambda x: tree_at(0, x, trees),
#     range(100)
# )))
# print(list(slope_positions(trees, right=3, down=1)))

def count_slope_positions(trees, slope):
    count = 0
    for (line, pos) in slope:
        if tree_at(line, pos, trees):
            count = count + 1
    return count

print(
        count_slope_positions(trees, slope_positions(trees, right=1, down=1))
    *
        count_slope_positions(trees, slope_positions(trees, right=3, down=1))
    *
        count_slope_positions(trees, slope_positions(trees, right=5, down=1))
    *
        count_slope_positions(trees, slope_positions(trees, right=7, down=1))
    *
        count_slope_positions(trees, slope_positions(trees, right=1, down=2))
)

# I realized I could have just used a modulo instead …

from math import floor

# Lists
def cycle_backwards(times, xs):
    n = len(xs)
    for i in range(n * times):
        print(xs[n - 1 - i % n])

def cycle_forwards(times, xs):
    n = len(xs)
    for i in range(n * times):
        print(xs[i % n])

def backwards(xs):
    n = len(xs)
    for i in range(n):
        print(xs[n - 1 - i])

def forwards(xs):
    for i in range(len(xs)):
        print(xs[i])

xs = [2, 5, 6, 9, 12]

print("Forwards")
forwards(xs)
print("Backwards")
backwards(xs)
print("Cycle forwards")
cycle_forwards(2, xs)
print("Cycle backwards")
cycle_backwards(2, xs)

# Tables
def tblr(table):
    for row in range(len(table)):
        for col in range(len(table[row])):
            print(table[row][col])

def tbrl(table):
    for row in range(len(table)):
        n = len(table[row])
        for col in range(n):
            print(table[row][n - 1 - col])

def btlr(table):
    n = len(table)
    for row in range(n):
        for col in range(len(table[row])):
            print(table[n - 1 - row][col])

def btrl(table):
    rows = len(table)
    for row in range(rows):
        cols = len(table[row])
        for col in range(cols):
            print(table[rows - 1 - row][cols - 1 - col])

def special(table):
    rows = len(table)
    cols = len(table[0])
    for col in range(cols):
        for row in range(rows):
            print(table[row][col])

def double_bonus(table):
    rows = len(table)
    cols = len(table[0])
    for i in range(rows):
        row = i
        for col in range(cols):
            print(table[row][col % cols])
            row = (row + 1) % rows

def free(table):
    rows = len(table)
    cols = len(table[0])
    d = rows * cols
    for i in range(d):
        row = floor((i % d) / cols)
        col = i % cols
        print(table[row][col])

table = [[1,2,3,4],
         [5,6,7,8]]

print("Top->Bottom, Left->Right")
tblr(table)
print("Top->Bottom, Right->Left")
tbrl(table)
print("Bottom->Top, Left->Right")
btlr(table)
print("Bottom->Top, Right->Left")
btrl(table)
print("Special")
special(table)
print("2x Bonus")
double_bonus(table)
print("Free")
free(table)

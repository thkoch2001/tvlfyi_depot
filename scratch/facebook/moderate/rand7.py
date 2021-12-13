# Define a function, rand7, that generates a random number [0,7), using only
# rand5, which generates a random number [0,5).

import random
from collections import Counter

# Returns [0,4]
def rand5():
    return random.randint(0,4)

# Return [0,6]
def rand7_a():
    return sum(rand5() for _ in range(7)) % 7

# Return [0,6]
def rand7_b():
    x = 5 * rand5() + rand5()
    if x < 21:
        return x % 7
    return rand7_b()

c = Counter([rand7_a() for _ in range(100000)])
print(c)
c = Counter([rand7_b() for _ in range(100000)])
print(c)

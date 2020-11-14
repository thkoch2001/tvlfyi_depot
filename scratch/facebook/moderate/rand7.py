# Define a function, rand7, that generates a random number [0,7), using only
# rand5, which generates a random number [0,5).

import random
from collections import Counter

# Returns [0,4]
def rand5():
    return random.randint(0,4)

def rand4():
    x = rand7()
    if x >= 4:
        return rand4()
    return x

# Return [0,6]
def rand7():
    return sum(rand5() for _ in range(7)) % 7

c = Counter([rand4() for _ in range(100000)])
print(c)

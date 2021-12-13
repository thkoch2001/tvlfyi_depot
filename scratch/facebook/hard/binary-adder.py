import random

def add(a, b):
    """
    Return the sum of `a` and `b`.
    """
    if b == 0:
        return a
    sum = a ^ b
    carry = (a & b) << 1
    return add(sum, carry)

################################################################################
# Tests
################################################################################

for _ in range(10):
    x, y = random.randint(0, 100), random.randint(0, 100)
    print("{} + {} = {} == {}".format(x, y, x + y, add(x, y)))
    assert add(x, y) == x + y
    print("Pass!")
print("Success!")

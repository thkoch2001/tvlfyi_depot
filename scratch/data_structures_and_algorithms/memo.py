import time
import random

memo = {}


def f(x):
    if x in memo:
        print("Hit.\t\tf({})".format(x))
        return memo[x]
    else:
        print("Computing...\tf({})".format(x))
        time.sleep(0.25)
        res = random.randint(0, 10)
        memo[x] = res
        return res


[f(random.randint(0, 10)) for _ in range(10)]

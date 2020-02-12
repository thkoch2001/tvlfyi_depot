from random import choice
from math import floor

# Applying Chapter 1 from "Algorithms to Live By", which describes optimal
# stopping problems. Technically this simulation is invalid because the
# `candidates` function takes a lower bound and an upper bound, which allows us
# to know the cardinal number of an individual candidates. The "look then leap"
# algorithm is ideal for no-information games - i.e. games when upper and lower
# bounds aren't known. The `look_then_leap/1` function is ignorant of this
# information, so it behaves as if in a no-information game. Strangely enough,
# this algorithm will pick the best candidate 37% of the time.
#
# Chapter 1 describes two algorithms:
# 1. Look-then-leap: ordinal numbers - i.e. no-information games. Look-then-leap
#    finds the best candidate 37% of the time.
# 2. Threshold: cardinal numbers - i.e. where upper and lower bounds are
#    known. The Threshold algorithm finds the best candidate ~55% of the time.
#
# All of this and more can be studied as "optimal stopping theory". This applies
# to finding a spouse, parking a car, picking an apartment in a city, and more.


# candidates :: Int -> Int -> Int -> [Int]
def candidates(lb, ub, ct):
    xs = list(range(lb, ub + 1))
    return [choice(xs) for _ in range(ct)]


# look_then_leap :: [Integer] -> Integer
def look_then_leap(candidates):
    best = candidates[0]
    seen_ct = 1
    ignore_ct = floor(len(candidates) * 0.37)
    for x in candidates[1:]:
        if ignore_ct > 0:
            ignore_ct -= 1
            best = max(best, x)
        else:
            if x > best:
                print('Choosing the {} candidate.'.format(seen_ct))
                return x
        seen_ct += 1
    print('You may have waited too long.')
    return candidates[-1]


candidates = candidates(1, 100, 100)
print(candidates)
print(look_then_leap(candidates))

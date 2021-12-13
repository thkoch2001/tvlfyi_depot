# This is a scratch pad for randomly selected questions

# def char_and_rest(i, xs):
#     return xs[i], xs[:i] + xs[i+1:]

# def perms(xs):
#     if len(xs) == 1:
#         return [xs]
#     result = []
#     for i in range(len(xs)):
#         c, rest = char_and_rest(i, xs)
#         for perm in perms(rest):
#             result.append(c + ''.join(perm))
#     return result

# print(perms(list("woah")))

# def f(take_out, dine_in, served):
#     j, k = 0, 0
#     for i in range(len(served)):
#         if j < len(take_out) and served[i] == take_out[j]:
#             j += 1
#         elif k < len(dine_in) and served[i] == dine_in[k]:
#             k += 1
#         else:
#             return False
#     if j < len(take_out) or k < len(dine_in):
#         return False
#     return True

# take_out = [17, 8, 24]
# dine_in = [12, 19, 2]
# served = [17, 8, 12, 19, 24, 2]
# print(f(take_out, dine_in, served))

# def match(a, b):
#     if a == '{':
#         return b == '}'
#     if a == '[':
#         return b == ']'
#     if a == '(':
#         return b == ')'
#     return False

# def f(xs):
#     s = []
#     for c in xs:
#         if c in {'{', '[', '('}:
#             s.append(c)
#         elif c in {'}', ']', ')'}:
#             opener = s.pop()
#             if not match(opener, c):
#                 return False
#     return len(s) == 0

# assert f("{[]()}")
# assert f("{[(])}") == False
# assert f("{[}") == False
# print("Success!")

# def valid_bst(node):
#     lhs = max_bst_value(node.left) if node.left else float('-inf')
#     rhs = min_bst_value(node.right) if node.right else float('inf')

#     return and([
#         lhs <= node.value,
#         rhs > node.value,
#         valid_bst(node.left),
#         valid_bst(node.right),
#     ])

import random
import math

def shuffle(xs):
    n = len(xs)
    for i in range(n - 1):
        j = random.randint(i + 1, n - 1)
        xs[i], xs[j] = xs[j], xs[i]
    return xs

def as_card(i):
    if i not in range(1, 53):
        raise Exception("Not a card")
    # 1
    suit = ['Hearts', 'Clubs', 'Diamonds', 'Spades'][math.floor((i - 1) / 13)]
    n = ['Ace',2,3,4,5,6,7,8,9,10,'Jack','Queen','King'][(i - 1) % 13]
    return '{} of {}'.format(n, suit)

xs = list(range(1, 53))
print(xs)
shuffle(xs)
for x in xs:
    print(as_card(x))

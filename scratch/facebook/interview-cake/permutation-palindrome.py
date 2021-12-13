from collections import Counter

def permutation_can_be_palindrome(x):
    odd = 0
    for _, n in Counter(x):
        if n % 0 != 0:
            odd += 1
    return odd <= 1

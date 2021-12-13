def valid_parens(n):
    if n == 0:
        return []
    if n == 1:
        return ["()"]

    result = set()
    for x in valid_parens(n - 1):
        result.add("({})".format(x))
        result.add("(){}".format(x))
        result.add("{}()".format(x))
    return result

def valid_parens_efficient(n):
    result = []
    curr = [''] * n**2
    do_valid_parens_efficient(result, curr, 0, n, n)
    return result

def do_valid_parens_efficient(result, curr, i, lhs, rhs):
    if lhs == 0 and rhs == 0:
        result.append(''.join(curr))
    else:
        if lhs > 0:
            curr[i] = '('
            do_valid_parens_efficient(result, curr, i + 1, lhs - 1, rhs)
        if rhs > lhs:
            curr[i] = ')'
            do_valid_parens_efficient(result, curr, i + 1, lhs, rhs - 1)

# Avoids recursion by using either a stack or a queue. I think this version is
# easier to understand.
def valid_parens_efficient_2(n):
    result = []
    xs = []
    xs.append(('', n, n))
    while xs:
        curr, lhs, rhs = xs.pop()
        print(curr)
        if lhs == 0 and rhs == 0:
            result.append(''.join(curr))
        if lhs > 0:
            xs.append((curr + '(', lhs - 1, rhs))
        if rhs > lhs:
            xs.append((curr + ')', lhs, rhs - 1))
    return result

# print(valid_parens(4))
print(valid_parens_efficient(3))
print(valid_parens_efficient_2(3))

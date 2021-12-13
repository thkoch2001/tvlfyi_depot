








def tokenize(xs):
    result = []
    i = 0
    while i < len(xs):
        curr = xs[i]
        if curr in {'*','+'}:
            result.append(curr)
            i += 1
            continue
        i += 1
        while i < len(xs) and xs[i] in {str(x) for x in range(10)}:
            curr += xs[i]
            i += 1
        result.append(int(curr))
    return result

def parse(tokens):
    result = []
    series = []
    for token in tokens:
        if token == '*':
            continue
        elif token == '+':
            result.append(series)
            series = []
        else:
            series.append(token)
    if series:
        result.append(series)
    return result

def product(xs):
    result = 1
    for x in xs:
        result *= x
    return result

def evaluate(tree):
    return sum([product(xs) for xs in tree])

print(evaluate(parse(tokenize("2+30*8*9+10"))))

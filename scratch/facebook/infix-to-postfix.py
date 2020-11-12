operators = {
    '*': 1,
    '+': 0,
}

def tokenize(xs):
    result = []
    i = 0
    while i < len(xs):
        current = xs[i]
        if current in operators.keys():
            result.append(current)
            i += 1
            continue
        else:
            i += 1
            while i < len(xs) and xs[i] in {str(n) for n in range(10)}:
                current += xs[i]
                i += 1
            result.append(int(current))
    return result

def postfix(xs):
    result = []
    s = []
    for x in xs:
        if x in operators.keys():
            while s and operators[s[-1]] >= operators[x]:
                result.append(s.pop())
            s.append(x)
        else:
            result.append(x)
    while s:
        result.append(s.pop())
    return result

def evaluate(xs):
    s = []
    for x in xs:
        print(s, x)
        if x == '*':
            s.append(s.pop() * s.pop())
        elif x == '+':
            s.append(s.pop() + s.pop())
        else:
            s.append(x)
        print(s)
    return s[-1]


print(evaluate(postfix(tokenize("12+3*10"))))

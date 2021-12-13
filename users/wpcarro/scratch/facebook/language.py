import random

# Write an evaluator for a small language:
#   - operators: '+', '*'
#   - operands:  Integers
#
# E.g. evaluate("2+14*90+5*16")

def tokenize(xs):
    result = []
    i = 0
    while i < len(xs):
        current = xs[i]
        if current in {'*', '+'}:
            result.append(current)
            i += 1
            continue
        elif current == ' ':
            i += 1
            continue
        else:
            i += 1
            while i < len(xs) and xs[i] in {str(x) for x in range(10)}:
                current += xs[i]
                i += 1
            result.append(int(current))
    return result

def ast(tokens):
    result = []
    series = []
    for token in tokens:
        if token == '+':
            result.append(series)
            series = []
        elif token == '*':
            continue
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

def evaluate(x):
    tokens = tokenize(x)
    tree = ast(tokens)
    return sum([product(xs) for xs in tree])

n = 7
operands = [random.randint(0, 100) for _ in range(n)]
operators = [random.choice(['+','*']) for _ in range(n - 1)]
expr = []
for i in range(n - 1):
    expr.append(operands[i])
    expr.append(operators[i])
expr.append(operands[-1])

expr = ' '.join([str(x) for x in expr])
print("Expression: {}".format(expr))
print("Tokens: {}".format(tokenize(expr)))
print("AST: {}".format(ast(tokenize(expr))))
print("Answer: {}".format(evaluate(expr)))
assert evaluate(expr) == eval(expr)
print("Success!")

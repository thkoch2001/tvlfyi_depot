# After stumbling through my first technical screen, I'm going to drill
# algorithms for implementing evaluators for a toy expression language:
# e.g. 2 + 13 * 3 + 5 * 2
#
# As of now, I'm aware of a few algorithms for solving this:
#   - DONE: Convert infix expression to Polish notation and evaluate the Polish
#     notation.
#   - DONE: Evaluate the tokens using two stacks and avoid converting it.
#   - DONE: Create a tree of depth two to encode the operator precedence and
#     evaluate that AST.
#   - TODO: Convert the infix expression to a prefix expression
#   - TODO: Write a recursive descent parser and evaluate the AST.

operators = {
    '*': 1,
    '+': 0,
}

def tokenize(xs):
    result = []
    i = 0
    while i < len(xs):
        current = xs[i]
        if current == ' ':
            i += 1
            continue
        elif current in operators.keys():
            result.append(current)
            i += 1
        else:
            i += 1
            while i < len(xs) and xs[i] in {str(n) for n in range(10)}:
                current += xs[i]
                i += 1
            result.append(int(current))
    return result

# Convert infix to postfix; evaluate postfix
# I believe this is known as the Shunting-Yards algorithm
def postfix(tokens):
    result = []
    s = []
    for token in tokens:
        if type(token) == int:
            result.append(token)
        else:
            while s and operators[token] < operators[s[-1]]:
                result.append(s.pop())
            s.append(token)
    while s:
        result.append(s.pop())
    return result

def do_evaluate_with_polish_notation(tokens):
    s = []
    for token in tokens:
        if token == '*':
            s.append(s.pop() * s.pop())
        elif token == '+':
            s.append(s.pop() + s.pop())
        else:
            s.append(token)
    return s[-1]

def evaluate_with_polish_notation(expr):
    tokens = tokenize(expr)
    print("Tokens:  {}".format(tokens))
    pn = postfix(tokens)
    print("Postfix: {}".format(pn))
    result = do_evaluate_with_polish_notation(pn)
    print("Result:  {}".format(result))
    return result

# Evaluate Tokens

def apply_operator(op, a, b):
    if op == '*':
        return a * b
    elif op == '+':
        return a + b

def do_evaluate_tokens(tokens):
    vals = []
    ops = []
    for token in tokens:
        if type(token) == int:
            vals.append(token)
        elif token == '*':
            ops.append(token)
        elif token == '+':
            while ops and operators[token] < operators[ops[-1]]:
                vals.append(apply_operator(ops.pop(), vals.pop(), vals.pop()))
            ops.append(token)
        else:
            raise Exception("Unexpected token: {}".format(token))
    while ops:
        vals.append(apply_operator(ops.pop(), vals.pop(), vals.pop()))
    return vals[-1]

def evaluate_tokens(expr):
    tokens = tokenize(expr)
    print("Tokens:  {}".format(tokens))
    result = do_evaluate_tokens(tokens)
    print("Result:  {}".format(result))
    return result

# Ad Hoc Tree

def parse(tokens):
    result = []
    series = []
    for token in tokens:
        if type(token) == int:
            series.append(token)
        elif token == '*':
            continue
        elif token == '+':
            result.append(series)
            series = []
        else:
            raise Exception("Unexpected token: {}".format(token))
    result.append(series)
    return result

def product(xs):
    result = 1
    for x in xs:
        result *= x
    return result

def do_evaluate_ad_hoc_tree(ast):
    return sum([product(xs) for xs in ast])

def evaluate_ad_hoc_tree(expr):
    tokens = tokenize(expr)
    print("Tokens:  {}".format(tokens))
    ast = parse(tokens)
    print("AST:     {}".format(ast))
    result = do_evaluate_ad_hoc_tree(ast)
    print("Result:  {}".format(result))
    return result

# Recursive Descent Parser

# expression     -> addition ;
# addition       -> multiplication ( "+" multiplication )* ;
# multiplication -> terminal ( "*" terminal )* ;
# terminal       -> NUMBER ;

class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0

    # mutations
    def advance(self):
        self.i += 1

    def consume(self):
        result = self.curr()
        self.advance()
        return result

    # predicates
    def match(self, x):
        if self.curr() == x:
            self.advance()
            return True
        return False

    def tokens_available(self):
        return self.i < len(self.tokens)

    # getters
    def prev(self):
        return self.tokens[self.i - 1]

    def curr(self):
        return self.tokens[self.i] if self.tokens_available() else None

    def next(self):
        return self.tokens[self.i + 1]

def parse_expression(tokens):
    parser = Parser(tokens)
    return parse_addition(parser)

def parse_addition(parser):
    result = parse_multiplication(parser)
    while parser.match("+"):
        op = parser.prev()
        rhs = parse_multiplication(parser)
        result = ["+", result, rhs]
    return result

def parse_multiplication(parser):
    result = parse_terminal(parser)
    while parser.match("*"):
        op = parser.prev()
        rhs = parse_terminal(parser)
        result = ["*", result, rhs]
    return result

def parse_terminal(parser):
    # If we reach here, the current token *must* be a number.
    return parser.consume()

def evaluate_ast(ast):
    if type(ast) == int:
        return ast
    else:
        op, lhs, rhs = ast[0], ast[1], ast[2]
        return apply_operator(op, evaluate_ast(lhs), evaluate_ast(rhs))

def evaluate_recursive_descent(expr):
    tokens = tokenize(expr)
    print("Tokens:  {}".format(tokens))
    ast = parse_expression(tokens)
    print("AST:     {}".format(ast))
    result = evaluate_ast(ast)
    return result

methods = {
    'Polish Notation': evaluate_with_polish_notation,
    'Evaluate Tokens': evaluate_tokens,
    'Ad Hoc Tree': evaluate_ad_hoc_tree,
    'Recursive Descent': evaluate_recursive_descent,
}

for name, fn in methods.items():
    expr = "13 + 2 * 4 + 7 + 3 * 8"
    print("Evaluating \"{}\" using the \"{}\" method...".format(expr, name))
    assert fn(expr) == eval(expr)
    print("Success!")

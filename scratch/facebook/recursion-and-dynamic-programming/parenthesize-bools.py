# BNF
# expression -> bool ( ( '|' | '&' | '^' ) bool )*
# bool       -> '0' | '1'

def tokenize(xs):
    result = []
    for c in xs:
        if c == '0':
            result.append(0)
        elif c == '1':
            result.append(1)
        elif c in "&|^":
            result.append(c)
        else:
            raise Exception("Unexpected token, \"{}\"".format(c))
    return result

class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0

    def prev(self):
        return self.tokens[self.i - 1]

    def curr(self):
        return self.tokens[self.i]

    def match(self, xs):
        if self.exhausted():
            return False
        if (self.curr() in xs):
            self.consume()
            return True
        return False

    def consume(self):
        result = self.curr()
        self.i += 1
        return result

    def exhausted(self):
        return self.i >= len(self.tokens)

def recursive_descent(tokens):
    parser = Parser(tokens)
    return parse_expression(parser)

def parse_expression(parser):
    lhs = parse_bool(parser)
    while parser.match(['|', '&', '^']):
        op = parser.prev()
        rhs = parse_expression(parser)
        lhs = [op, lhs, rhs]
    return lhs

def parse_bool(parser):
    if parser.curr() == 0:
        parser.consume()
        return False
    elif parser.curr() == 1:
        parser.consume()
        return True
    else:
        raise Exception("Unexpected token: {}".format(parser.curr()))

def f(expr, result):
    tokens = tokenize(expr)
    tree = recursive_descent(tokens)
    return do_f(tree, result)

def do_f(tree, result):
    if type(tree) == bool:
        if tree == result:
            return 1
        else:
            return 0

    op, lhs, rhs = tree[0], tree[1], tree[2]
    truth_tables = {
        True: {
            '|': [
                (True, True),
                (True, False),
                (False, True),
            ],
            '&': [
                (True, True),
            ],
            '^': [
                (True, False),
                (False, True),
            ],
        },
        False: {
            '|': [
                (False, False),
            ],
            '&': [
                (False, False),
                (True, False),
                (False, True),
            ],
            '^': [
                (True, True),
                (False, False),
            ],
        }
    }

    return sum([do_f(lhs, x) * do_f(rhs, y) for x, y in truth_tables[result][op]])

print(f("1^0|0|1", False))
print(f("1|0|1|1", False))

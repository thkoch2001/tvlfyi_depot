# Writing a small proof-of-concept...
#   - lexer
#   - parser
#   - compiler
# ...for regex.
#
# BNF
# expression -> ( char_class | CHAR ) quantifier? ( "|" expression )*
# char_class -> "[" CHAR+ "]"
# quantifier -> "?" | "*" | "+" | "{" INT? "," INT? "}"
#
# Of the numerous things I do not support, here are a few items of which I'm
# aware:
#   - alternatives:   (a|b)
#   - capture groups: (ab)cd

from parser import Parser
import string

################################################################################
# Top-Level API
################################################################################

def tokenize(xs):
    """
    Transform `xs` into a list of tokens.

    Also: expand shorthand symbols using the following table:
      - ? -> {0,1}
      - * -> {0,}
      - + -> {1,}
    """
    result = []
    i = 0
    shorthand = {
        "?": ["{", 0, ",", 1, "}"],
        "*": ["{", 0, ",", "}"],
        "+": ["{", 1, ",", "}"],
    }
    while i < len(xs):
        if xs[i] in shorthand:
            for c in shorthand[xs[i]]:
                result.append(c)
            i += 1
        elif xs[i] == "{":
            result.append(xs[i])
            i += 1
            curr = ""
            while xs[i] in string.digits:
                curr += xs[i]
                i += 1
            result.append(int(curr))
            assert xs[i] == ","
            result.append(",")
            i += 1
            curr = ""
            while xs[i] in string.digits:
                curr += xs[i]
                i += 1
            result.append(int(curr))
        else:
            result.append(xs[i])
            i += 1
    return result

def parse(expr):
    """
    Tokenize `expr` and convert it into a parse-tree.
    """
    tokens = tokenize(expr)
    return parse_tokens(tokens)

def compile(xs):
    """
    Transform `xs`, a parse-tree representing a regex, into a function that
    accepts a string, and returns the substring that the regex matches.
    """
    def fn(input):
        match = ""
        i = 0
        for x in xs:
            matches, q = x[1], x[2]
            lo, hi = q[1], q[2]
            for j in range(lo):
                if i < len(input) and input[i] in matches:
                    match += input[i]
                    i += 1
                else:
                    print("Failed to match {} with {}".format(input[i], matches))
                    return None
            if hi == float('inf'):
                while i < len(input) and input[i] in matches:
                    match += input[i]
                    i += 1
            else:
                for j in range(hi - lo):
                    if i < len(input) and input[i] in matches:
                        match += input[i]
                        i += 1
        return match
    return fn

################################################################################
# Helper Functions
################################################################################

def parse_tokens(tokens):
    result = []
    parser = Parser(tokens)
    while not parser.exhausted():
        result.append(parse_expression(parser))
    return result

def parse_expression(parser):
    if parser.curr() == "[":
        return parse_character_class(parser)
    else:
        return parse_character(parser)

def parse_character_class(parser):
    parser.expect("[")
    beg = parser.consume()
    parser.expect("-")
    end = parser.consume()
    parser.expect("]")
    if parser.curr() == "{":
        q = parse_quantifier(parser)
    return char_class(xs=expand_range(beg, end), q=q)

def parse_quantifier(parser):
    parser.expect("{")
    if parser.match([","]):
        end = parser.consume()
        parser.expect("}")
        return quantifier(beg=0, end=end)
    else:
        beg = parser.consume()
        parser.expect(",")
        if parser.match(["}"]):
            return quantifier(beg=beg)
        else:
            end = parser.consume()
            parser.expect("}")
            return quantifier(beg=beg, end=end)

def parse_character(parser):
    c = parser.consume()
    q = None
    if parser.curr() == "{":
        q = parse_quantifier(parser)
    return char_class(xs={c}, q=q)

def char_class(xs=set(), q=None):
    if not q:
        q = quantifier(beg=1, end=1)
    return ["CHARACTER_CLASS", xs, q]

def expand_range(beg, end):
    # TODO: Implement this
    return {string.printable[i]
            for i in range(string.printable.index(beg),
                           string.printable.index(end) + 1)}

def quantifier(beg=0, end=float('inf')):
    return ['QUANTIFIER', beg, end]

################################################################################
# Tests
################################################################################

xs = [
    ("[a-c]*[0-9]{2,3}", ["dog"]),
    ("ca+t?", ["cat", "caaaat", "ca", "dog"]),
]

for re, inputs in xs:
    print("Regex:  {}".format(re))
    print("Tokens: {}".format(tokenize(re)))
    print("Parsed: {}".format(parse(re)))
    print("\nTESTS")
    for input in inputs:
        print("Attempting to match \"{}\"...".format(input))
        parser = compile(parse(re))
        print("Result: \"{}\"\n".format(parser(input)))
